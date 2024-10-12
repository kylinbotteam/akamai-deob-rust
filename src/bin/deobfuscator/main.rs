use swc_core::common::{FileName, Globals, GLOBALS, Mark, SourceMap, Span, DUMMY_SP};
use swc_core::common::input::StringInput;
use swc_core::common::sync::Lrc;
use swc_core::ecma::codegen::Emitter;
use swc_core::ecma::codegen::text_writer::JsWriter;
use swc_core::ecma::parser::{EsSyntax, Parser, Syntax};
use swc_core::ecma::transforms::optimization::simplify::{expr_simplifier, dead_branch_remover, const_propagation, inlining};
use swc_core::ecma::visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use swc_core::common::pass::Repeated;
use swc_core::ecma::ast::*;
use swc_core::ecma::ast::VarDeclKind;

use std::collections::HashMap;
use std::fs;
use std::env;

use akamai_deob_rust::transformers::inline_lazy_initializer;

struct Deobfuscator;

impl VisitMut for Deobfuscator {
    fn visit_mut_ident(&mut self, ident: &mut Ident) {
        // Simple logic to rename obfuscated variable names
        if ident.sym.starts_with('_') {
            ident.sym = format!("var_{}", &ident.sym[1..]).into();
        }
    }

    fn visit_mut_var_declarator(&mut self, var: &mut VarDeclarator) {
        // You can add more transformations for variable declarations here
        var.visit_mut_children_with(self);
    }

    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        // Simplify complex expressions here
        expr.visit_mut_children_with(self);
    }
}


struct ConstantFolder {
    // Track constants and their values for propagation
    consts: HashMap<String, Expr>,
}

impl ConstantFolder {
    fn new() -> Self {
        ConstantFolder {
            consts: HashMap::new(),
        }
    }

    fn eval_const_expr(&self, expr: &Expr) -> Option<Expr> {
        match expr {
            // Handle numeric constants
            Expr::Bin(BinExpr { left, op, right, .. }) => {
                if let (Expr::Lit(Lit::Num(left_num)), Expr::Lit(Lit::Num(right_num))) =
                    (&**left, &**right)
                {
                    let result = match op {
                        BinaryOp::Add => left_num.value + right_num.value,
                        BinaryOp::Sub => left_num.value - right_num.value,
                        BinaryOp::Mul => left_num.value * right_num.value,
                        BinaryOp::Div => left_num.value / right_num.value,
                        _ => return None,
                    };
                    return Some(Expr::Lit(Lit::Num(Number {
                        span: DUMMY_SP,
                        value: result,
                        raw: None,
                    })));
                } else if let (Expr::Lit(Lit::Str(left_str)), Expr::Lit(Lit::Str(right_str))) =
                (&**left, &**right) {
                    // Handle string concatenation
                    if *op == BinaryOp::Add {
                        let result = format!("{}{}", left_str.value, right_str.value);
                        return Some(Expr::Lit(Lit::Str(Str {
                            span: DUMMY_SP,
                            value: result.into(),
                            raw: None,
                        })));
                    } else {
                        return None
                    }
                } else {
                    return None
                }
            }
            _ => None,
        }
    }
}

impl VisitMut for ConstantFolder {
    fn visit_mut_var_declarator(&mut self, declarator: &mut VarDeclarator) {
        if let Some(init) = &declarator.init {
            if let Expr::Lit(_) = **init {
                // If this is a constant, store it for propagation
                if let Pat::Ident(ident) = &declarator.name {
                    self.consts
                        .insert(ident.id.sym.to_string(), *init.clone());
                }
            } else if let Some(folded) = self.eval_const_expr(init) {
                // If it's a foldable expression, replace it
                declarator.init = Some(Box::new(folded));
            }
        }
        declarator.visit_mut_children_with(self);
    }

    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        // Replace variable references with their constant values
        if let Expr::Ident(ident) = expr {
            if let Some(const_value) = self.consts.get(&ident.sym.to_string()) {
                *expr = const_value.clone();
            }
        } else {
            expr.visit_mut_children_with(self);
        }
    }
}

#[derive(Debug, Clone)]
struct Binding {
    name: String,
    kind: BindingKind,
    reference_paths: Vec<Span>,
}


impl Binding {
    fn new(name: String, kind: BindingKind) -> Self {
        Binding {
            name,
            kind,
            reference_paths: vec![],
        }
    }

    fn add_reference(&mut self, span: Span) {
        self.reference_paths.push(span);
    }
}

#[derive(Debug, Clone)]
enum BindingKind {
    Var,
    Let,
    Const,
    Function,
}

#[derive(Debug, Clone)]
struct Scope {
    bindings: HashMap<String, Binding>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            bindings: HashMap::new(),
        }
    }

    fn add_binding(&mut self, name: String, binding: Binding) {
        self.bindings.insert(name, binding);
    }

    fn find_binding_mut(&mut self, name: &str) -> Option<&mut Binding> {
        self.bindings.get_mut(name)
    }
}

#[derive(Debug)]
struct ScopeTracker {
    scopes: Vec<Scope>,
}

impl ScopeTracker {
    fn new() -> Self {
        ScopeTracker {
            scopes: vec![Scope::new()], // Start with a global scope
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_binding(&mut self, name: String, binding: Binding) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.add_binding(name, binding);
        }
    }

    fn find_binding_mut(&mut self, name: &str) -> Option<&mut Binding> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.find_binding_mut(name) {
                return Some(binding);
            }
        }
        None
    }
}
struct VarFinder {
    tracker: ScopeTracker,
}

impl<'a> Visit for VarFinder {
    fn visit_var_decl(&mut self, var_decl: &VarDecl) {
        for decl in &var_decl.decls {
            if let Pat::Ident(ident) = &decl.name {
                let binding = Binding::new(
                    ident.id.sym.to_string(),
                    match var_decl.kind {
                        VarDeclKind::Var => BindingKind::Var,
                        VarDeclKind::Let => BindingKind::Let,
                        VarDeclKind::Const => BindingKind::Const,
                    },
                );
                self.tracker.add_binding(ident.id.sym.to_string(), binding);
            }
        }
        var_decl.visit_children_with(self);
    }

    fn visit_fn_decl(&mut self, fn_decl: &FnDecl) {
        self.tracker.add_binding(
            fn_decl.ident.sym.to_string(),
            Binding::new(fn_decl.ident.sym.to_string(), BindingKind::Function),
        );

        self.tracker.enter_scope();

        for param in &fn_decl.function.params {
            if let Pat::Ident(ident) = &param.pat {
                self.tracker.add_binding(
                    ident.id.sym.to_string(),
                    Binding::new(ident.id.sym.to_string(), BindingKind::Var),
                );
            }
        }

        fn_decl.function.body.visit_with(self);

        self.tracker.exit_scope();
    }

    fn visit_block_stmt(&mut self, block: &BlockStmt) {
        self.tracker.enter_scope();
        block.visit_children_with(self);
        self.tracker.exit_scope();
    }

    fn visit_ident(&mut self, ident: &Ident) {
        if let Some(binding) = self.tracker.find_binding_mut(&ident.sym.to_string()) {
            binding.add_reference(ident.span);
            println!(
                "Found reference to '{}': span={:?}",
                ident.sym, ident.span
            );
        } else {
            println!("No binding found for '{}'", ident.sym);
        }
    }
}

fn main() {
    let default_input = &String::from("test.js");
    let default_output = &String::from("test-out.js");

    let args: Vec<String> = env::args().collect();
    let filename = args.get(1).unwrap_or(default_input);
    let output_filename = args.get(2).unwrap_or(default_output);

    let buf = fs::read(filename).unwrap();
    let str = std::str::from_utf8(&buf).unwrap();

    let globals = Globals::new();
    GLOBALS.set(&globals, || {
        // Setup SWC
        let cm: Lrc<SourceMap> = Default::default();
        let fm = cm.new_source_file(
            FileName::Custom("input.js".into()).into(),
            str.into(),
        );
        // Parse JavaScript code
        let mut parser = Parser::new(
            Syntax::Es(EsSyntax::default()),
            StringInput::from(&*fm),
            None,
        );
        let mut program = parser.parse_program().expect("parse_program failed");

        let mut mark = Mark::new();
        // Apply expr_simplifier
        let mut simplifier = expr_simplifier(mark, Default::default());
        let mut dce = dead_branch_remover(mark);
        let mut cp = const_propagation::constant_propagation();
        loop {
            program.visit_mut_with(&mut simplifier);
            program.visit_mut_with(&mut cp);
            // program.visit_mut_with(&mut dce);

            if !simplifier.changed() && !dce.changed() {
                break;
            }
            simplifier.reset();
            dce.reset();
        }

        let mut lazy_initializer_inliner = inline_lazy_initializer::inline_lazy_initializer();
        program.visit_mut_with(&mut lazy_initializer_inliner);

        // Generate new code from the modified AST
        let mut buf = Vec::new();
        let writer = Box::new(JsWriter::new(cm.clone(), "\n", &mut buf, None));
        let mut emitter = Emitter {
            cfg: Default::default(),
            cm: cm.clone(),
            comments: None,
            wr: writer,
        };
        emitter.emit_program(&program).expect("emit_script failed");
        let output_code = String::from_utf8(buf).expect("String::from_utf8 failed");

        std::fs::write(output_filename, output_code).expect("writing output failed");
    });
}
