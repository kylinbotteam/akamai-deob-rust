#![feature(core_intrinsics)]

use akamai_deob_rust::transformers::inline_window_var;
use akamai_deob_rust::util::syntax_context;
use akamai_deob_rust::vm;
use akamai_deob_rust::vm::extractor::IVmScriptExtractor;
use swc_core::common::{FileName, Globals, GLOBALS, Mark, SourceMap, Span, DUMMY_SP, SourceMapper};
use swc_core::common::input::StringInput;
use swc_core::common::sync::Lrc;
use swc_core::ecma::codegen::Emitter;
use swc_core::ecma::codegen::text_writer::JsWriter;
use swc_core::ecma::parser::{EsSyntax, Parser, Syntax};
use swc_core::ecma::transforms::base::fixer::fixer;
use swc_core::ecma::transforms::optimization::simplify::{expr_simplifier, dead_branch_remover, const_propagation, inlining};
use swc_core::ecma::visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use swc_core::common::pass::Repeated;
use swc_core::ecma::ast::*;
use swc_core::ecma::ast::VarDeclKind;
use swc_core::ecma::transforms::base::{fixer, resolver};

use std::collections::HashMap;
use std::fs;
use std::env;

use akamai_deob_rust::transformers::{inline_lazy_initializer, inline_ops_fns};
use akamai_deob_rust::transformers::anti_tempering::{self as anti_tempering_key, IAntiTempering};
use akamai_deob_rust::deobfuscator::anti_tempering;

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

fn deobfuscate<'a>( program: &'a mut Program, anti_tempering: Option<(&str, &str)>, vm: bool) -> &'a mut Program {
    let mut mark = Mark::new();

    let mut anti_tempering_key_value = 0;
    if let Some(at) = anti_tempering {
        let mut anti_tempering_key_extractor = anti_tempering_key::extract_anti_tempering_key();
        program.visit_mut_with(&mut anti_tempering_key_extractor);

        let key_str = anti_tempering_key_extractor.get_anti_tempering_key_str();
        let key_nonce = anti_tempering_key_extractor.get_anti_tempering_key_nonce();
        if key_str.is_none() {
            return program;
        } else {
            anti_tempering_key_value = anti_tempering_key::generate_value(at.0, at.1, key_str.as_ref().unwrap().as_str(), key_nonce);
            std::print!("Found antitempering key: {}, {} => {}\n", key_str.as_ref().unwrap().as_str(), key_nonce, anti_tempering_key_value);
        }
    }

    let mut resolver_pass = resolver(mark, mark, false);
    program.visit_mut_with(&mut resolver_pass);

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

    let mut ops_fns_inliner = inline_ops_fns::inline_ops_fns();
    program.visit_mut_with(&mut ops_fns_inliner);

    // reset the syntax context of the ast tree
    let mut remove_syntax_context = syntax_context::remove_syntax_context();
    program.visit_mut_with(&mut remove_syntax_context);
    let mut resolver_pass = resolver(mark, mark, false);
    program.visit_mut_with(&mut resolver_pass);

    let mut fixer_pass = fixer(None);
    program.visit_mut_with(&mut fixer_pass);

    let mut window_var_inliner = inline_window_var::inline_window_var(mark);
    program.visit_mut_with(&mut window_var_inliner);

    program
}

fn main() {
    let default_input = &String::from("nike-obfuscated-20220516.js"); // "test.js");
    let default_output = &String::from("nike-obfuscated-20220516-deob.js"); // "test-out.js");

    let args: Vec<String> = env::args().collect();
    let filename = args.get(1).unwrap_or(default_input);
    let output_filename = args.get(2).unwrap_or(default_output);

    let buf = fs::read(filename).unwrap();
    let str = std::str::from_utf8(&buf).unwrap();

    let globals = Globals::new();
    GLOBALS.set(&globals, || {
        // Setup SWC
        // let cm: Lrc<SourceMap> = Default::default();
        let cm: Lrc<SourceMap> = Lrc::new(SourceMap::default());

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
        let mut anti_tempering = anti_tempering::extract_anti_tempering(str);
        let mut program = parser.parse_program().expect("parse_program failed");
        let mut vm_extractor = vm::extractor::extract_vm_script();
        program.visit_mut_with(&mut vm_extractor);

        if vm_extractor.get_script_id().len() > 0 {
            if !anti_tempering.is_none() {
                let at = anti_tempering.unwrap();
                if at.1 == vm_extractor.get_script_id() {
                    anti_tempering = None;
                }
            }
            let span = vm_extractor.get_script_span();

            let mut vm_code = String::from("(");
            vm_code.push_str(cm.span_to_snippet(span).unwrap().as_str());
            vm_code.push_str(")();");
            let vm_code_str = vm_code.as_str();
            let vm_anti_tempering = anti_tempering::extract_anti_tempering(vm_code_str);

            let vm_fm = cm.new_source_file(
                FileName::Custom("vm.js".into()).into(),
                vm_code_str.into(),
            );
            let mut vm_parser = Parser::new(
                Syntax::Es(EsSyntax::default()),
                StringInput::from(&*vm_fm),
                None,
            );
            let mut vm_program = vm_parser.parse_program().expect("parse_program failed");
            let deob_vm_program: &mut Program = deobfuscate(&mut vm_program, vm_anti_tempering, true);
            let mut vm_replacer = vm::replace::replace_vm_script(vm_extractor.get_script_id(), &deob_vm_program);
            program.visit_mut_with(&mut vm_replacer);
        }
        let program: &mut Program = deobfuscate(&mut program, anti_tempering, false);

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
