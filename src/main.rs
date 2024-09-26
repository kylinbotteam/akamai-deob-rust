use swc_common::{sync::Lrc, Globals, SourceMap, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_parser::{Parser, StringInput, Syntax};
use swc_ecma_visit::{VisitMut, VisitMutWith};

use std::collections::HashMap;

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


fn main() {
    // Input: Obfuscated JavaScript code as a string
    let code = r#"
        function _0xabc(_0x123, _0x456) {
            return _0x123 + _0x456;
        }
        const _0x789 = _0xabc(1, 2);

        const fourtyTwo = 0x25fb + 0x3eb * -0x9 + -0x28e;
        const msg = 'The\x20answer' + '\x20is:';
        console['log'](msg, fourtyTwo);
    "#;

    let cm: Lrc<SourceMap> = Default::default();
    let _globals = Globals::new();

    let fm = cm.new_source_file(
        swc_common::FileName::Custom("input.js".into()).into(),
        code.into(),
    );

    let lexer = swc_ecma_parser::lexer::Lexer::new(
        Syntax::Es(Default::default()),
        swc_ecma_ast::EsVersion::Es2020,
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    let module = parser
        .parse_module()
        .expect("Failed to parse the JavaScript code.");

    let mut deobfuscator = Deobfuscator;

    // Apply the deobfuscation transformation
    let mut transformed_module = module.clone();
    transformed_module.visit_mut_with(&mut deobfuscator);

    let mut folder = ConstantFolder::new();

    // Apply constant folding and propagation
    transformed_module.visit_mut_with(&mut folder);

    // Code generation (pretty print the transformed AST back to JavaScript)
    let mut codegen_config = swc_ecma_codegen::Config::default();
    codegen_config.minify = false;

    let mut buf = vec![];
    {
        let mut emitter = swc_ecma_codegen::Emitter {
            cfg: codegen_config,
            cm: cm.clone(),
            wr: Box::new(swc_ecma_codegen::text_writer::JsWriter::new(
                cm.clone(),
                "\n",
                &mut buf,
                None,
            )),
            comments: None,
        };

        emitter.emit_module(&transformed_module).unwrap();
    }

    let output_code = String::from_utf8(buf).unwrap();

    println!("Transformed code:\n{}", output_code);
}
