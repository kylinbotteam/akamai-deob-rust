#![allow(clippy::print_stdout)]
use std::path::Path;

use oxc_allocator::Allocator;
use oxc_codegen::CodeGenerator;
use oxc_parser::Parser;
use oxc_span::{SourceType, SPAN};
use pico_args::Arguments;
use oxc_ast::ast::*;
use oxc_ast::visit::VisitMut;
use oxc_ast::ast_builder::AstBuilder;

use std::time::Instant;

// Instruction:
// create a `test.js`,
// run `cargo run --example minifier`

fn main() -> std::io::Result<()> {
    let mut args = Arguments::from_env();

    let name = args.subcommand().ok().flatten().unwrap_or_else(|| String::from("test.js"));

    let path = Path::new(&name);
    let source_text = std::fs::read_to_string(path)?;
    let source_type = SourceType::from_path(path).unwrap();

    let allocator = Allocator::default();
    let start_marker = Instant::now();
    let printed = deob(&allocator, &source_text, source_type);
    let dt_ms = start_marker.elapsed();
    println!("{printed}");
    println!("elapsed: {dt_ms:?}");


    Ok(())
}

struct Deobfuscator<'a> {
    ast: &'a AstBuilder<'a>
}

impl<'a> VisitMut<'a> for Deobfuscator<'a> {
    fn visit_variable_declaration(&mut self, var_decl: &mut VariableDeclaration<'a>) {
        oxc_ast::visit::walk_mut::walk_variable_declaration(self, var_decl);

        let declarations = &mut var_decl.declarations;
        for declarator in declarations.iter_mut() {
            if let Some(init) = & declarator.init {
                if let Some(simplified_expr) = simplify_expression(self.ast, init) {
                    declarator.init = Some(simplified_expr);
                }
            }
        }
    }

    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast::visit::walk_mut::walk_expression(self, expr);

        if let Expression::BinaryExpression(_) = expr {
            if let Some(simplified_expr) = simplify_expression(self.ast, expr) {
                *expr = simplified_expr;
            }
        }
    }
}

fn simplify_expression<'a>(ast_builder: &AstBuilder<'a>, expr: &Expression<'a>) -> Option<Expression<'a>> {
    if let Expression::BinaryExpression(binary_expr) = expr {
        match (&binary_expr.left, &binary_expr.right) {
            (Expression::StringLiteral(left), Expression::StringLiteral(right)) => {
                let sum = format!("{}{}", left.value.as_str(), right.value.as_str());

                let lit = ast_builder.alloc_string_literal(SPAN, sum);

                Some(Expression::StringLiteral(lit))
            },
            (Expression::NumericLiteral(left), Expression::UnaryExpression(right)) => {
                match (right.operator, &right.argument) {
                    (UnaryOperator::UnaryNegation, Expression::NumericLiteral(arg)) => {
                        match binary_expr.operator {
                            BinaryOperator::Addition => {
                                let sum = left.value - arg.value;
                                let raw_value = format!("{}", sum);
        
                                let lit = ast_builder.alloc_numeric_literal(SPAN, sum, raw_value, NumberBase::Decimal);
        
                                Some(Expression::NumericLiteral(lit))
                            },
                            BinaryOperator::Subtraction => {
                                let sum = left.value + arg.value;
                                let raw_value = format!("{}", sum);
        
                                let lit = ast_builder.alloc_numeric_literal(SPAN, sum, raw_value, NumberBase::Decimal);
        
                                Some(Expression::NumericLiteral(lit))
                            },
                            BinaryOperator::Multiplication => {
                                let sum = left.value * (-1.0 * arg.value);
                                let raw_value = format!("{}", sum);
        
                                let lit = ast_builder.alloc_numeric_literal(SPAN, sum, raw_value, NumberBase::Decimal);
        
                                Some(Expression::NumericLiteral(lit))
                            },
                            BinaryOperator::Division => {
                                let sum = left.value / (-1.0 * arg.value);
                                let raw_value = format!("{}", sum);
        
                                let lit = ast_builder.alloc_numeric_literal(SPAN, sum, raw_value, NumberBase::Decimal);
        
                                Some(Expression::NumericLiteral(lit))
                            },
                            _ => None,
                        }

                    },
                    _ => None
                }
            },
            (Expression::NumericLiteral(left), Expression::NumericLiteral(right)) => {
                match binary_expr.operator {
                    BinaryOperator::Addition => {
                        let sum = left.value + right.value;
                        let raw_value = format!("{}", sum);

                        let lit = ast_builder.alloc_numeric_literal(SPAN, sum, raw_value, NumberBase::Decimal);

                        Some(Expression::NumericLiteral(lit))
                    },
                    BinaryOperator::Subtraction => {
                        let sum = left.value - right.value;
                        let raw_value = format!("{}", sum);

                        let lit = ast_builder.alloc_numeric_literal(SPAN, sum, raw_value, NumberBase::Decimal);

                        Some(Expression::NumericLiteral(lit))
                    },
                    BinaryOperator::Multiplication => {
                        let sum = left.value * right.value;
                        let raw_value = format!("{}", sum);

                        let lit = ast_builder.alloc_numeric_literal(SPAN, sum, raw_value, NumberBase::Decimal);

                        Some(Expression::NumericLiteral(lit))
                    },
                    BinaryOperator::Division => {
                        let sum = left.value / right.value;
                        let raw_value = format!("{}", sum);

                        let lit = ast_builder.alloc_numeric_literal(SPAN, sum, raw_value, NumberBase::Decimal);

                        Some(Expression::NumericLiteral(lit))
                    },
                    _ => None,
                }
            },
            _ => None,
        }
    } else {
        None
    }
}

fn deob(
    allocator: &Allocator,
    source_text: &str,
    source_type: SourceType,
) -> String {
    let ret = Parser::new(allocator, source_text, source_type).parse();
    let ast_builder = AstBuilder{
        allocator: allocator
    };
    let mut program = ret.program;
    let mut deobfuscator = Deobfuscator{
        ast: &ast_builder
    };
    deobfuscator.visit_program(&mut program);

    CodeGenerator::new().build(&program).source_text
}