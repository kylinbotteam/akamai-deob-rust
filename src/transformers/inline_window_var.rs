#![allow(clippy::borrowed_box)]

use swc::atoms::Atom;
use swc_core::common::{SyntaxContext, Mark};
use swc_core::ecma::ast::*;
use swc_core::ecma::visit::{noop_visit_mut_type, VisitMut, VisitMutWith};

pub fn inline_window_var(unresolved_mark: Mark) -> impl 'static + VisitMut + IGlobalVar {
    WindowVarInliner{
        unresolved_mark,
        ..Default::default()
    }
}

#[derive(Default)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Phase {
    #[default]
    Analysis,
    Inlining
}

pub trait IGlobalVar {
    fn get_global_var_id(&self) -> Option<Id> {
        None
    }
}

#[derive(Default)]
struct WindowVarInliner {
    unresolved_mark: Mark,
    phase: Phase,
    global_var_id: Option<Id>,
    window_var_id: Option<Id>,
    intializer_id: Id,
    function_nesting_level: u32,
    switch_case_nesting_level: u32,
}

impl IGlobalVar for WindowVarInliner {
    fn get_global_var_id(&self) -> Option<Id> {
        self.global_var_id.clone()
    }
}

// function wN() {
//     VZ = Object['\x63\x72\x65\x61\x74\x65']({});
//     RL = 14;
//     S1[gAT[RL]] = bHnFJOVcEB;
//     if (typeof window !== [] + [][[]]) {
//         Xm = window;
//     } else if (typeof global !== 'undefined') {
//         Xm = global;
//     } else {
//         Xm = this;
//     }
// }
impl VisitMut for WindowVarInliner {
    noop_visit_mut_type!(fail);

    fn visit_mut_program(&mut self, program: &mut Program) {
        self.function_nesting_level = 0;
        self.switch_case_nesting_level = 0;

        self.phase = Phase::Analysis;
        program.visit_mut_children_with(self);

        if self.window_var_id.is_some() {
            self.phase = Phase::Inlining;
            program.visit_mut_children_with(self);
        }
    }

    fn visit_mut_fn_decl(&mut self, node: &mut FnDecl) {
        if self.phase == Phase::Inlining {
            if node.ident.to_id() == self.intializer_id {
                return
            }
        }

        node.visit_mut_children_with(self);

        if self.phase == Phase::Analysis {
            if self.global_var_id.is_none() && self.function_nesting_level < 2 && self.switch_case_nesting_level == 0 {
                if let Some(id) = find_window_var_fn(node) {
                    std::print!("Found window var initializer: {} {} {}\n", node.ident.sym, &(id.0.0), &id.1.0);

                    self.global_var_id = Some(id.0);
                    self.window_var_id = Some(id.1);
                    self.intializer_id = node.ident.to_id();
                }
            }
        }
    }

    fn visit_mut_function(&mut self, node: &mut Function) {
        self.function_nesting_level += 1;
        node.visit_mut_children_with(self);
        self.function_nesting_level -= 1;
    }

    fn visit_mut_switch_case(&mut self, node: &mut SwitchCase) {
        self.switch_case_nesting_level += 1;
        node.visit_mut_children_with(self);
        self.switch_case_nesting_level -= 1;
    }

    fn visit_mut_member_expr(&mut self, node: &mut MemberExpr) {
        if self.phase == Phase::Inlining {
            if node.obj.is_ident() {
                let ident = node.obj.as_mut_ident().unwrap();
                let window_var_id = self.window_var_id.as_ref().unwrap();
                if ident.to_id() == *window_var_id {
                    std::print!("Replacing window var {}\n", window_var_id.0);
                    ident.sym = Atom::new("window");
                    ident.ctxt = SyntaxContext::empty().apply_mark(self.unresolved_mark)
                }
            }
        }
        node.visit_mut_children_with(self);
    }
}

fn find_window_var_fn(fn_decl: &FnDecl) -> Option<(Id, Id)> {
    if fn_decl.ident.sym.is_empty() {
        return None
    }
    if let Some(body) = &fn_decl.function.body {
        if body.stmts.len() == 4 {
            std::print!("Scanning window var fn: {}\n", fn_decl.ident.sym);
            let stmt0 = body.stmts.get(0).unwrap();
            let stmt1 = body.stmts.get(1).unwrap();
            let stmt2 = body.stmts.get(2).unwrap();
            let stmt3 = body.stmts.get(3).unwrap();

            match (stmt0, stmt1, stmt2, stmt3) {
                (Stmt::Expr(exp0), Stmt::Expr(exp1), Stmt::Expr(exp2), Stmt::If(if_expr)) => {
                    if !if_expr.cons.is_block() || if_expr.alt.is_none() {
                        return None
                    }
                    let block_expr = if_expr.cons.as_ref().as_block().unwrap();
                    if block_expr.stmts.len() != 1 {
                        return None
                    }
                    if !block_expr.stmts.get(0).unwrap().is_expr() {
                        return None
                    }

                    let cons_expr = block_expr.stmts.get(0).unwrap().as_expr().unwrap();
                    if !cons_expr.expr.is_assign() {
                        return None
                    }
                    let assign_expr = cons_expr.expr.as_assign().unwrap();
                    if !assign_expr.right.is_ident() {
                        return None
                    }
                    if assign_expr.right.as_ident().unwrap().sym != "window" {
                        return None
                    }

                    if !assign_expr.is_simple_assign() {
                        return None
                    }
                    let window_var_id = assign_expr.left.as_ident().unwrap().to_id();
                    match (&*exp0.expr, &*exp1.expr, &*exp2.expr) {
                        (Expr::Assign(assign), Expr::Assign(_), Expr::Assign(_)) => {
                            match &assign.left {
                                AssignTarget::Simple(target) => {
                                    match target {
                                        SimpleAssignTarget::Ident(ident) => {
                                            return Some((ident.to_id(), window_var_id))
                                        },
                                        _ => {
                                            return None
                                        }

                                    }
                                },
                                _ => {
                                    return None
                                }
                            }
                        },
                        _ => {
                            return None
                        }
                    }
                },
                _ => {
                    return None
                }
            }
        }
    }
    None
}
