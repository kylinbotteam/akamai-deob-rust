#![allow(clippy::borrowed_box)]

use swc_core::common::DUMMY_SP;
use swc_core::common::collections::AHashMap;
use swc_core::ecma::ast::*;
use swc_core::ecma::visit::{as_folder, noop_visit_mut_type, Fold, VisitMut, VisitMutWith};

/// This pass is kind of inliner, but it's far faster.
pub fn inline_lazy_initializer() -> impl 'static + Fold + VisitMut {
    as_folder(InlineLazyInitializer{
        phase: Phase::Analysis,
        scope: Default::default()
    })
}

#[derive(Default)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Phase {
    #[default]
    Analysis,
    Inlining,
}

#[derive(Default)]
struct InlineLazyInitializer<'a> {
    phase: Phase,
    scope: Scope<'a>
}
#[derive(Default)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    /// Stores only inlinable constant variables.
    vars: AHashMap<Id, Box<Expr>>,
}

impl<'a> Scope<'a> {
    fn new(parent: &'a Scope<'a>) -> Self {
        Self {
            parent: Some(parent),
            vars: Default::default(),
        }
    }

    fn find_var(&self, id: &Id) -> Option<&Box<Expr>> {
        if let Some(v) = self.vars.get(id) {
            return Some(v);
        }

        self.parent.and_then(|parent| parent.find_var(id))
    }
}

impl VisitMut for InlineLazyInitializer<'_> {
    noop_visit_mut_type!(fail);

    fn visit_mut_program(&mut self, program: &mut Program) {
        let old_phase = self.phase;

        self.phase = Phase::Analysis;
        program.visit_mut_children_with(self);

        // Inline
        self.phase = Phase::Inlining;
        program.visit_mut_children_with(self);

        self.phase = old_phase;
    }

    fn visit_mut_decl(&mut self, decl: &mut Decl) {
        decl.visit_mut_children_with(self);

        if self.phase == Phase::Inlining {
            match decl {
                Decl::Fn(fn_decl) => {
                    if let Some(expr) = self.scope.find_var(&fn_decl.ident.to_id()) {
                        *decl = Decl::Var(Box::new(VarDecl{
                            span: DUMMY_SP,
                            ctxt: fn_decl.ident.ctxt,
                            kind: VarDeclKind::Var,
                            declare: false,
                            decls: vec![VarDeclarator{
                                span: DUMMY_SP,
                                name: Pat::Ident(fn_decl.ident.clone().into()),
                                init: Some(expr.clone()),
                                definite: false
                            }]
                        }));
                    }
                },
                _ => {}
            }
        }
    }

    fn visit_mut_expr(&mut self, e: &mut Expr) {
        if self.phase == Phase::Inlining {
            match e {
                Expr::Call(call_expr) => {
                    match &call_expr.callee {
                        Callee::Expr(expr) => {
                            match &**expr {
                                Expr::Ident(i) => {
                                    if let Some(expr) = self.scope.find_var(&i.to_id()) {
                                        *e = Expr::Ident(Ident{
                                            span: DUMMY_SP,
                                            ctxt: i.ctxt,
                                            sym: i.sym.clone(),
                                            optional: false
                                        });
                                        return;
                                    }
                                },
                                _ => ()
                            }
                        },
                        _ => ()
                    }
                }

                _ => ()
            }
        }

        e.visit_mut_children_with(self);
    }

    fn visit_mut_fn_decl(&mut self, n: &mut FnDecl) {
        n.visit_mut_children_with(self);

        if self.phase == Phase::Analysis {
            let (initializer, var_decl) = is_lazy_initializer(&self.scope, n);
            if initializer {
                let init = var_decl.unwrap().decls.get(0).unwrap().init.as_ref().unwrap();
                self.scope.vars.insert(n.ident.to_id(), init.clone().into());
            }
        }
    }
}

fn is_lazy_initializer<'a>(scope: &Scope<'_>, fn_decl: &'a FnDecl) -> (bool, Option<&'a VarDecl>) {
    if fn_decl.ident.sym.is_empty() {
        return (false, None)
    }

    let f = fn_decl.function.as_ref();

    if f.is_async || f.is_generator {
        return (false, None)
    }
    if f.body.is_none() || f.body.as_ref().unwrap().stmts.len() != 3 {
        return (false, None)
    }
    if f.params.len() > 0 {
        return (false, None)
    }

    let return_id: Id;
    if let Some(first_statment) = f.body.as_ref().unwrap().stmts.get(2) {
        match first_statment {
            Stmt::Return(expr) => {
                if let Some(arg) = &expr.arg {
                    match &**arg {
                        Expr::Ident(id) => {
                            return_id = id.to_id();
                        },
                        _ => {
                            return (false, None)
                        }
                    }
                } else {
                    return (false, None)
                }
            },
            _ => {
                return (false, None)
            }
        }
    } else {
        return (false, None)
    }

    if let Some(first_statment) = f.body.as_ref().unwrap().stmts.get(1) {
        match first_statment {
            Stmt::Expr(expr) => {
                match *expr.expr {
                    Expr::Assign(_) => {
                    },
                    _ => {
                        return (false, None)
                    }
                }
            },
            _ => {
                return (false, None)
            }
        }
    } else {
        return (false, None)
    }

    let mut var_decl : Option<&VarDecl> = None;
    if let Some(first_statment) = f.body.as_ref().unwrap().stmts.get(0) {
        match first_statment {
            Stmt::Decl(_decl) => {
                match _decl {
                    Decl::Var(var) => {
                        if var.decls.len() != 1 {
                            return (false, None);
                        }
                        var_decl = Some(var);
                    },
                    _ => {
                        return (false, None)
                    }
                }
            },
            _ => {
                return (false, None)
            }
        }
    }
    if let Some(decl) = var_decl {
        if let Some(var_declarator) = decl.decls.get(0) {
            match &var_declarator.name {
                Pat::Ident(id) => {
                    if id.to_id() != return_id {
                        return (false, None)
                    }
                },
                _ => {
                    return (false, None)
                }
            }
        }
        return (true, var_decl);
    }
    return (false, None)
}