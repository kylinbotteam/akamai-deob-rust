#![allow(clippy::borrowed_box)]

#![feature(core_intrinsics)]
use ::std::intrinsics::breakpoint;

use swc_core::common::collections::AHashMap;
use swc_core::ecma::ast::*;
use swc_core::ecma::visit::{as_folder, noop_visit_mut_type, Fold, Visit, VisitMut, VisitMutWith, VisitWith};

pub fn inline_ops_fns() -> impl 'static + Fold + VisitMut {
    as_folder(InlineOpsFns{
        phase: Phase::Analysis,
        ..Default::default()
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
struct InlineOpsFns<'a> {
    phase: Phase,
    scope: Scope<'a>,
    fn_name: Id,
    var_name: Id
}

enum OpsFn {
    Decl(Box<FnDecl>),
    Expr(Box<FnExpr>)
}

#[derive(Default)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    fns: AHashMap<Id, OpsFn>,
}

impl<'a> Scope<'a> {
    fn new(parent: &'a Scope<'a>) -> Self {
        Self {
            parent: Some(parent),
            fns: Default::default(),
        }
    }

    fn find_ops_fn(&self, id: &Id) -> Option<&OpsFn> {
        // if id.0 == "MQ" {
        //     unsafe { breakpoint() };
        // }

        if let Some(v) = self.fns.get(id) {
            return Some(v);
        }

        self.parent.and_then(|parent| parent.find_ops_fn(id))
    }
}

impl VisitMut for InlineOpsFns<'_> {
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

    fn visit_mut_var_declarator(&mut self, node: &mut VarDeclarator) {
        let var_name = self.var_name.clone();
        match &node.name {
            Pat::Ident(ident) => {
                self.var_name = ident.to_id();
            },
            _ => {
                self.var_name = Default::default();
            }
        }
        node.visit_mut_children_with(self);
        self.var_name = var_name;
    }

    fn visit_mut_decl(&mut self, decl: &mut Decl) {
        decl.visit_mut_children_with(self);

        if self.phase == Phase::Inlining {
            match decl {
                Decl::Fn(fn_decl) => {
                    if let Some(expr) = self.scope.find_ops_fn(&fn_decl.ident.to_id()) {
                        // TODO: remove this function.
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
                                    std::print!("Try to find {} {:?}\n", &i.sym, i.ctxt);
                                    if let Some(expr) = self.scope.find_ops_fn(&i.to_id()) {
                                        std::print!("Try to inline {} {:?}\n", &i.sym, i.ctxt);
                                        match expr {
                                            OpsFn::Decl(fn_decl) => {
                                                let stmt = fn_decl.function.body.as_ref().unwrap().stmts.get(0).unwrap();
                                                match stmt {
                                                    Stmt::Return(rtn) => {
                                                        let mut stmts = rtn.arg.as_ref().unwrap().clone();

                                                        let mut renamer = OpFnsRenamer{
                                                            error: false,
                                                            params: &fn_decl.function.params,
                                                            args: &call_expr.args
                                                        };
                                                        stmts.visit_mut_with(&mut renamer);

                                                        *e = *stmts;
                                                        return;
                                                    },
                                                    _ => {}
                                                }
                                            },
                                            OpsFn::Expr(fn_decl) => {
                                                let stmt = fn_decl.function.body.as_ref().unwrap().stmts.get(0).unwrap();
                                                match stmt {
                                                    Stmt::Return(rtn) => {
                                                        let mut stmts = rtn.arg.as_ref().unwrap().clone();

                                                        let mut renamer = OpFnsRenamer{
                                                            error: false,
                                                            params: &fn_decl.function.params,
                                                            args: &call_expr.args
                                                        };
                                                        stmts.visit_mut_with(&mut renamer);

                                                        *e = *stmts;
                                                        return;
                                                    },
                                                    _ => {}
                                                }
                                            },
                                            _ => {}
                                        }
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
        let fn_name = self.fn_name.clone();
        self.fn_name = n.ident.to_id();
        n.visit_mut_children_with(self);

        if self.phase == Phase::Analysis {
            let name: &(swc::atoms::Atom, swc_core::common::SyntaxContext) = if self.fn_name.0.len() > 0 { &self.fn_name } else { &self.var_name };
            if name.0.len() > 0 && is_ops_fn_decl_like(n) {
                std::print!("Scanning op fns: {}\n", n.ident.sym);

                let mut visitor = OpFnsVisitor{
                    ..Default::default()
                };
                n.visit_with(&mut visitor);

                if visitor.done && visitor.result {
                    std::print!("Found op fns: {}\n", name.0);
                    self.scope.fns.insert(name.clone(), OpsFn::Decl(Box::new(n.clone())));
                }
            }
        }
        self.fn_name = fn_name;
    }

    fn visit_mut_fn_expr(&mut self, n: &mut FnExpr) {
        let fn_name = self.fn_name.clone();
        if n.ident.is_some() {
            self.fn_name = n.ident.as_ref().unwrap().to_id();
        } else {
            self.fn_name = Default::default();
        }
        n.visit_mut_children_with(self);

        if self.phase == Phase::Analysis {
            let name: &(swc::atoms::Atom, swc_core::common::SyntaxContext) = if self.fn_name.0.len() > 0 { &self.fn_name } else { &self.var_name };
            if name.0.len() > 0 && is_ops_fn_expr_like(n) {
                let mut visitor = OpFnsVisitor{
                    ..Default::default()
                };
                n.visit_with(&mut visitor);

                if visitor.done && visitor.result {
                    std::print!("Found op fns: {}\n", name.0);
                    self.scope.fns.insert(name.clone(), OpsFn::Expr(Box::new(n.clone())));
                }
            }
        }
        self.fn_name = fn_name;
    }
}

fn is_ops_fn_decl_like<'a,>(fn_decl: &'a FnDecl) -> bool {
    let f = fn_decl.function.as_ref();

    if f.is_async || f.is_generator {
        return false;
    }
    if f.body.is_none() || f.body.as_ref().unwrap().stmts.len() != 1 {
        return false;
    }

    if f.params.len() == 0 {
        return false;
    }

    if let Some(stmt) = f.body.as_ref().unwrap().stmts.get(0) {
        return stmt.is_return_stmt()
    }
    return false;
}

fn is_ops_fn_expr_like<'a,>(fn_decl: &'a FnExpr) -> bool {
    let f = fn_decl.function.as_ref();

    if f.is_async || f.is_generator {
        return false;
    }
    if f.body.is_none() || f.body.as_ref().unwrap().stmts.len() != 1 {
        return false;
    }

    if f.params.len() == 0 {
        return false;
    }

    if let Some(stmt) = f.body.as_ref().unwrap().stmts.get(0) {
        return stmt.is_return_stmt()
    }
    return false;
}

#[derive(Default)]
struct OpFnsVisitor {
    result: bool,
    done: bool,
    function_nesting_level: u32,
    params: AHashMap<Id, Box<Param>>,
    fn_name: Id,
    var_name: Id
}

impl Visit for OpFnsVisitor {
    fn visit_var_declarator(&mut self, node: &VarDeclarator) {
        if self.done {
            return
        }
        node.visit_children_with(self);
    }

    fn visit_fn_expr(&mut self, node: &FnExpr) {        
        if self.done {
            return
        }
        self.function_nesting_level += 1;
        if self.function_nesting_level == 1 {
            self.result = true;
            if node.function.params.len() > 0 {
                for param in node.function.params.iter() {
                    self.params.insert(param.pat.as_ident().unwrap().to_id(), Box::new(param.clone()));
                }
            }
            if let Some(ident) = &node.ident {
                self.fn_name = ident.to_id();
            }
            node.visit_children_with(self);
            self.done = true;
        } else if self.function_nesting_level > 1 {
            self.fail();
        }
        self.function_nesting_level -= 1;
    }

    fn visit_fn_decl(&mut self, node: &FnDecl) {
        if self.done {
            return
        }
        self.function_nesting_level += 1;
        if self.function_nesting_level == 1 {
            self.result = true;
            if node.function.params.len() > 0 {
                for param in node.function.params.iter() {
                    self.params.insert(param.pat.as_ident().unwrap().to_id(), Box::new(param.clone()));
                }
            }
            self.fn_name = node.ident.to_id();
            node.visit_children_with(self);
            self.done = true;
        } else if self.function_nesting_level > 1 {
            self.fail();
        }
        self.function_nesting_level -= 1;
    }

    fn visit_function(&mut self, node: &Function) {
        if self.done {
            return
        }
        if self.function_nesting_level > 1 {
            self.fail();
            return
        }

        if node.params.len() == 0 || node.body.is_none() || node.body.as_ref().unwrap().stmts.len() != 1 {
            self.fail();
            return
        }
        let stmt = node.body.as_ref().unwrap().stmts.get(0).unwrap();
        if !stmt.is_return_stmt() {
            self.fail();
            return
        }

        node.visit_children_with(self);
    }

    fn visit_ident(&mut self, node: &Ident) {
        if self.done {
            return
        }
        if node.to_id() == self.fn_name {
        } else if let Some(_) = self.find_param(&node.to_id()) {
        } else {
            self.fail();
        }
    }
}

impl OpFnsVisitor {
    fn fail (&mut self) {
        self.done = true;
        self.result = false;
    }

    fn find_param(&self, id: &Id) -> Option<&Box<Param>> {
        if let Some(v) = self.params.get(id) {
            return Some(v);
        }

        None
    }
}

struct OpFnsRenamer<'a> {
    error: bool,
    params: &'a Vec<Param>,
    args: &'a Vec<ExprOrSpread>
}

impl VisitMut for OpFnsRenamer<'_> {
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        if self.error {
            return
        }
        if node.is_ident() {
            let ident = node.clone().ident().unwrap();
            for (idx, param) in self.params.iter().enumerate() {
                if param.pat.as_ident().unwrap().sym == ident.sym {
                    if idx >= self.args.len() {
                        self.error = true;
                        return
                    }
                    let expr_or_spread = self.args.get(idx).unwrap();
                    *node = expr_or_spread.expr.as_ref().clone();
                    break
                }
            }
        } else {
            node.visit_mut_children_with(self);
        }
    }
}