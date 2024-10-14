#![allow(clippy::borrowed_box)]

#![feature(core_intrinsics)]
use std::intrinsics;
use ::std::intrinsics::breakpoint;

use swc_core::common::collections::{AHashMap, AHashSet};
use swc_core::common::DUMMY_SP;
use swc_core::ecma::ast::*;
use swc_core::ecma::visit::{as_folder, noop_visit_mut_type, Fold, Visit, VisitMut, VisitMutWith, VisitWith};
use swc_core::ecma::transforms::base::pass::Repeated;

pub fn inline_ops_fns() -> impl 'static + Fold + VisitMut + Repeated {
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
    changed: bool,
    phase: Phase,
    scope: Scope<'a>
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

impl Repeated for InlineOpsFns<'_> {
    fn changed(&self) -> bool {
        self.changed
    }

    fn reset(&mut self) {
        self.phase = Phase::Analysis;
        self.changed = false;
        self.scope = Default::default();
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
        match &node.name {
            Pat::Ident(ident) => {
                if let Some(init) = &node.init {
                    match &**init {
                        Expr::Fn(fn_expr) => {
                            if self.phase == Phase::Analysis {
                                if is_ops_fn_expr_like(fn_expr) {
                                    std::print!("Scanning op fns: {}\n", ident.sym);
                    
                                    let mut visitor = OpFnsVisitor{
                                        ..Default::default()
                                    };
                                    fn_expr.visit_with(&mut visitor);
                    
                                    if visitor.done && visitor.result {
                                        std::print!("Found op fns: {}\n", ident);
                                        self.scope.fns.insert(ident.to_id(), OpsFn::Expr(Box::new(fn_expr.clone())));
                                    }
                                    return
                                }
                            } else if self.phase == Phase::Inlining {
                            } 
                        },
                        _ => {}
                    }
                }
            },
            _ => {}
        }

        node.visit_mut_children_with(self);
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
        e.visit_mut_children_with(self);

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

                                                        *e = Expr::Paren(ParenExpr{
                                                            span: DUMMY_SP,
                                                            expr: stmts
                                                        });
                                                        self.changed = true;
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
                                                        self.changed = true;
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
                },
                _ => ()
            }
        }
    }

    fn visit_mut_fn_decl(&mut self, n: &mut FnDecl) {
        n.visit_mut_children_with(self);

        if self.phase == Phase::Analysis {
            if is_ops_fn_decl_like(n) {
                std::print!("Scanning op fns: {}\n", n.ident.sym);

                let mut visitor = OpFnsVisitor{
                    ..Default::default()
                };
                n.visit_with(&mut visitor);

                if visitor.done && visitor.result {
                    std::print!("Found op fns: {}\n", n.ident.sym);
                    self.scope.fns.insert(n.ident.to_id(), OpsFn::Decl(Box::new(n.clone())));
                }
            }
        }
    }

    fn visit_mut_fn_expr(&mut self, n: &mut FnExpr) {
        n.visit_mut_children_with(self);

        if self.phase == Phase::Analysis {
            if n.ident.is_some() && is_ops_fn_expr_like(n) {
                let mut visitor = OpFnsVisitor{
                    ..Default::default()
                };
                n.visit_with(&mut visitor);

                if visitor.done && visitor.result {
                    let ident = n.ident.as_ref().unwrap();
                    std::print!("Found op fns: {}\n", ident.sym);
                    self.scope.fns.insert(ident.to_id(), OpsFn::Expr(Box::new(n.clone())));
                }
            }
        }
    }
}

impl InlineOpsFns<'_> {
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
    members: AHashSet<Id>
}

impl Visit for OpFnsVisitor {
    fn visit_var_declarator(&mut self, node: &VarDeclarator) {
        if self.done {
            return
        }
        node.visit_children_with(self);
    }

    fn visit_expr(&mut self, node: &Expr) {
        if self.done {
            return
        }
        match node {
            Expr::Member(mem) => {
                match &*mem.obj {
                    Expr::Ident(ident) => {
                        self.members.insert(ident.to_id());
                    },
                    _ => ()
                }
                match &mem.prop {
                    MemberProp::Computed(prop) => {
                        match &*prop.expr {
                            Expr::Ident(ident) => {
                                self.members.insert(ident.to_id());
                            },
                            _ => ()
                        }
                    },
                    _ => ()
                }
            },
            _ => ()
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
        } else if self.is_member_ident(&node.to_id()) {
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

    fn is_member_ident(&self, id: &Id) -> bool{
        self.members.contains(id)
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