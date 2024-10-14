use swc_core::ecma::ast::*;
use swc_core::ecma::visit::{as_folder, Fold, VisitMut, VisitMutWith};
use swc_core::common::SyntaxContext;

// Visitor to remove all syntax contexts
pub struct RemoveSyntaxContext;

impl VisitMut for RemoveSyntaxContext {
    fn visit_mut_ident(&mut self, node: &mut Ident) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_class(&mut self, node: &mut Class) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_private_prop(&mut self, node: &mut PrivateProp) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_constructor(&mut self, node: &mut Constructor) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_var_decl(&mut self, node: &mut VarDecl) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_call_expr(&mut self, node: &mut CallExpr) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_new_expr(&mut self, node: &mut NewExpr) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_arrow_expr(&mut self, node: &mut ArrowExpr) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_tagged_tpl(&mut self, node: &mut TaggedTpl) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_opt_call(&mut self, node: &mut OptCall) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }

    fn visit_mut_block_stmt(&mut self, node: &mut BlockStmt) {
        node.ctxt = SyntaxContext::empty();
        node.visit_mut_children_with(self);
    }
}

// Function to apply the visitor to an AST Program
pub fn remove_syntax_context_from_ast(program: &mut Program) {
    program.visit_mut_with(&mut RemoveSyntaxContext);
}

pub fn remove_syntax_context() -> impl 'static + Fold + VisitMut {
    as_folder(RemoveSyntaxContext{
    })
}