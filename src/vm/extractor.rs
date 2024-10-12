#![allow(clippy::borrowed_box)]

use swc_core::common::{Span, Spanned, DUMMY_SP};
use swc_core::ecma::ast::*;
use swc_core::ecma::visit::{noop_visit_mut_type, VisitMut, VisitMutWith};

pub fn extract_vm_script() -> impl 'static + VisitMut + IVmScriptExtractor {
    VmScriptExtractor{
        ..Default::default()
    }
}

pub trait IVmScriptExtractor {
    fn get_script_id(&self) -> &str {
        ""
    }

    fn get_script_span(&self) -> Span {
        DUMMY_SP
    }
}

#[derive(Default)]
struct VmScriptExtractor {
    vm_script_id: String,
    vm_script_span: Span,
    function_nesting_level: u32,
    switch_case_nesting_level: u32,
}

impl IVmScriptExtractor for VmScriptExtractor {
    fn get_script_id(&self) -> &str {
        self.vm_script_id.as_str()
    }

    fn get_script_span(&self) -> Span {
        self.vm_script_span
    }
}

impl VisitMut for VmScriptExtractor {
    noop_visit_mut_type!(fail);

    fn visit_mut_program(&mut self, program: &mut Program) {
        self.function_nesting_level = 0;
        self.switch_case_nesting_level = 0;

        program.visit_mut_children_with(self);
    }

    fn visit_mut_fn_expr(&mut self, node: &mut FnExpr) {
        node.visit_mut_children_with(self);

        if self.vm_script_id.is_empty() && self.function_nesting_level > 0 && self.switch_case_nesting_level > 0 {
            if let Some(i) = &node.ident{
                if i.sym.len() == 10 {
                    self.vm_script_id = i.sym.to_string();
                    self.vm_script_span = node.span();
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
}
