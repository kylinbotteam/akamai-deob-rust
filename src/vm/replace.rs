#![allow(clippy::borrowed_box)]

use swc_core::ecma::ast::*;
use swc_core::ecma::visit::{noop_visit_type, noop_visit_mut_type, Visit, VisitWith, VisitMut, VisitMutWith };

pub fn replace_vm_script<'a>(vm_script_id: &'a str, vm_program: &'a Program) -> impl 'a + VisitMut {
    VmScriptReplacer{
        vm_script_id: vm_script_id,
        vm_program: vm_program,
        vm_script_func: None,
        function_nesting_level: 0,
        switch_case_nesting_level: 0
    }
}

struct VmScriptReplacer<'a> {
    vm_script_id: &'a str,
    vm_program: &'a Program,
    vm_script_func: Option<Box<FnExpr>>,
    function_nesting_level: u32,
    switch_case_nesting_level: u32,
}

impl VisitMut for VmScriptReplacer<'_> {
    noop_visit_mut_type!(fail);

    fn visit_mut_program(&mut self, program: &mut Program) {
        self.function_nesting_level = 0;
        self.switch_case_nesting_level = 0;

        let mut finder = VmScriptFinder{
            ..Default::default()
        };
        self.vm_program.visit_with(&mut finder);
        self.vm_script_func = finder.vm_script_func;
    
        program.visit_mut_children_with(self);
    }

    fn visit_mut_fn_expr(&mut self, node: &mut FnExpr) {
        node.visit_mut_children_with(self);

        if !self.vm_script_func.is_none() && self.function_nesting_level > 0 && self.switch_case_nesting_level > 0 {
            if let Some(i) = &node.ident{
                if i.sym == self.vm_script_id {
                    let vm_script_func = self.vm_script_func.clone().unwrap().clone();
                    *node = *vm_script_func;
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

#[derive(Default)]
struct VmScriptFinder {
    vm_script_func: Option<Box<FnExpr>>,
    function_nesting_level: u32,
    switch_case_nesting_level: u32,
}

impl Visit for VmScriptFinder {
    noop_visit_type!(fail);

    fn visit_program(&mut self, program: & Program) {
        self.function_nesting_level = 0;
        self.switch_case_nesting_level = 0;

        program.visit_children_with(self);
    }

    fn visit_fn_expr(&mut self, node: & FnExpr) {
        node.visit_children_with(self);

        if self.vm_script_func.is_none() && self.function_nesting_level == 0 && self.switch_case_nesting_level == 0 {
            if let Some(i) = &node.ident{
                if i.sym.len() == 10 {
                    let func = node.clone();
                    self.vm_script_func = Some(Box::new(func));
                }

            }
        }
    }

    fn visit_function(&mut self, node: & Function) {
        self.function_nesting_level += 1;
        node.visit_children_with(self);
        self.function_nesting_level -= 1;
    }

    fn visit_switch_case(&mut self, node: & SwitchCase) {
        self.switch_case_nesting_level += 1;
        node.visit_children_with(self);
        self.switch_case_nesting_level -= 1;
    }
}
