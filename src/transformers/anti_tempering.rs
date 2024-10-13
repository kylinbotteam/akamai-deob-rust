#![allow(clippy::borrowed_box)]

use swc_core::ecma::ast::*;
use swc_core::ecma::visit::{noop_visit_mut_type, VisitMut, VisitMutWith};

pub fn extract_anti_tempering_key() -> impl 'static + VisitMut + IAntiTempering {
    AntiTemperingKey{
        ..Default::default()
    }
}

pub trait IAntiTempering {
    fn get_anti_tempering_key_str(&self) -> Option<String> {
        None
    }

    fn get_anti_tempering_key_nonce(&self) -> u32 {
        0
    }
}

#[derive(Default)]
struct AntiTemperingKey {
    vm_script_key_str: Option<String>,
    vm_script_key_nonce: u32,
    function_nesting_level: u32,
    switch_case_nesting_level: u32,
}

impl IAntiTempering for AntiTemperingKey {
    fn get_anti_tempering_key_str(&self) -> Option<String> {
        if self.vm_script_key_str.is_none() {
            return None
        }
        return self.vm_script_key_str.clone();
    }

    fn get_anti_tempering_key_nonce(&self) -> u32 {
        return self.vm_script_key_nonce
    }
}

// nonce
// function N6T() {
//     return pWT(x5T(), 6879);
// }
//
// key
// function L0T() {
//     return ZlT(`${S1[MA(RL)]}`, "0xcc9a8af");
// }
// function L0T() {
//     return ZlT(`${S1()[MA(RL)]}`, "0x" + "\x63\x63\x39\x61\x38\x61\x66");
// }
impl VisitMut for AntiTemperingKey {
    noop_visit_mut_type!(fail);

    fn visit_mut_program(&mut self, program: &mut Program) {
        self.function_nesting_level = 0;
        self.switch_case_nesting_level = 0;

        program.visit_mut_children_with(self);
    }

    fn visit_mut_fn_expr(&mut self, node: &mut FnExpr) {
        node.visit_mut_children_with(self);
    }

    fn visit_mut_fn_decl(&mut self, node: &mut FnDecl) {
        node.visit_mut_children_with(self);

        if self.vm_script_key_str.is_none() && self.function_nesting_level < 2 && self.switch_case_nesting_level == 0 {
            if let Some(key) = find_anti_tempering_key_fn(node) {
                self.vm_script_key_str = Some(key);
                return
            }
        }
        if self.vm_script_key_nonce == 0 && self.function_nesting_level < 2 && self.switch_case_nesting_level == 0 {
            if let Some(nonce) = find_anti_tempering_key_nonce_fn(node) {
                self.vm_script_key_nonce = nonce;
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

fn find_anti_tempering_key_fn(fn_decl: &FnDecl) -> Option<String> {
    if fn_decl.ident.sym.is_empty() {
        return None
    }
    if let Some(body) = &fn_decl.function.body {
        if body.stmts.len() == 1 {
            let stmt = body.stmts.get(0).unwrap();
            match stmt {
                Stmt::Return(rtn) => {
                    if rtn.arg.is_none() {
                        return None
                    }
                    let arg = rtn.arg.as_ref().unwrap();
                    match &**arg {
                        Expr::Call(call) => {
                            if call.args.len() != 2 {
                                return None
                            }
                            let arg1 = call.args.get(0).unwrap();
                            let arg2 = call.args.get(1).unwrap();
                            match (&*arg1.expr, &*arg2.expr) {
                                (Expr::Tpl(_tpl), Expr::Bin(bin_expr)) => {
                                    match &*bin_expr.left {
                                        Expr::Lit(lhs) => {
                                            match lhs {
                                                Lit::Str(lhs_str) => {
                                                    if lhs_str.value != "0x" {
                                                        return None
                                                    }
                                                    match &*bin_expr.right {
                                                        Expr::Lit(rhs) => {
                                                            match rhs {
                                                                Lit::Str(rhs_str) => {
                                                                    let mut key = lhs_str.value.as_str().to_owned();
                                                                    key.push_str(rhs_str.value.as_str());
                                                                    return Some(key);
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
                                },
                                (Expr::Tpl(_tpl), Expr::Lit(lit)) => {
                                    match lit {
                                        Lit::Str(rhs_str) => {
                                            if rhs_str.value.starts_with("0x") {
                                                return Some(rhs_str.value.as_str().to_owned());
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
                },
                _ => {
                    return None
                }
            }
        }
    }
    None
}

fn find_anti_tempering_key_nonce_fn(fn_decl: &FnDecl) -> Option<u32> {
    if fn_decl.ident.sym.is_empty() {
        return None
    }
    if let Some(body) = &fn_decl.function.body {
        if body.stmts.len() == 1 {
            let stmt = body.stmts.get(0).unwrap();
            match stmt {
                Stmt::Return(rtn) => {
                    if rtn.arg.is_none() {
                        return None
                    }
                    let arg = rtn.arg.as_ref().unwrap();
                    match &**arg {
                        Expr::Call(call) => {
                            if call.args.len() != 2 {
                                return None
                            }
                            let arg1 = call.args.get(0).unwrap();
                            let arg2 = call.args.get(1).unwrap();
                            match (&*arg1.expr, &*arg2.expr) {
                                (Expr::Call(call_expr), Expr::Lit(lit)) => {
                                    if !call_expr.callee.is_expr() {
                                        return None
                                    }
                                    let callee = call_expr.callee.as_expr().unwrap();
                                    if !callee.is_ident() {
                                        return None
                                    }
                                    if call_expr.args.len() != 0 {
                                        return None
                                    }
                                    match lit {
                                        Lit::Num(n) => {
                                            return Some(n.value as i32 as u32);
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

pub fn generate_value(func_to_string: &str, fn_name: &str, key: &str, dynamic_integer: u32) -> i32 {
    let key_index = func_to_string.find(key).unwrap_or(0);
    let semicolon_index = func_to_string[key_index..].find(';').map(|i| i + key_index).unwrap_or(0);

    let key_len = key_index + key.len() + 1;

    let anb = &func_to_string[key_len..semicolon_index];
    let qnb = &func_to_string[0..key_index];
    let znb = &func_to_string[semicolon_index + 1..];

    let tnb = format!("{}{}undefined", qnb, znb);
    let mnb = bit_stuff(&tnb, dynamic_integer);
    (anb.parse::<i64>().unwrap_or(0) - mnb as i64) as i32
}

fn bit_stuff(xnb: &str, ynb: u32) -> u32 {
    let mut gnb = ynb;
    let knb: u32 = 0xcc9e2d51;
    let vnb: u32 = 0x1b873593;
    let mut xnb_len = 0;

    let mut v: Vec<u16> = xnb.encode_utf16().collect();
    for bbb in v.iter() {
        let bbb = *bbb as u16;
        if bbb == '\n' as u16 || bbb == '\r' as u16 || bbb == ' ' as u16 {
            continue;
        }
        let mut bbb = bbb as u32;
        bbb = (bbb & 0xffff).wrapping_mul(knb).wrapping_add(((bbb >> 16).wrapping_mul(knb) & 0xffff) << 16);
        bbb = bbb.rotate_left(15);
        bbb = (bbb & 0xffff).wrapping_mul(vnb).wrapping_add(((bbb >> 16).wrapping_mul(vnb) & 0xffff) << 16);

        gnb ^= bbb;
        gnb = gnb.rotate_left(13);
        let mbb = (gnb & 0xffff).wrapping_mul(5).wrapping_add(((gnb >> 16).wrapping_mul(5) & 0xffff) << 16);
        gnb = (mbb & 0xffff).wrapping_add(0x6b64).wrapping_add((((mbb >> 16) + 0xe654) & 0xffff) << 16);

        xnb_len += 1;
    }

    gnb ^= xnb_len as u32;
    gnb ^= gnb >> 16;
    gnb = (gnb & 0xffff).wrapping_mul(0x85ebca6b).wrapping_add(((gnb >> 16).wrapping_mul(0x85ebca6b) & 0xffff) << 16);
    gnb ^= gnb >> 13;
    gnb = (gnb & 0xffff).wrapping_mul(0xc2b2ae35).wrapping_add(((gnb >> 16).wrapping_mul(0xc2b2ae35) & 0xffff) << 16);
    gnb ^= gnb >> 16;

    gnb
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bit_stuff_basic() {
        // Test with a basic string and integer
        let result = bit_stuff("hello", 12345);
        let expected_value = 357333553;
        assert_eq!(result, expected_value);
    }

    #[test]
    fn test_bit_stuff_with_whitespace() {
        // Test with a string containing spaces and newlines
        let result = bit_stuff("hello \n world", 98765);
        let expected_value = 3472114696;
        assert_eq!(result, expected_value);
    }

    #[test]
    fn test_bit_stuff_empty_string() {
        // Test with an empty string
        let result = bit_stuff("", 54321);
        let expected_value = 1241660863; // As no chars, gNb should remain the same
        assert_eq!(result, expected_value);
    }

    #[test]
    fn test_bit_stuff_unicode() {
        // Test with Unicode characters in the string
        let result = bit_stuff("こんにちは", 67890);
        let expected_value = 192897888;
        assert_eq!(result, expected_value);
    }

    #[test]
    fn test_bit_stuff_large_integer() {
        // Test with a large dynamic integer
        let result = bit_stuff("test", 0xFFFFFFFF);
        let expected_value = 1614445759;
        assert_eq!(result, expected_value);
    }

    #[test]
    fn test_bit_stuff_special_characters() {
        // Test with special characters
        let result = bit_stuff("!@#$%^&*()", 12345);
        let expected_value = 3179081968;
        assert_eq!(result, expected_value);
    }
}