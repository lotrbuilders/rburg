use crate::*;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, TokenStreamExt};

pub(super) fn emit_get_vregisters(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_get_vregisters_arm(&program.definitions[i].pattern, &quote! {index});
        let i = i as u16;
        arms.append_all(quote! {
            #i => {#arm}
        });
    }

    let mut result_arm = TokenStream::new();
    for i in 0..program.definitions.len() {
        if let DefinitionType::Reg(class) = &program.definitions[i].name {
            let i = i as u16;
            let class = format_ident!("REG_CLASS_{}", class.to_string().to_uppercase());
            result_arm.append_all(quote! {
                #i => Some((self.instructions[index as usize].get_result().unwrap(),#class.clone())),
            });
        }
    }
    quote! {

        fn get_vregisters(&self,index:u32,rule:u16) -> (SmallVec<[(u32, RegisterClass<Register>);4]>,Option<(u32, RegisterClass<Register>)>)
        {
            let used_vregs=if let IRInstruction::Call(_,_,_,arguments) = &self.instructions[index as usize] {
                let used_vregs = arguments.arguments.iter().map(|r| r.unwrap()).collect::<Vec<u32>>();
                let used_classes = self.get_call_regs(&arguments.sizes);
                used_vregs.iter().zip(used_classes.iter()).map(|(r,c)| (*r,(*c).clone())).collect()

            }else if let IRInstruction::CallV(_,_,addr,arguments) = &self.instructions[index as usize] {
                let used_vregs = arguments.arguments.iter().map(|r| r.unwrap()).collect::<Vec<u32>>();
                let used_classes = self.get_call_regs(&arguments.sizes);
                let mut result:SmallVec<_>=used_vregs.iter().zip(used_classes.iter()).map(|(r,c)| (*r,(*c).clone())).collect();
                self.get_vregisters2(index, rule, &mut result);//Should have exactly one register, so this should be fine
                result

            } else {
                let mut used_vregs=SmallVec::new();
                self.get_vregisters2(index, rule, &mut used_vregs);
                used_vregs
            };

            let result_vreg=self.has_result(index,rule);
            (used_vregs,result_vreg)
        }

        fn get_vregisters2(&self,index:u32,rule:u16, result:&mut SmallVec<[(u32, RegisterClass<Register>);4]>) -> ()
        {
            match rule
            {
                #arms
                0xfffe => (),
                _ => {
                    log::error!("Unsupporteded rule {}",rule);
                }
            };
        }

        fn has_result(&self,index:u32,rule:u16)-> Option<(u32, RegisterClass<Register>)>{
            if let IRInstruction::Label(Some(_),_) = self.instructions[index as usize] {
                return None;
            }
            match rule
            {
                #result_arm
                _ => None,
            }
        }
    }
}

fn emit_get_vregisters_arm(pattern: &IRPattern, prelude: &TokenStream) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
            left,
            right,
        } => {
            let left_prelude = if let IRPattern::Reg(..) = **left {
                quote! {self.get_left_vreg(#prelude) }
            } else {
                quote! {self.get_left_index(#prelude)}
            };

            let mut left = emit_get_vregisters_arm(&*left, &left_prelude);
            if let Some(right) = right {
                let right_prelude = if let IRPattern::Reg(..) = **right {
                    quote! {self.get_right_vreg(#prelude) }
                } else {
                    quote! {self.get_right_index(#prelude)}
                };
                let right = emit_get_vregisters_arm(&*right, &right_prelude);
                left.append_all(right);
            }
            left
        }
        IRPattern::NonTerm(_, nonterm) => {
            // Challenge: get registers from another rule here(rewrite to iterators?)
            let nt_type = format_ident!("{}_NT", nonterm);
            quote! {
                let rule=self.instruction_states[#prelude as usize].rule[#nt_type];
                let index=#prelude;
                self.get_vregisters2(index,rule,result);
            }
            //todo!()
        }
        IRPattern::Reg(_, class) => {
            let class = format_ident!("REG_CLASS_{}", class.to_string().to_uppercase());
            quote! {
                result.push((#prelude, #class.clone()));
            }
        }

        IRPattern::Const(_) => TokenStream::new(),
    }
}

pub(super) fn emit_two_address(program: &Program) -> TokenStream {
    let mut result = TokenStream::new();
    let mut first = true;
    for index in 0..program.definitions.len() {
        let pattern = &program.definitions[index];
        let rule = index as u16;
        if pattern.two_address {
            if !first {
                result.append_all(quote! {|});
            }
            first = false;
            result.append_all(quote! {#rule});
        }
    }
    if result.is_empty() {
        quote! { fn is_two_address(rule:u16) -> bool { false } }
    } else {
        quote! {
            fn is_two_address(rule:u16) -> bool {
                match rule {
                    #result => true,
                    _ => false,
                }
            }
        }
    }
}
