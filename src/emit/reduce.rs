use crate::*;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, TokenStreamExt};

pub(super) fn emit_get_child(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_get_child_arm(&program.definitions[i].pattern, &quote! {index});
        let i = i as u16;
        arms.append_all(quote! {
            #i => {vec![#arm]},
        })
    }
    quote! {
        fn get_kids(&self,index:u32,rule_number:u16) -> Vec<u32>
        {
            //log::trace!("Get kids of {} with rule {}",index,rule_number);
            if let IRInstruction::Label(Some(phi),_) = &self.instructions[index as usize] {
                let result=phi.sources.iter().
                    flat_map(|src| src.iter()).
                    map(|r| self.definition_index[*r as usize]).
                    collect();
                log::debug!("Get kids of phi node {}, {:?}",index,result);
                return result
            }
            else if let IRInstruction::Call(..,arguments) = &self.instructions[index as usize] {
                let result=arguments.arguments.iter()
                    .map(|r| r.unwrap())
                    .map(|r| self.definition_index[r as usize])
                    .collect();
                log::debug!("Get kids of call {}, {:?}",index,result);
                return result
            }
            else if let IRInstruction::CallV(..,addr,arguments) = &self.instructions[index as usize] {
                let mut result:Vec<u32>=arguments.arguments.iter()
                    .map(|r| r.unwrap())
                    .map(|r| self.definition_index[r as usize])
                    .collect();
                result.push(self.definition_index[*addr as usize]);
                log::debug!("Get kids of virtual call {}, {:?}",index,result);
                return result
            }
            match rule_number {
                #arms
                _ => {
                    log::error!("Rule {} does not exist(index={})",rule_number,index);
                    Vec::new()
                }
            }
        }

        fn get_child_non_terminals(&self,index:u32,rule_number:u16) -> Vec<usize>
        {
            if let IRInstruction::Label(Some(phi),_) = &self.instructions[index as usize] {
                let length=phi.sources.len()*phi.sources.get(0).map(|v| v.len()).unwrap_or(0);
                return vec![reg_NT;length];
            }
            else if let IRInstruction::Call(..,arguments) = &self.instructions[index as usize] {
                let length=arguments.arguments.len();
                return vec![reg_NT;length];
            }
            else if let IRInstruction::CallV(..,arguments) = &self.instructions[index as usize] {
                let length=arguments.arguments.len()+1;//One extra for the address, which must be in register
                return vec![reg_NT;length];
            }
            //log::trace!("Get non_terminals of rule {}",rule_number);
            match rule_number {
                0xffff => {
                    log::error!("Unallowed rule number when getting the non terminal of the children");
                    return Vec::new();
                }
                0xfffe => {
                    return Vec::new();
                }
                _ => self.non_terminals[rule_number as usize].clone()
            }
        }
    }
}

fn emit_get_child_arm(pattern: &IRPattern, prelude: &TokenStream) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
            left,
            right,
        } => {
            let left_prelude = quote! {self.get_left_index(#prelude) };
            let mut left = emit_get_child_arm(&*left, &left_prelude);
            //println!("left: {}", left.to_string());
            if let Some(right) = right {
                let right_prelude = quote! {self.get_right_index(#prelude) };
                let right = emit_get_child_arm(&*right, &right_prelude);
                left.append_all(right);
            }
            left
        }
        IRPattern::NonTerm(..) => quote! {
            #prelude,
        },
        IRPattern::Reg(_, _) => quote! {
            #prelude,
        },

        IRPattern::Const(_) => TokenStream::new(),
    }
}

pub(super) fn emit_get_non_terminals(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_get_non_terminals_arm(&program.definitions[i].pattern);
        let i = i as u16;
        arms.append_all(quote! {
            {vec![#arm]}
        });
        if i as usize != program.definitions.len() - 1 {
            arms.append_all(quote! {,})
        }
    }
    quote! {
        [#arms]
    }
}

fn emit_get_non_terminals_arm(pattern: &IRPattern) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
            left,
            right,
        } => {
            let mut left = emit_get_non_terminals_arm(&*left);
            //println!("left: {}", left.to_string());
            if let Some(right) = right {
                let right = emit_get_non_terminals_arm(&*right);
                left.append_all(right);
            }
            left
        }
        IRPattern::NonTerm(_, nonterm) => {
            let nt_type = format_ident!("{}_NT", nonterm);
            quote! {#nt_type}
        }
        IRPattern::Reg(_, _) => quote! {
            reg_NT,
        },
        IRPattern::Const(_) => TokenStream::new(),
    }
}

pub(super) fn emit_reduce_terminals(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_reduce_terminals_arm(&program.definitions[i].pattern, &quote! {index}, true);
        let i = i as u16;
        arms.append_all(quote! {
            #i => {
                #arm
            },
        })
    }
    quote! {
        fn reduce_terminals(&mut self,index:u32,rule_number:u16) -> ()
        {
            match rule_number {
                #arms
                _ => {
                    log::warn!("Rule {} does not exist(index={})",rule_number,index);
                }
            }
        }
    }
}

fn emit_reduce_terminals_arm(
    pattern: &IRPattern,
    prelude: &TokenStream,
    first: bool,
) -> TokenStream {
    //print!("emit label pattern cost");
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
            left,
            right,
        } => {
            //println!("Node");
            let left_prelude = quote! {self.get_left_index(#prelude) };
            let mut left = emit_reduce_terminals_arm(&*left, &left_prelude, false);
            //println!("left: {}", left.to_string());
            if let Some(right) = right {
                let right_prelude = quote! {self.get_right_index(#prelude) };
                let right = emit_reduce_terminals_arm(&*right, &right_prelude, false);
                left.append_all(right)
            }
            let txt = quote! {
                let i=#prelude as usize;
                self.rules[i] = 0xfffe;
            };
            if first {
                left
            } else {
                quote! {#txt #left}
            }
        }
        _ => TokenStream::new(),
    }
}
