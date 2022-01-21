use crate::{
    emit::register_size::{emit_default_register_width, modify_template},
    *,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, TokenStreamExt};

pub(super) fn emit_asm(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();

    for i in 0..program.definitions.len() {
        let definition = &program.definitions[i];
        let (modified_registers, modified_template) = modify_template(
            &program.settings,
            &definition.name,
            &definition.pattern,
            &definition.template.value(),
        );

        let _tab = match definition.name {
            DefinitionType::NonTerm(..) => "",
            _ => "\t",
        };
        let template = modified_template;
        let arm = emit_asm_arm(&definition.pattern, &quote! {index as u32});
        let register_width = super::register_size::emit_register_width_arm(
            &definition.pattern,
            &modified_registers,
            &quote! {index as u32},
            &quote! {instruction.get_result().unwrap()},
        );

        let (result_width, _) = if let DefinitionType::Reg(_) = definition.name {
            (
                quote! {let res_width=self.get_default_register_width(index as u32,instruction.get_result().unwrap());},
                quote! {,res_width=res_width},
            )
        } else {
            (TokenStream::new(), TokenStream::new())
        };

        let format_arm = emit_asm_format(&definition.pattern);
        let register_format = super::register_size::emit_register_width_format(&modified_registers);
        let i = i as u16;
        if has_used_result(definition) {
            arms.append_all(quote! {
                #i => {
                    let _=#template;
                    let res=self.allocation[instruction.get_result().unwrap() as usize][index].unwrap();
                    #arm
                    #register_width
                    #result_width
                    format!(#template,res=res #format_arm #register_format)
                }
            });
        } else {
            arms.append_all(quote! {
                #i => {
                    let _=#template;
                    #arm
                    #register_width
                    format!(#template #format_arm #register_format)
                }
            });
        }
    }

    let default_register_width =
        emit_default_register_width(program.settings.int_size, &program.settings.register_sizes);

    quote! {
        #default_register_width
        fn gen_asm(&self,index:usize) -> String
        {
            let rule=self.rules[index];
            self.gen_asm2(index,rule,index)
        }
        fn gen_asm2(&self,index:usize,rule:u16,original_index:usize) -> String
        {
            let instruction=&self.instructions[index];
            log::trace!("Generating {} with rule {}",instruction,rule);
            match rule {
                #arms
                0xfffe => String::new(),
                _ => {
                    log::error!("Unkown rule {} when emitting assembly instruction {}",rule,index);
                    String::new()
                }
            }
        }
    }
}

fn emit_asm_arm(pattern: &IRPattern, prelude: &TokenStream) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
            left,
            right,
        } => {
            let left_prelude = if let IRPattern::Reg(..) = **left {
                quote! {self.get_left_vreg(#prelude) }
            } else if let IRPattern::Const(..) = **left {
                prelude.clone()
            } else {
                quote! {self.get_left_index(#prelude)}
            };

            let mut left = emit_asm_arm(&*left, &left_prelude);
            if let Some(right) = right {
                let right_prelude = if let IRPattern::Reg(..) = **right {
                    quote! {self.get_right_vreg(#prelude) }
                } else if let IRPattern::Const(..) = **right {
                    prelude.clone()
                } else {
                    quote! {self.get_right_index(#prelude)}
                };
                let right = emit_asm_arm(&*right, &right_prelude);
                left.append_all(right);
            }
            left
        }

        IRPattern::NonTerm(name, nonterm) => {
            //let s = name.to_string();
            let nt_type = format_ident!("{}_NT", nonterm);
            quote! {
                let rule=self.instruction_states[#prelude as usize].rule[#nt_type];
                let #name=self.gen_asm2(#prelude as usize,rule,original_index);
            }
        }

        IRPattern::Reg(name, _) => {
            quote! {
                let #name=self.allocation[#prelude as usize][original_index].unwrap();
            }
        }

        IRPattern::Const(name) => {
            quote! {
                let #name=self.get_value(#prelude);
            }
        }
    }
}

fn emit_asm_format(pattern: &IRPattern) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
            left,
            right,
        } => {
            let mut left = emit_asm_format(&*left);
            if let Some(right) = right {
                let right = emit_asm_format(&*right);
                left.append_all(right);
            }
            left
        }

        IRPattern::NonTerm(name, _) | IRPattern::Const(name) | IRPattern::Reg(name, _) => {
            if let Some('_') = name.to_string().chars().next() {
                TokenStream::new()
            } else {
                quote! {,#name=#name}
            }
        }
    }
}

pub(super) fn emit_custom_print(program: &Program) -> TokenStream {
    let mut result = TokenStream::new();
    for definition in &program.definitions {
        let t = definition.custom_print;
        result.append_all(quote! {#t,});
    }
    result
}

fn has_used_result(definition: &Definition) -> bool {
    if let DefinitionType::Reg(..) = definition.name {
        if definition.template.value().contains("{res") {
            return true;
        }
    }
    false
}
