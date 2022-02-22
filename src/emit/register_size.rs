use std::borrow::Cow;

use crate::*;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, TokenStreamExt};

fn is_register<'a>(pattern: &'a IRPattern) -> Vec<String> {
    let mut result = Vec::new();
    is_register2(&mut result, pattern);
    result
}

fn is_register2<'a>(result: &mut Vec<String>, pattern: &'a IRPattern) {
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
            left,
            right,
        } => {
            is_register2(result, left);
            if let Some(right) = right {
                is_register2(result, right);
            }
        }

        IRPattern::Reg(name, _) => result.push(name.to_string()),

        IRPattern::NonTerm(..) | IRPattern::Const(..) => (),
    }
}

pub(super) fn modify_template(
    settings: &ProgramSettings,
    definition: &DefinitionType,
    pattern: &IRPattern,
    template: &str,
) -> (Vec<String>, String) {
    if settings.register_sizes.is_none() {
        return (Vec::new(), template.to_string());
    }

    let mut registers = is_register(pattern);
    if let DefinitionType::Reg(_) = definition {
        registers.push("res".to_string());
    }
    let mut used_registers = Vec::new();

    let iter = template.split(|c| matches!(c, '{' | '}'));
    let slices = iter
        .scan(false, |inside_block, s| {
            let mut result = Cow::from(s);

            if *inside_block && registers.contains(&s.to_string()) && !s.contains(":") {
                *result.to_mut() = format!("{}:.{}_width$", s, s);
                used_registers.push(s.to_string())
            }
            *inside_block = !*inside_block;
            return Some(result);
        })
        .collect::<Vec<_>>();

    let mut result = String::new();
    for (str, &seperator) in slices.iter().zip(['{', '}'].iter().cycle()) {
        result.push_str(str);
        result.push(seperator);
    }
    result.pop();

    (used_registers, result)
}

pub(super) fn emit_register_width_arm(
    pattern: &IRPattern,
    modified_registers: &Vec<String>,
    prelude: &TokenStream,
    register_prelude: &TokenStream,
) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
            left,
            right,
        } => {
            let left_prelude = quote! {self.get_left_index(#prelude)};
            let left_register = quote! {self.get_left_vreg(#prelude)};

            let mut left =
                emit_register_width_arm(&*left, modified_registers, &left_prelude, &left_register);
            if let Some(right) = right {
                let right_prelude = quote! {self.get_right_index(#prelude)};
                let right_register = quote! {self.get_right_vreg(#prelude)};
                let right = emit_register_width_arm(
                    &*right,
                    modified_registers,
                    &right_prelude,
                    &right_register,
                );
                left.append_all(right);
            }
            left
        }

        IRPattern::Reg(name, _) if modified_registers.contains(&name.to_string()) => {
            let name = format_ident!("{}_width", name);
            quote! {
                let #name=self.get_default_register_width(#prelude,#register_prelude);
            }
        }

        IRPattern::Reg(..) | IRPattern::NonTerm(..) | IRPattern::Const(..) => TokenStream::new(),
    }
}

pub(super) fn emit_register_width_format(modified_registers: &Vec<String>) -> TokenStream {
    modified_registers
        .iter()
        .collect::<HashSet<_>>()
        .iter()
        .map(|r| format_ident!("{}_width", r))
        .flat_map(|r| quote! {,#r=#r})
        .collect()
}

pub(super) fn emit_default_register_width(
    int_size: Option<NonZeroU32>,
    register_sizes: &Option<Vec<(Ident, i32)>>,
) -> TokenStream {
    if register_sizes.is_none() {
        return quote! {
        fn get_default_register_width(&self, index: u32, register: u32) -> usize { unreachable!()}};
    }

    let int_size: u32 = int_size.unwrap_or(NonZeroU32::new(32).unwrap()).into();
    let int_size = format_ident!("S{}", int_size);

    let register_sizes = register_sizes.as_ref().unwrap();
    let mut result = TokenStream::new();

    for (sizes, width) in register_sizes {
        let sizes = split_size(&sizes.to_string().to_uppercase());
        let width = *width as usize;
        for size in sizes {
            let size = format_ident!("{}", size);
            result.append_all(quote! {IRSize::#size => #width,})
        }
    }

    quote! {
        fn get_vreg_size(&self, index : u32, vreg : u32) -> IRSize {
            let instruction=&self.instructions[index as usize];
            if index == 0 {
                let arguments = &self.arguments.sizes;
                return arguments[vreg as usize];
            }
            else if let IRInstruction::Label(Some(phi),_) = instruction {
                for i in 0..phi.targets.len() {
                    if phi.targets[i] == vreg
                    {
                        return phi.size[i];
                    }
                }
                unreachable!();
            }
            instruction.get_result_size(IRSize::#int_size)
        }

        fn get_default_register_width(&self, index: u32, register: u32) -> usize {
            log::trace!("Get default register of vreg {} at {}",register,index);
            let size = get_vreg_size(index,register);
            get_default_register_width2(size)
        }
        fn get_default_register_width2(&self, size: IRSize) -> usize {
            let result = match size {
                #result
                _ => unreachable!(),
            };
            log::trace!("Default register size of {} is {}", size, result);
            result
        }
    }
}
