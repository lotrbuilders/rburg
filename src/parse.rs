/*
This module parses the program description in the rburg-DSL given to the backend
<program> : <definition>+
<definition>
    (%reg|non-term|<empty>): <tree> template (#|?)* {<rust-code>}
<tree>:
    | <term> [<size>] [ '(' <tree> [ , <tree> ] ')' ]
    | <name> <non-term>
    | <name> '%'<reg>
    | '#' <name>
*/
use crate::*;
use proc_macro2::Span;
use std::collections::HashMap;
use syn::parenthesized;
use syn::parse::{Parse, ParseStream};
use syn::Block;
use syn::{Ident, Result, Token};

impl Parse for Program {
    fn parse(input: ParseStream) -> Result<Self> {
        let implements = input.parse()?;
        input.parse::<Token![,]>()?;
        let mut definitions = Vec::<Definition>::new();
        let mut span = Vec::<Span>::new();
        while !input.is_empty() {
            span.push(input.span());
            definitions.push(input.parse::<Definition>()?);
        }

        let mut terminals = HashMap::<String, Vec<u16>>::new();
        for (definition, i) in definitions.iter().zip(0u16..) {
            match &definition.pattern {
                IRPattern::Node { term, .. } => {
                    let term = term.to_string();
                    if !terminals.contains_key(&term) {
                        terminals.insert(term.clone(), vec![i]);
                    } else {
                        terminals.get_mut(&term).unwrap().push(i);
                    }
                }
                _ => (),
            }
        }

        let mut nt_equivelances = HashMap::<String, Vec<u16>>::new();
        for (definition, i) in definitions.iter().zip(0u16..) {
            //if let DefinitionType::NonTerm(name) = &definition.name {
            match &definition.pattern {
                IRPattern::NonTerm(_name, nonterm) => {
                    let nonterm = nonterm.to_string();
                    if !nt_equivelances.contains_key(&nonterm) {
                        nt_equivelances.insert(nonterm.clone(), vec![i]);
                    } else {
                        nt_equivelances.get_mut(&nonterm).unwrap().push(i);
                    }
                }
                /*IRPattern::Reg(..) => {
                    let nonterm = "reg".to_string();
                    if !nt_equivelances.contains_key(&nonterm) {
                        nt_equivelances.insert(nonterm.clone(), vec![i]);
                    } else {
                        nt_equivelances.get_mut(&nonterm).unwrap().push(i);
                    }
                }*/
                _ => (),
            }
            //}
        }

        let non_terminals = definitions
            .iter()
            .filter_map(|def| match &def.name {
                DefinitionType::NonTerm(name) => Some(name.to_string()),
                _ => None,
            })
            .collect();

        Ok(Program {
            implements,
            definitions,
            span,
            terminals,
            non_terminals,
            nt_equivelances,
        })
    }
}

impl Parse for Definition {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        let pattern = input.parse()?;
        let mut two_address = false;
        let mut custom_print = false;
        loop {
            if input.peek(Token![?]) {
                input.parse::<Token![?]>().expect("msg");
                two_address = true;
            } else if input.peek(Token![#]) {
                input.parse::<Token![#]>().expect("msg");
                custom_print = true;
            } else {
                break;
            }
        }

        let template = input.parse()?;
        let rust_code = match input.parse::<Block>() {
            Ok(block) => Some(block),
            Err(_) => None,
        };

        Ok(Definition {
            name,
            pattern,
            template,
            rust_code,
            two_address,
            custom_print,
        })
    }
}

impl Parse for DefinitionType {
    fn parse(input: ParseStream) -> Result<Self> {
        let result = if input.peek(Token![%]) {
            input.parse::<Token![%]>()?;
            Ok(DefinitionType::Reg(input.parse()?))
        } else if input.peek(Ident) {
            Ok(DefinitionType::NonTerm(input.parse()?))
        } else {
            Ok(DefinitionType::Stmt)
        };
        input.parse::<Token![:]>()?;
        result
    }
}

impl Parse for IRPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![#]) {
            input.parse::<Token![#]>()?;
            let name = input.parse::<Ident>()?;
            return Ok(IRPattern::Const(name));
        }
        let term = input.parse::<Ident>()?;

        match term.to_string().chars().next().unwrap() {
            // If it starts with a capital letter it must be a terminal
            'A'..='Z' => {
                let size = if input.peek(Ident) {
                    let ident = input.parse::<Ident>()?;
                    let ident = Ident::new(&ident.to_string().to_uppercase(), ident.span());
                    Some(ident)
                } else {
                    None
                };

                let content;
                parenthesized!(content in input);
                let left = Box::new(content.parse::<IRPattern>()?);
                let right = if content.peek(Token![,]) {
                    content.parse::<Token![,]>()?;
                    Some(Box::new(content.parse::<IRPattern>()?))
                } else {
                    None
                };
                Ok(IRPattern::Node {
                    term,
                    size,
                    left,
                    right,
                })
            }
            _ => {
                let name = term;
                if input.peek(Token![%]) {
                    input.parse::<Token![%]>()?;
                    Ok(IRPattern::Reg(name, input.parse()?))
                } else {
                    Ok(IRPattern::NonTerm(name, input.parse()?))
                }
            }
        }
    }
}
