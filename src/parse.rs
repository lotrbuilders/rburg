/*
This module parses the program description in the rburg-DSL given to the backend
<program> : <definition>+
<definition> : ( <ins_definition> )
<ins_definition>: define_ins name [<ir-pattern>] string
                | define_ins name [<ir-pattern>] {rust code} string

<ir-pattern>    : (set (name |%name|) <ir-pattern>)
                | (operator (name |%name|) (name |%name|))
                | ()

<definition>
    (%reg|!non-term!|<empty>): <tree> template {<rust-code>}
<tree>:
    | <term> [<size>] [ '(' <tree> [ , <tree> ] ')' ]
    | !<name> <non-term>!
    | <name> '%'<reg>
    | '#' <name>

*/
use crate::*;
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
        while !input.is_empty() {
            definitions.push(input.parse::<Definition>()?);
        }

        let mut terminals = HashMap::<String, Vec<u16>>::new();
        for i in 0..definitions.len() {
            let definition = &definitions[i];
            match &definition.pattern {
                IRPattern::Node { term, .. } => {
                    let term = term.to_string();
                    if !terminals.contains_key(&term) {
                        terminals.insert(term.clone(), vec![i as u16]);
                    } else {
                        terminals.get_mut(&term).unwrap().push(i as u16);
                    }
                }
                _ => (),
            }
        }

        Ok(Program {
            implements,
            definitions,
            terminals,
        })
    }
}

impl Parse for Definition {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        let pattern = input.parse()?;
        let two_address = input.peek(Token![?]);
        if two_address {
            input.parse::<Token![?]>()?;
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
        })
    }
}

impl Parse for DefinitionType {
    fn parse(input: ParseStream) -> Result<Self> {
        let result = if input.peek(Token![%]) {
            input.parse::<Token![%]>()?;
            Ok(DefinitionType::Reg(input.parse()?))
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
                    Err(input.error("Expected # or %reg"))
                }
            }
        }
    }
}
