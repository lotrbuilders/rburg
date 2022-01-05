mod check;
mod emit;
mod fmt;
mod parse;

use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, TokenStreamExt};
use syn::{parse_macro_input, Block, Ident, LitStr};

struct Program {
    implements: Ident,
    definitions: Vec<Definition>,
    span: Vec<Span>,
    terminals: HashMap<String, Vec<u16>>,
    nt_equivelances: HashMap<String, Vec<u16>>,
    non_terminals: HashSet<String>,
}

impl Program {
    fn get_nt(&self, index: u16) -> Ident {
        match &self.definitions[index as usize].name {
            DefinitionType::NonTerm(ident) => format_ident!("{}_NT", ident),
            DefinitionType::Reg(_) => format_ident!("reg_NT"),
            DefinitionType::Stmt => format_ident!("stmt_NT"),
        }
    }
}

struct Definition {
    name: DefinitionType,
    pattern: IRPattern,
    template: LitStr,
    rust_code: Option<Block>,
    two_address: bool,
    custom_print: bool,
}

enum DefinitionType {
    Reg(Ident),
    NonTerm(Ident),
    Stmt,
}

impl Definition {
    fn get_equivelance(&self) -> Ident {
        match &self.name {
            DefinitionType::NonTerm(ident) => format_ident!("label_equivelance_{}", ident),
            DefinitionType::Reg(_) => format_ident!("label_equivelance_reg"),
            DefinitionType::Stmt => format_ident!("label_equivelance_stmt"),
        }
    }
}

enum IRPattern {
    Node {
        term: Ident,
        size: Option<Ident>,
        left: Box<IRPattern>,
        right: Option<Box<IRPattern>>,
    },
    Reg(Ident, Ident),
    NonTerm(Ident, Ident),
    Const(Ident),
}

fn get_default_size(ident: &Ident) -> String {
    String::from(match &ident.to_string() as &str {
        "AddrL" | "AddrG" | "Jmp" | "Label" | "Cvp" => "P",

        "Imm" | "Load" | "Store" | "Add" | "Sub" | "Xor" | "Or" | "And" | "Eq" | "Ne" | "Lt"
        | "Le" | "Gt" | "Ge" | "Jcc" | "Jnc" | "Arg" => "I32",

        "Mul" | "Div" | "Call" | "CallV" => "S32",

        _ => "unknown",
    })
}

fn split_size(size: &String) -> Vec<String> {
    let mut result = Vec::new();
    let mut string = String::new();
    for ch in size.chars() {
        if ch.is_ascii_alphabetic() && !string.is_empty() {
            process_size(&mut result, &mut string)
        }
        string.push(ch);
    }
    process_size(&mut result, &mut string);
    result
}

fn process_size(result: &mut Vec<String>, string: &mut String) {
    let temp: String = string.drain(0..).collect();
    if temp.starts_with('I') {
        result.push(temp.replace('I', "S"));
    } else {
        result.push(temp);
    }
}

// This macro generates the entire backend given in the rburg-DSL
#[proc_macro]
pub fn rburg_main(input: TokenStream) -> TokenStream {
    let program = parse_macro_input!(input as Program);
    //println!("{}", program);
    let mut result = if let Err(err) = program.check(&Span::call_site()) {
        err
    } else {
        proc_macro2::TokenStream::new()
    };

    result.append_all(emit::emit(program));
    result.into()
}
