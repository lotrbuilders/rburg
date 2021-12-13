mod check;
mod emit;
mod fmt;
mod parse;

use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, TokenStreamExt};
use syn::{parse_macro_input, Block, Ident, LitStr};

use crate::check::Checkable;

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

fn get_default_size(ident: &Ident) -> proc_macro2::TokenStream {
    use quote::quote;
    let mut result = proc_macro2::TokenStream::new();
    let str = match &ident.to_string() as &str {
        "AddrL" | "Jmp" | "Label" => "P",

        "Imm" | "Load" | "Store" | "Add" | "Sub" | "Xor" | "Or" | "And" | "Eq" | "Ne" | "Lt"
        | "Le" | "Gt" | "Ge" | "Jcc" | "Jnc" => "I32",

        "Mul" | "Div" => "S32",

        _ => {
            use syn::Error;
            result.append_all(
                Error::new(ident.span(), "Unsupported default size for").to_compile_error(),
            );
            "unknown"
        }
    };
    let ident = format_ident!("{}", str);
    result.append_all(quote!(#ident));
    result
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
