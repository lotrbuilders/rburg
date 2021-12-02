mod emit;
mod fmt;
mod parse;

use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::format_ident;
use syn::{parse_macro_input, Block, Ident, LitStr};

struct Program {
    implements: Ident,
    definitions: Vec<Definition>,
    terminals: HashMap<String, Vec<u16>>,
}

impl Program {
    fn get_nt(&self, index: u16) -> Ident {
        match self.definitions[index as usize].name {
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
    Stmt,
}

enum IRPattern {
    Node {
        term: Ident,
        left: Box<IRPattern>,
        right: Option<Box<IRPattern>>,
    },
    Reg(Ident, Ident),
    //NonTerm(Ident, Ident),
    Const(Ident),
}

// This macro generates the entire backend given in the rburg-DSL
#[proc_macro]
pub fn rburg_main(input: TokenStream) -> TokenStream {
    let program = parse_macro_input!(input as Program);
    println!("{}", program);
    emit::emit(program).into()
}
