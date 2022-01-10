mod asm;
mod label;
mod reduce;
mod register;
mod register_size;

use crate::*;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, TokenStreamExt};

// The main function to emit all seperate stage
// The main stages are
// - Emitting the state with the correct parameter
// - Emitting the labelizer
// - Emit the code needed to find all the children corresponding to an instruction and rule
// - Emit the code needed to find all the virtual registers used by an instruction and rule
// - Emit the code to get the new non-terminals when reducing
// - Emit the assembly producing stage
// - Emitting the backend itself
pub(super) fn emit(program: Program) -> TokenStream {
    let mut _result = TokenStream::new();
    let state = emit_state(&program.non_terminals);
    let label = self::label::emit_label(&program);
    let equivelances = self::label::emit_label_equivelances(&program);
    let child = self::reduce::emit_get_child(&program);
    let vreg = self::register::emit_get_vregisters(&program);
    let non_terminals = self::reduce::emit_get_non_terminals(&program);
    let reduce_terminals = self::reduce::emit_reduce_terminals(&program);
    let assembly = self::asm::emit_asm(&program);
    let two_address = self::register::emit_two_address(&program);
    let is_instruction = emit_is_instruction(&program);
    let custom_print = self::asm::emit_custom_print(&program);

    let rule_count = program.definitions.len();
    let backend_name = program.settings.implements;

    quote!(
        pub struct #backend_name {
            function_name: String,
            function_names:HashSet<String>,
            instructions: Vec<IRInstruction>,
            definition_index: Vec<u32>,
            use_count: Vec<u32>,
            arguments: IRArguments,

            instruction_states:Vec<State>,
            rules : Vec<u16>,

            allocation: Vec<RegisterAllocation<Register>>,
            reg_relocations: Vec<Vec<RegisterRelocation<Register>>>,

            local_offsets: Vec<i32>,
            stack_size: i32,

            non_terminals: [Vec<usize>;#rule_count],
            custom_print: [bool;#rule_count],
        }
        #state
        impl #backend_name {
            #label
            #equivelances
            #child
            #vreg
            #reduce_terminals
            #assembly
            #is_instruction

            pub fn new() -> #backend_name {
                #backend_name {
                    function_name: String::new(),
                    function_names: HashSet::new(),
                    instructions: Vec::new(),
                    definition_index: Vec::new(),
                    use_count: Vec::new(),
                    arguments: IRArguments{sizes:Vec::new(),arguments:Vec::new(),count:0},

                    instruction_states: Vec::new(),
                    rules: Vec::new(),

                    allocation: Vec::new(),
                    reg_relocations: Vec::new(),

                    local_offsets: Vec::new(),
                    stack_size: 0,

                    non_terminals: #non_terminals,
                    custom_print: [#custom_print],
                }
            }
        }
        #two_address

    )
}

fn emit_state(non_terminals: &HashSet<String>) -> TokenStream {
    let length = non_terminals.len() + 2;
    let non_terminals = non_terminals
        .iter()
        .map(|s| format_ident!("{}_NT", s))
        .zip(2usize..)
        .map(|(id, i)| quote! {const #id:usize=#i;})
        .flatten()
        .collect::<TokenStream>();

    quote!(
        const stmt_NT:usize=0;
        const reg_NT:usize=1;
        #non_terminals

        #[derive(Clone)]
        struct State {
            cost: [u16; #length],
            rule: [u16; #length],
            labeled: bool,
        }

        impl State {
            fn new()->State {
                State{
                    cost:[0x7fff;#length],
                    rule:[0xffff;#length],
                    labeled: false,
                }
            }
        }

        impl std::fmt::Display for State {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                if self.labeled {
                    write!(f,"labeled: ")?;
                }
                else {
                    write!(f,"unlabeled: ")?;
                }
                write!(f,"cost{:?} : rule{:?} ",self.cost,self.rule)?;
                Ok(())
            }
        }
    )
}

fn emit_is_instruction(program: &Program) -> TokenStream {
    let mut result = TokenStream::new();
    for (definition, rule) in program.definitions.iter().zip(0u16..) {
        if let DefinitionType::NonTerm(_) = &definition.name {
            result.append_all(quote! {|#rule});
        }
    }

    quote! {
        fn is_instruction(&self, rule: u16) -> bool {
            match rule {
                0xffff
                | 0xfffe
                #result
                 => false,
                _ => true,
            }
        }
    }
}
