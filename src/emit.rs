use crate::*;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};

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
    let state = emit_state();
    let label = emit_label(&program);
    let child = emit_get_child(&program);
    let vreg = emit_get_vregisters(&program);
    let non_terminals = emit_get_non_terminals(&program);
    let assembly = emit_asm(&program);
    let rule_count = program.definitions.len();
    let backend_name = program.implements;

    quote!(
        pub struct #backend_name {
            function_name: String,
            instructions: Vec<IRInstruction>,
            definition_index: Vec<u32>,
            instruction_states:Vec<State>,
            rules : Vec<u16>,
            non_terminals: [Vec<usize>;#rule_count],
            vreg2reg: Vec<Register>,
            reg_relocations: Vec<Vec<RegisterRelocation>>,
        }
        #state
        impl #backend_name {
            #label
            #child
            #vreg
            #assembly

            pub fn new() -> #backend_name {
                #backend_name {
                    function_name: String::new(),
                    instructions: Vec::new(),
                    definition_index: Vec::new(),
                    instruction_states: Vec::new(),
                    rules: Vec::new(),
                    non_terminals: #non_terminals,
                    vreg2reg: Vec::new(),
                    reg_relocations: Vec::new(),
                }
            }
        }
    )
}

fn emit_state() -> TokenStream {
    quote!(
        const stmt_NT:usize=1;
        const reg_NT:usize=2;

        #[derive(Clone)]
        struct State {
            cost: [u16; 3],
            rule: [u16; 3],
            labeled: bool,
        }

        impl State {
            fn new()->State {
                State{
                    cost:[0x7fff,0x7fff,0x7fff],
                    rule:[0xffff,0xffff,0xffff],
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

fn emit_label(program: &Program) -> TokenStream {
    let _backend_name = &program.implements;

    let match_arms = emit_label_arms(program);

    // Initialization of states should already have been done at this point
    quote!(
        pub fn label(&mut self, index: u32) -> () {
            //log::trace!("Labeling instruction {}",index);
            let state = State::new();
            let ins = self.instructions[index as usize].clone();
            if self.instruction_states[index as usize].labeled
            {
                //log::trace!("Instruction {} already labeled",index);
                return ();
            }
            self.instruction_states[index as usize].labeled=true;
            match(ins.to_type())
            {
                #match_arms

                _ => {
                    log::error!("Bad/Unsupported terminal in the instruction selection");
                },
            }
        }

        fn try_label_left(&mut self, ins: &IRInstruction) -> () {
            //log::trace!("Trying to label left node");
            if let Some(index) = ins.get_left()
            {
                self.label(self.definition_index[ index as usize]);
            }
        }

        fn try_label_right(&mut self, ins: &IRInstruction) -> () {
            //log::trace!("Trying to label right node");
            if let Some(index) = ins.get_right()
            {
                self.label(self.definition_index[ index as usize]);
            }
        }

        fn get_left_index(&self,index:u32) -> u32 {
            self.definition_index[self.instructions[index as usize].get_left().unwrap() as usize]
        }

        fn get_right_index(&self,index:u32) -> u32 {
            self.definition_index[self.instructions[index as usize].get_right().unwrap() as usize]
        }

        fn get_left_vreg(&self,index:u32) -> u32 {
            self.instructions[index as usize].get_left().unwrap()
        }

        fn get_right_vreg(&self,index:u32) -> u32 {
            self.instructions[index as usize].get_right().unwrap()
        }

        pub fn to_string(&self) -> String {
            let mut result=String::new();
            for state in &self.instruction_states
            {
                result.push_str(&format!("{}\n",state));
            }
            result
        }
    )
}

fn emit_label_arms(program: &Program) -> TokenStream {
    //println!("emit label arms");
    let mut arms = TokenStream::new();
    for (terminal, indeces) in &program.terminals {
        arms.append_all(emit_label_arm(program, terminal, indeces));
    }

    arms
}

#[allow(unused_variables)]
fn emit_label_arm(program: &Program, terminal: &String, indeces: &Vec<u16>) -> TokenStream {
    //println!("emit label arm {:?}", indeces);
    let mut tokens = TokenStream::new();
    for i in indeces {
        tokens.append_all(emit_label_pattern(program, *i));
    }

    let string = terminal;
    let terminal = format_ident!("{}", terminal);
    quote! {
        IRType::#terminal => {
            //log::trace!("Labelling {} node",#string);
            self.try_label_left(&ins);
            self.try_label_right(&ins);
            #tokens
        }
    }
}

fn emit_label_pattern(program: &Program, index: u16) -> TokenStream {
    //println!("emit label pattern {}", index);
    let definition = &program.definitions[index as usize];
    let pattern = &definition.pattern;
    let mut tokens = TokenStream::new();
    if let IRPattern::Node {
        term: _,
        left: _,
        right: _,
    } = pattern
    {
        let child_cost = emit_label_pattern_cost(pattern, &quote! {index});
        let cost = if let Some(code) = &definition.rust_code {
            code.to_token_stream()
        } else {
            quote! {0}
        };
        tokens = quote! {let cost = #child_cost #cost;};

        let nt_type = program.get_nt(index);

        tokens.append_all(quote! {
            {
                let state= & mut self.instruction_states[index as usize];
                if cost<state.cost[#nt_type] {
                    state.cost[#nt_type]=cost;
                    state.rule[#nt_type]=#index;
                }
            }
        });
    } else {
        println!("Error: Unallowed IR Pattern");
    }
    //println!("tokens:{}", tokens.to_string());
    tokens
}

fn _emit_label_pattern_condition(_pattern: &IRPattern, _prelude: &TokenStream) -> TokenStream {
    TokenStream::new()
}

fn emit_label_pattern_cost(pattern: &IRPattern, prelude: &TokenStream) -> TokenStream {
    print!("emit label pattern cost");
    match pattern {
        IRPattern::Node {
            term: _,
            left,
            right,
        } => {
            //println!("Node");
            let left_prelude = quote! {self.get_left_index(#prelude) };
            let mut left = emit_label_pattern_cost(&*left, &left_prelude);
            //println!("left: {}", left.to_string());
            if let Some(right) = right {
                let right_prelude = quote! {self.get_right_index(#prelude) };
                let right = emit_label_pattern_cost(&*right, &right_prelude);
                left.append_all(right)
            }
            left
        }
        IRPattern::Reg(_, _) => {
            //println!("Reg");
            quote! {
                self.instruction_states[#prelude as usize].cost[reg_NT]+
            }
        }
        IRPattern::Const(_) => {
            //println!("Node");
            TokenStream::new()
        }
    }
}

fn emit_get_child(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_get_child_arm(&program.definitions[i].pattern, &quote! {index});
        let i = i as u16;
        arms.append_all(quote! {
            #i => {let mut temp=vec![#arm 0];temp.pop(); temp},
        })
    }
    quote! {
        fn get_kids(&self,index:u32,rule_number:u16) -> Vec<u32>
        {
            match rule_number {
                #arms
                _ => {
                    log::error!("Rule {} does not exist(index={})",rule_number,index),
                    Vec::new()
                }
            }
        }

        fn get_child_non_terminals(&self,rule_number:u16) -> Vec<usize>
        {
            if rule_number==0xffff{
                log::error!("Unallowed rule number when getting the non terminal of the children")
                return Vec::new();
            }
            self.non_terminals[rule_number as usize].clone()
        }
    }
}

fn emit_get_child_arm(pattern: &IRPattern, prelude: &TokenStream) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
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
        IRPattern::Reg(_, _) => quote! {
            #prelude,
        },

        IRPattern::Const(_) => TokenStream::new(),
    }
}

fn emit_get_non_terminals(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_get_non_terminals_arm(&program.definitions[i].pattern);
        let i = i as u16;
        arms.append_all(quote! {
            {let mut temp=vec![#arm 0];temp.pop();temp}
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
        IRPattern::Reg(_, _) => quote! {
            reg_NT,
        },
        IRPattern::Const(_) => TokenStream::new(),
    }
}

fn emit_get_vregisters(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_get_vregisters_arm(&program.definitions[i].pattern, &quote! {index});
        let i = i as u16;
        arms.append_all(quote! {
            #i => {let mut temp=vec![#arm (0,& REG_CLASS_EMPTY)];temp.pop();temp}
        });
    }
    quote! {

        fn get_vregisters(&self,index:u32,rule:u16) -> (Vec<(u32,&'static[bool;REG_COUNT])>,Option<u32>)
        {
            let used_vregs: Vec<(u32,&'static[bool;REG_COUNT])>=match rule
            {
                #arms
                _ => {
                    log::error!("Unsupporteded rule {}",rule);
                    Vec::new()
                }
            };
            let result_vreg=self.instructions[index as usize].get_result();
            (used_vregs,result_vreg)
        }
    }
}

fn emit_get_vregisters_arm(pattern: &IRPattern, prelude: &TokenStream) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
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
        IRPattern::Reg(_, class) => {
            let class = format_ident!("REG_CLASS_{}", class.to_string().to_uppercase());
            quote! {
                (#prelude,& #class),
            }
        }

        IRPattern::Const(_) => TokenStream::new(),
    }
}

fn has_used_result(definition: &Definition) -> bool {
    if let DefinitionType::Reg(..) = definition.name {
        if definition.template.value().contains("{res}") {
            return true;
        }
    }
    false
}

fn emit_asm(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();

    for i in 0..program.definitions.len() {
        let definition = &program.definitions[i];
        // Does not currently handle # comments in the code. These might have to be removed later at some stage
        // At the moment all these commments are handled by the handwritten portion.
        let template = format!("\t{}", definition.template.value());
        let arm = emit_asm_arm(&definition.pattern, &quote! {index as u32});
        let format_arm = emit_asm_format(&definition.pattern);
        let i = i as u16;
        if has_used_result(definition) {
            arms.append_all(quote! {
                #i => {
                    let res=self.vreg2reg[instruction.get_result().unwrap() as usize].to_string();
                    #arm
                    format!(#template,res=res #format_arm)
                }
            });
        } else {
            arms.append_all(quote! {
                #i => {
                    #arm
                    format!(#template #format_arm)
                }
            });
        }
    }

    quote! {
        fn gen_asm(&self,index:usize) -> String
        {
            let instruction=&self.instructions[index];
            let rule=self.rules[index];
            match rule {
                #arms
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
        IRPattern::Reg(name, _) => {
            quote! {
                let #name=self.vreg2reg[#prelude as usize].to_string();
            }
        }

        IRPattern::Const(name) => {
            quote! {
                let #name=self.instructions[#prelude as usize].get_value();
            }
        }
    }
}

fn emit_asm_format(pattern: &IRPattern) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
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
        IRPattern::Reg(name, _) => {
            if let Some('_') = name.to_string().chars().next() {
                TokenStream::new()
            } else {
                quote! {,#name=#name}
            }
        }

        IRPattern::Const(name) => {
            if let Some('_') = name.to_string().chars().next() {
                TokenStream::new()
            } else {
                quote! {,#name=#name}
            }
        }
    }
}
