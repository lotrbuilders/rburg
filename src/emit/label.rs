use crate::*;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};

pub(super) fn emit_label(program: &Program) -> TokenStream {
    let match_arms = emit_label_arms(program);

    // Initialization of states should already have been done at this point
    quote!(
        pub fn label(&mut self, index: u32) -> () {
            //log::trace!("Labeling instruction {}",index);
            let state = State::new();
            let ins = self.instructions[index as usize].clone();
            let size = ins.get_size();
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
                    log::debug!("{} is not found as a base terminal",ins.to_type());
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
            //log::trace!("Get left index of {}",index);
            self.definition_index[self.instructions[index as usize].get_left().unwrap() as usize]
        }

        fn get_right_index(&self,index:u32) -> u32 {
            //log::trace!("Get right index of {}",index);
            self.definition_index[self.instructions[index as usize].get_right().unwrap() as usize]
        }

        fn get_left_vreg(&self,index:u32) -> u32 {
            self.instructions[index as usize].get_left().unwrap()
        }

        fn get_right_vreg(&self,index:u32) -> u32 {
            self.instructions[index as usize].get_right().unwrap()
        }

        fn get_vreg_use_count(&self,index:u32) -> u32 {
            self.instructions[index as usize].get_result().map(|res| self.use_count[res as usize]).unwrap_or(0)
        }

        // Get the value of immediate instructions in string form
        fn get_value(&self, instruction:u32) -> String {
            let instruction = &self.instructions[instruction as usize];
            match instruction {
                IRInstruction::Imm(_, _, value) => format!("{}", value),
                &IRInstruction::AddrL(_,_,i) => format!("{}", self.local_offsets[i]),
                IRInstruction::AddrG(_,_,name) => format!("{}", name),
                IRInstruction::Jcc(..,i) => format!("{}", i),
                IRInstruction::Jnc(..,i) => format!("{}", i),
                IRInstruction::Jmp(i) => format!("{}", i),
                IRInstruction::Label(_,i) => format!("{}", i),
                IRInstruction::Call(..,name,_) => format!("{}", name),
                IRInstruction::Phi(phi) => format!("{}",phi),
                IRInstruction::PhiSrc(label) => format!("phisrc L{}",label),
                IRInstruction::Nop => format!("nop"),
                _ => {
                    log::error!("Get value called without value");
                    format!("")
                }
            }
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
        size: _,
        left: _,
        right: _,
    } = pattern
    {
        let condition = emit_label_pattern_condition(pattern, &quote! {index}, true);
        let child_cost = emit_label_pattern_cost(pattern, &quote! {index});
        let cost = if let Some(code) = &definition.rust_code {
            code.to_token_stream()
        } else if let DefinitionType::Reg(..) | DefinitionType::Stmt = definition.name {
            quote! {1}
        } else {
            quote! {0}
        };
        //tokens = quote! {};

        let nt_type = program.get_nt(index);
        let nt_equivelance = definition.get_equivelance();

        tokens.append_all(quote! {
            if #condition true {
                let cost = #child_cost #cost;
                let state = &mut self.instruction_states[index as usize];
                if cost<state.cost[#nt_type] {
                    state.cost[#nt_type]=cost;
                    state.rule[#nt_type]=#index;
                    self.#nt_equivelance(index as usize,cost);
                }
            }
        });
    } else {
        println!("Error: Unallowed IR Pattern");
    }
    //println!("tokens:{}", tokens.to_string());
    tokens
}

fn emit_label_pattern_condition(
    pattern: &IRPattern,
    prelude: &TokenStream,
    root: bool,
) -> TokenStream {
    //print!("emit label pattern cost");
    match pattern {
        IRPattern::Node {
            term,
            size,
            left,
            right,
        } => {
            //println!("Node");
            let span = size
                .as_ref()
                .map(|s| s.span())
                .unwrap_or_else(|| term.span());

            let size = size
                .as_ref()
                .map(|id| id.to_string())
                .unwrap_or_else(|| get_default_size(term));

            let ir_sizes = split_size(&size.to_uppercase());
            let mut check_size = TokenStream::new();
            for size in ir_sizes {
                let size = format_ident!("{}", size, span = span);
                check_size.append_all(
                    quote! {self.instructions[#prelude as usize].get_size()==IRSize::#size||},
                )
            }
            let check_size = quote! {
                (#check_size false)
            };

            let left_prelude = quote! {self.get_left_index(#prelude) };
            let mut left = emit_label_pattern_condition(&*left, &left_prelude, false);
            //println!("left: {}", left.to_string());
            if let Some(right) = right {
                let right_prelude = quote! {self.get_right_index(#prelude) };
                let right = emit_label_pattern_condition(&*right, &right_prelude, false);
                left.append_all(right)
            }

            let check_use_count = if !root {
                quote! {&& self.get_vreg_use_count(#prelude) <= 1}
            } else {
                TokenStream::new()
            };
            quote! {
                self.instructions[#prelude as usize].to_type()==IRType::#term
                && #check_size
                #check_use_count
                && #left
            }
        }
        IRPattern::NonTerm(..) => {
            //println!("NonTerm");
            TokenStream::new()
        }
        IRPattern::Reg(_, _) => {
            //println!("Reg");
            TokenStream::new()
        }
        IRPattern::Const(_) => {
            //println!("Node");
            TokenStream::new()
        }
    }
}

fn emit_label_pattern_cost(pattern: &IRPattern, prelude: &TokenStream) -> TokenStream {
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
            let mut left = emit_label_pattern_cost(&*left, &left_prelude);
            //println!("left: {}", left.to_string());
            if let Some(right) = right {
                let right_prelude = quote! {self.get_right_index(#prelude) };
                let right = emit_label_pattern_cost(&*right, &right_prelude);
                left.append_all(right)
            }
            left
        }
        IRPattern::NonTerm(_, nonterm) => {
            let nt_type = format_ident!("{}_NT", nonterm);
            quote! {
                self.instruction_states[#prelude as usize].cost[#nt_type]+
            }
        }
        IRPattern::Reg(_, _) => {
            quote! {
                self.instruction_states[#prelude as usize].cost[reg_NT]+
            }
        }
        IRPattern::Const(_) => TokenStream::new(),
    }
}

pub(super) fn emit_label_equivelances(program: &Program) -> TokenStream {
    let mut result = TokenStream::new();
    for non_terminal in program
        .non_terminals
        .iter()
        .chain(vec![/*"reg".to_string(),*/ "stmt".to_string()].iter())
    {
        let empty = Vec::<u16>::new();
        let equivelances = program
            .nt_equivelances
            .get(non_terminal)
            .unwrap_or_else(|| &empty);

        let arms = equivelances
            .iter()
            .map(|i| &program.definitions[*i as usize])
            .zip(equivelances.iter())
            .map(|(definition, i)| emit_label_equivelances_arm(definition, *i))
            .flatten()
            .collect::<TokenStream>();

        let ident = format_ident!("label_equivelance_{}", non_terminal);
        result.append_all(quote! {
            fn #ident(&mut self,index:usize,cost:u16)
            {
                #arms
            }
        });
    }

    //Special case to allow for register producing instructions to always be seen as statements
    result.append_all(quote! {
        fn label_equivelance_reg(&mut self,index:usize,cost:u16)
        {
            let  state = &mut self.instruction_states[index];
            let cost = cost + 1000;
            if(cost<state.cost[stmt_NT] )
            {
                state.cost[stmt_NT]=cost;
                state.rule[stmt_NT]=state.rule[reg_NT];
            }
        }
    });
    result
}

fn emit_label_equivelances_arm(definition: &Definition, index: u16) -> TokenStream {
    let nonterm = match &definition.name {
        DefinitionType::NonTerm(nonterm) => nonterm.to_string(),
        DefinitionType::Reg(..) => "reg".to_string(),
        DefinitionType::Stmt => "stmt".to_string(),
        //_ => unreachable!(),
    };

    let cost = definition
        .rust_code
        .as_ref()
        .map(|c| c.to_token_stream())
        .unwrap_or_else(|| quote! {0});

    let rule = format_ident!("{}_NT", nonterm);
    let equivelance = format_ident!("label_equivelance_{}", nonterm);

    quote! {
            {
            let  state = &mut self.instruction_states[index];
            let cost = cost + #cost;
            if(cost<state.cost[#rule])
            {
                state.cost[#rule]=cost;
                state.rule[#rule]=#index;
                self.#equivelance(index,cost);
            }
        }
    }
}
