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
    let state = emit_state(&program.non_terminals);
    let label = emit_label(&program);
    let equivelances = emit_label_equivelances(&program);
    let child = emit_get_child(&program);
    let vreg = emit_get_vregisters(&program);
    let non_terminals = emit_get_non_terminals(&program);
    let reduce_terminals = emit_reduce_terminals(&program);
    let assembly = emit_asm(&program);
    let two_address = emit_two_address(&program);
    let is_instruction = emit_is_instruction(&program);
    let rule_count = program.definitions.len();
    let backend_name = program.implements;

    quote!(
        pub struct #backend_name {
            function_name: String,
            instructions: Vec<IRInstruction>,
            definition_index: Vec<u32>,
            use_count: Vec<u32>,
            arguments: IRArguments,

            instruction_states:Vec<State>,
            rules : Vec<u16>,
            non_terminals: [Vec<usize>;#rule_count],

            allocation: Vec<RegisterAllocation>,
            reg_relocations: Vec<Vec<RegisterRelocation>>,

            local_offsets: Vec<i32>,
            stack_size: i32,
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
                    instructions: Vec::new(),
                    definition_index: Vec::new(),
                    use_count: Vec::new(),
                    arguments: IRArguments{sizes:Vec::new(),arguments:Vec::new(),count:0},

                    instruction_states: Vec::new(),
                    rules: Vec::new(),
                    non_terminals: #non_terminals,

                    allocation: Vec::new(),
                    reg_relocations: Vec::new(),

                    local_offsets: Vec::new(),
                    stack_size: 0,
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

fn emit_label(program: &Program) -> TokenStream {
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
            //println!("Reg");
            let nt_type = format_ident!("{}_NT", nonterm);
            quote! {
                self.instruction_states[#prelude as usize].cost[#nt_type]+
            }
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

fn emit_label_equivelances(program: &Program) -> TokenStream {
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

fn emit_get_child(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_get_child_arm(&program.definitions[i].pattern, &quote! {index});
        let i = i as u16;
        arms.append_all(quote! {
            #i => {vec![#arm]},
        })
    }
    quote! {
        fn get_kids(&self,index:u32,rule_number:u16) -> Vec<u32>
        {
            //log::trace!("Get kids of {} with rule {}",index,rule_number);
            if let IRInstruction::Label(Some(phi),_) = &self.instructions[index as usize] {
                let result=phi.sources.iter().
                    flat_map(|src| src.iter()).
                    map(|r| self.definition_index[*r as usize]).
                    collect();
                log::debug!("Get kids of phi node {}, {:?}",index,result);
                return result
            }
            else if let IRInstruction::Call(..,arguments) = &self.instructions[index as usize] {
                let result=arguments.arguments.iter()
                    .map(|r| r.unwrap())
                    .map(|r| self.definition_index[r as usize])
                    .collect();
                log::debug!("Get kids of call {}, {:?}",index,result);
                return result
            }
            match rule_number {
                #arms
                _ => {
                    log::error!("Rule {} does not exist(index={})",rule_number,index);
                    Vec::new()
                }
            }
        }

        fn get_child_non_terminals(&self,index:u32,rule_number:u16) -> Vec<usize>
        {
            if let IRInstruction::Label(Some(phi),_) = &self.instructions[index as usize] {
                let length=phi.sources.len()*phi.sources.get(0).map(|v| v.len()).unwrap_or(0);
                return vec![reg_NT;length];
            }
            else if let IRInstruction::Call(..,arguments) = &self.instructions[index as usize] {
                let length=arguments.arguments.len();
                return vec![reg_NT;length];
            }
            //log::trace!("Get non_terminals of rule {}",rule_number);
            match rule_number {
                0xffff => {
                    log::error!("Unallowed rule number when getting the non terminal of the children");
                    return Vec::new();
                }
                0xfffe => {
                    return Vec::new();
                }
                _ => self.non_terminals[rule_number as usize].clone()
            }
        }
    }
}

fn emit_get_child_arm(pattern: &IRPattern, prelude: &TokenStream) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
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
        IRPattern::NonTerm(..) => quote! {
            #prelude,
        },
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
            {vec![#arm]}
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
            size: _,
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
        IRPattern::NonTerm(_, nonterm) => {
            let nt_type = format_ident!("{}_NT", nonterm);
            quote! {#nt_type}
        }
        IRPattern::Reg(_, _) => quote! {
            reg_NT,
        },
        IRPattern::Const(_) => TokenStream::new(),
    }
}

fn emit_reduce_terminals(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_reduce_terminals_arm(&program.definitions[i].pattern, &quote! {index}, true);
        let i = i as u16;
        arms.append_all(quote! {
            #i => {
                #arm
            },
        })
    }
    quote! {
        fn reduce_terminals(&mut self,index:u32,rule_number:u16) -> ()
        {
            match rule_number {
                #arms
                _ => {
                    log::error!("Rule {} does not exist(index={})",rule_number,index);
                }
            }
        }
    }
}

fn emit_reduce_terminals_arm(
    pattern: &IRPattern,
    prelude: &TokenStream,
    first: bool,
) -> TokenStream {
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
            let mut left = emit_reduce_terminals_arm(&*left, &left_prelude, false);
            //println!("left: {}", left.to_string());
            if let Some(right) = right {
                let right_prelude = quote! {self.get_right_index(#prelude) };
                let right = emit_reduce_terminals_arm(&*right, &right_prelude, false);
                left.append_all(right)
            }
            let txt = quote! {
                let i=#prelude as usize;
                self.rules[i] = 0xfffe;
            };
            if first {
                left
            } else {
                quote! {#txt #left}
            }
        }
        IRPattern::NonTerm(_, nonterm) => {
            // Challenge: Correctly reduce the members of the nonterminal
            let _nt_type = format_ident!("{}_NT", nonterm);
            if first {
                quote! {
                    //let i=#prelude as usize;
                    //let rule=self.instruction_states[i].rule[#nt_type];
                    //self.reduce_terminals(i as u32,rule);
                    //self.rules[i] = rule;
                }
            } else {
                quote! {
                    //let i=#prelude as usize;
                    //let rule=self.instruction_states[i].rule[#nt_type];
                    //self.rules[i] = rule;
                    //self.reduce_terminals(i as u32,rule);
                }
            }
            //todo!()
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

fn emit_get_vregisters(program: &Program) -> TokenStream {
    let mut arms = TokenStream::new();
    for i in 0..program.definitions.len() {
        let arm = emit_get_vregisters_arm(&program.definitions[i].pattern, &quote! {index});
        let i = i as u16;
        arms.append_all(quote! {
            #i => {#arm}
        });
    }

    let mut result_arm = TokenStream::new();
    for i in 0..program.definitions.len() {
        if let DefinitionType::Reg(class) = &program.definitions[i].name {
            let i = i as u16;
            let class = format_ident!("REG_CLASS_{}", class.to_string().to_uppercase());
            result_arm.append_all(quote! {
                #i => Some((self.instructions[index as usize].get_result().unwrap(),&#class)),
            });
        }
    }
    quote! {

        fn get_vregisters(&self,index:u32,rule:u16) -> (Vec<(u32,&'static RegisterClass)>,Option<(u32,&'static RegisterClass)>)
        {
            let used_vregs=if let IRInstruction::Call(_,_,_,arguments) = &self.instructions[index as usize] {
                let used_vregs = arguments.arguments.iter().map(|r| r.unwrap()).collect::<Vec<u32>>();
                let used_classes = self.get_call_regs(&arguments.sizes);
                used_vregs.iter().zip(used_classes.iter()).map(|(r,c)| (*r,*c)).collect()
            } else {
                let mut used_vregs=Vec::with_capacity(4);
                self.get_vregisters2(index, rule, &mut used_vregs);
                used_vregs
            };

            let result_vreg=self.has_result(index,rule);
            (used_vregs,result_vreg)
        }

        fn get_vregisters2(&self,index:u32,rule:u16, result:&mut Vec<(u32,&'static RegisterClass)>) -> ()
        {
            match rule
            {
                #arms
                0xfffe => (),
                _ => {
                    log::error!("Unsupporteded rule {}",rule);
                }
            };
        }

        fn has_result(&self,index:u32,rule:u16)-> Option<(u32,&'static RegisterClass)>{
            if let IRInstruction::Label(Some(_),_) = self.instructions[index as usize] {
                return None;
            }
            match rule
            {
                #result_arm
                _ => None,
            }
        }
    }
}

fn emit_get_vregisters_arm(pattern: &IRPattern, prelude: &TokenStream) -> TokenStream {
    match pattern {
        IRPattern::Node {
            term: _,
            size: _,
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
        IRPattern::NonTerm(_, nonterm) => {
            // Challenge: get registers from another rule here(rewrite to iterators?)
            let nt_type = format_ident!("{}_NT", nonterm);
            quote! {
                let rule=self.instruction_states[#prelude as usize].rule[#nt_type];
                let index=#prelude;
                self.get_vregisters2(index,rule,result);
            }
            //todo!()
        }
        IRPattern::Reg(_, class) => {
            let class = format_ident!("REG_CLASS_{}", class.to_string().to_uppercase());
            quote! {
                result.push((#prelude,& #class));
            }
        }

        IRPattern::Const(_) => TokenStream::new(),
    }
}

fn has_used_result(definition: &Definition) -> bool {
    if let DefinitionType::Reg(..) = definition.name {
        if definition.template.value().contains("{res") {
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
        let tab = match definition.name {
            DefinitionType::NonTerm(..) => "",
            _ => "\t",
        };
        let template = format!("{}{}", tab, definition.template.value());
        let arm = emit_asm_arm(&definition.pattern, &quote! {index as u32});
        let format_arm = emit_asm_format(&definition.pattern);
        let i = i as u16;
        if has_used_result(definition) {
            arms.append_all(quote! {
                #i => {

                    let res=self.allocation[instruction.get_result().unwrap() as usize][index].unwrap();
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
                //log::trace!("{}-txt-{}-txt-{}",#s,#name,#s);
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

        IRPattern::NonTerm(name, _) => {
            if let Some('_') = name.to_string().chars().next() {
                TokenStream::new()
            } else {
                quote! {,#name=#name}
            }
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

fn emit_two_address(program: &Program) -> TokenStream {
    let mut result = TokenStream::new();
    let mut first = true;
    for index in 0..program.definitions.len() {
        let pattern = &program.definitions[index];
        let rule = index as u16;
        if pattern.two_address {
            if !first {
                result.append_all(quote! {|});
            }
            first = false;
            result.append_all(quote! {#rule});
        }
    }
    if result.is_empty() {
        quote! { fn is_two_address(rule:u16) -> bool { false } }
    } else {
        quote! {
            fn is_two_address(rule:u16) -> bool {
                match rule {
                    #result => true,
                    _ => false,
                }
            }
        }
    }
}

fn emit_is_instruction(program: &Program) -> TokenStream {
    let mut result = TokenStream::new();
    for (definition, rule) in program.definitions.iter().zip(0u16..) {
        //let pattern = &program.definitions[index];
        //let rule = index as u16;
        /*if let DefinitionType::NonTerm(_) = &definition.name {
            if let IRPattern::NonTerm(_name, nonterm) = &definition.pattern {
                if &nonterm.to_string() == "reg" {
                    // Instructions that only have reg as nonterminal are effectively other instructions
                } else {
                    result.append_all(quote! {|#rule});
                }
            } else {
                result.append_all(quote! {|#rule});
            }
        }*/
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
