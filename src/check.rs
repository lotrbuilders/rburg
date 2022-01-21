use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::Error;

use crate::*;

impl Program {
    pub fn check(&self, _: &Span) -> Result<(), TokenStream> {
        for (definition, span) in self.definitions.iter().zip(self.span.iter()) {
            definition.check(span)?;
        }
        let implements = &self.settings.implements;
        if !implements.to_string().contains("Backend") {
            let string = format!(
                "Expected the backend name \"{}\" to contain \"Backend\"",
                implements.to_string()
            );
            let lit = LitStr::new(&string, implements.span());
            Err(lit.to_token_stream())
        } else {
            Ok(())
        }
    }
}

impl Definition {
    fn check(&self, span: &Span) -> Result<(), TokenStream> {
        let mut result = TokenStream::new();
        if let Err(err) = self.pattern.check(&self.name, true, span) {
            result.append_all(quote! {compile_error!(#err);});
        }

        if !result.is_empty() {
            Err(result)
        } else {
            Ok(())
        }
    }
}

//fn expected_pattenr ->()

impl IRPattern {
    fn check(
        &self,
        result_name: &DefinitionType,
        first: bool,
        span: &Span,
    ) -> Result<(), TokenStream> {
        match self {
            IRPattern::Node {
                term,
                size,
                left,
                right,
            } => {
                let left = &**left;
                let right = right.as_ref().clone().map(|f| &**f);

                match (&term.to_string() as &str, left) {
                    (
                        "Imm" | "AddrL" | "AddrG" | "Label" | "Jmp" | "Call" | "Nop",
                        IRPattern::Const(_),
                    ) => Ok(()),
                    ("Imm" | "AddrL" | "AddrG" | "Label" | "Jmp" | "Call" | "Nop", _) => {
                        Err(Error::new(*span, "Expected constants").to_compile_error())
                    }

                    (
                        "Load",
                        IRPattern::Reg(..) | IRPattern::Node { .. } | IRPattern::NonTerm(..),
                    ) => Ok(()),
                    ("Load", _) => {
                        Err(Error::new(*span, "Load expects pattern or register")
                            .to_compile_error())
                    }

                    (
                        "Ret" | "Arg" | "Store" | "Add" | "Sub" | "Xor" | "Or" | "And" | "Eq"
                        | "Ne" | "Lt" | "Le" | "Gt" | "Ge" | "Mul" | "Div" | "Jcc" | "Jnc" | "Cvp"
                        | "Cvs" | "Cvu",
                        IRPattern::Reg(..) | IRPattern::Node { .. } | IRPattern::NonTerm(..),
                    ) => Ok(()),

                    ("CallV", IRPattern::Reg(..)) => Ok(()),
                    ("CallV", _) => {
                        Err(Error::new(*span, "CallV expects register").to_compile_error())
                    }

                    (
                        "Ret" | "Arg" | "Store" | "Add" | "Sub" | "Xor" | "Or" | "And" | "Eq"
                        | "Ne" | "Lt" | "Le" | "Gt" | "Ge" | "Mul" | "Div" | "Jcc" | "Jnc" | "Cvp"
                        | "Cvs" | "Cvu",
                        _,
                    ) => Err(Error::new(*span, "Unexpected constant").to_compile_error()),

                    (string, _) => {
                        Err(Error::new(*span, &format!("Unknown patern {}", string))
                            .to_compile_error())
                    }
                }?;

                match (&term.to_string() as &str, right) {
                    (
                        "Imm" | "AddrL" | "AddrG" | "Load" | "Ret" | "Arg" | "Label" | "Jmp"
                        | "Call" | "CallV" | "Cvp" | "Cvs" | "Cvu" | "Nop",
                        None,
                    ) => Ok(()),
                    (
                        "Imm" | "AddrL" | "AddrG" | "Load" | "Ret" | "Arg" | "Label" | "Jmp"
                        | "Call" | "CallV" | "Cvp" | "Cvs" | "Cvu" | "Nop",
                        _,
                    ) => Err(Error::new(*span, "Unexpected right side in tree").to_compile_error()),

                    (
                        "Store" | "Add" | "Sub" | "Xor" | "Or" | "And" | "Eq" | "Ne" | "Lt" | "Le"
                        | "Gt" | "Ge" | "Mul" | "Div",
                        Some(IRPattern::Reg(..) | IRPattern::Node { .. } | IRPattern::NonTerm(..)),
                    ) => Ok(()),

                    (
                        "Store" | "Add" | "Sub" | "Xor" | "Or" | "And" | "Eq" | "Ne" | "Lt" | "Le"
                        | "Gt" | "Ge" | "Mul" | "Div",
                        Some(_),
                    ) => Err(Error::new(*span, "Unexpected constant").to_compile_error()),

                    (
                        "Store" | "Add" | "Sub" | "Xor" | "Or" | "And" | "Eq" | "Ne" | "Lt" | "Le"
                        | "Gt" | "Ge" | "Mul" | "Div",
                        None,
                    ) => Err(Error::new(*span, "Expected right side").to_compile_error()),

                    ("Jcc" | "Jnc", Some(IRPattern::Const(..))) => Ok(()),

                    ("Jcc" | "Jnc", _) => {
                        Err(Error::new(*span, "Expected right side constant").to_compile_error())
                    }

                    _ => unreachable!(),
                }?;

                if let Some(size) = size {
                    for s in split_size(&size.to_string().to_uppercase()) {
                        match &s as &str {
                            "P" | "S8" | "S16" | "S32" | "S64" | "V" => Ok(()),
                            string => Err(Error::new(*span, &format!("Unknown patern {}", string))
                                .to_compile_error()),
                        }?;
                    }
                }

                if first {
                    match (&term.to_string() as &str, result_name) {
                        (
                            "Jcc" | "Jnc" | "Jmp" | "Store" | "Ret" | "Arg" | "Label" | "Nop",
                            DefinitionType::Stmt,
                        ) => Ok(()),
                        ("Call" | "CallV", DefinitionType::Stmt) => Ok(()),
                        (_, DefinitionType::Stmt) => {
                            Err(Error::new(*span, "Expected a result").to_compile_error())
                        }
                        (_, _) => Ok(()),
                    }?;
                } else {
                    match &term.to_string() as &str {
                        "Jcc" | "Jnc" | "Jmp" | "Store" | "Ret" | "Arg" | "Label" => Err(
                            Error::new(*span, "Unexpected terminal without result in tree")
                                .to_compile_error(),
                        ),
                        _ => Ok(()),
                    }?;
                }

                left.check(result_name, false, span)?;
                if let Some(right) = right {
                    right.check(result_name, false, span)?;
                }

                Ok(())
            }
            IRPattern::NonTerm(_, _nonterm) => Ok(()),
            IRPattern::Reg(..) | IRPattern::Const(_) => Ok(()),
        }
    }
}
