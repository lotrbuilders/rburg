use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::Error;

use crate::*;

// Check for reg vs %ireg
pub(super) trait Checkable {
    fn check(&self, span: &Span) -> Result<(), TokenStream>;
}

impl Checkable for Program {
    fn check(&self, _: &Span) -> Result<(), TokenStream> {
        for (definition, span) in self.definitions.iter().zip(self.span.iter()) {
            definition.check(span)?;
        }
        if !self.implements.to_string().contains("Backend") {
            let string = format!(
                "Expected the backend name \"{}\" to contain \"Backend\"",
                self.implements.to_string()
            );
            let lit = LitStr::new(&string, self.implements.span());
            Err(lit.to_token_stream())
        } else {
            Ok(())
        }
    }
}

impl Checkable for Definition {
    fn check(&self, span: &Span) -> Result<(), TokenStream> {
        let mut result = TokenStream::new();
        if let Err(err) = self.pattern.check(span) {
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

impl Checkable for IRPattern {
    fn check(&self, span: &Span) -> Result<(), TokenStream> {
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
                    ("Imm" | "AddrL" | "Label" | "Jmp", IRPattern::Const(_)) => Ok(()),
                    ("Imm" | "AddrL" | "Label" | "Jmp", _) => {
                        Err(Error::new(*span, "Imm and AddrL expect constants").to_compile_error())
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
                        "Ret" | "Store" | "Add" | "Sub" | "Xor" | "Eq" | "Ne" | "Lt" | "Le" | "Gt"
                        | "Ge" | "Mul" | "Div" | "Jcc" | "Jnc",
                        IRPattern::Reg(..) | IRPattern::Node { .. } | IRPattern::NonTerm(..),
                    ) => Ok(()),

                    (
                        "Ret" | "Store" | "Add" | "Sub" | "Xor" | "Eq" | "Ne" | "Lt" | "Le" | "Gt"
                        | "Ge" | "Mul" | "Div" | "Jcc" | "Jnc",
                        _,
                    ) => Err(Error::new(*span, "Unexpected constant").to_compile_error()),

                    (string, _) => {
                        Err(Error::new(*span, &format!("Unknown patern {}", string))
                            .to_compile_error())
                    }
                }?;

                match (&term.to_string() as &str, right) {
                    ("Imm" | "AddrL" | "Load" | "Ret" | "Label" | "Jmp", None) => Ok(()),
                    ("Imm" | "AddrL" | "Load" | "Ret" | "Label" | "Jmp", _) => {
                        Err(Error::new(*span, "Unexpected right side in tree").to_compile_error())
                    }

                    (
                        "Store" | "Add" | "Sub" | "Xor" | "Eq" | "Ne" | "Lt" | "Le" | "Gt" | "Ge"
                        | "Mul" | "Div",
                        Some(IRPattern::Reg(..) | IRPattern::Node { .. } | IRPattern::NonTerm(..)),
                    ) => Ok(()),

                    (
                        "Store" | "Add" | "Sub" | "Xor" | "Eq" | "Ne" | "Lt" | "Le" | "Gt" | "Ge"
                        | "Mul" | "Div",
                        Some(_),
                    ) => Err(Error::new(*span, "Unexpected constant").to_compile_error()),

                    (
                        "Store" | "Add" | "Sub" | "Xor" | "Eq" | "Ne" | "Lt" | "Le" | "Gt" | "Ge"
                        | "Mul" | "Div",
                        None,
                    ) => Err(Error::new(*span, "Expected right side").to_compile_error()),

                    ("Jcc" | "Jnc", Some(IRPattern::Const(..))) => Ok(()),

                    ("Jcc" | "Jnc", _) => {
                        Err(Error::new(*span, "Expected right side constant").to_compile_error())
                    }

                    _ => unreachable!(),
                }?;

                if let Some(size) = size {
                    match &size.to_string() as &str {
                        "p" | "P" | "i32" | "I32" | "s32" | "S32" => Ok(()),
                        string => Err(Error::new(*span, &format!("Unknown patern {}", string))
                            .to_compile_error()),
                    }?;
                }

                left.check(span)?;
                if let Some(right) = right {
                    right.check(span)?;
                }

                Ok(())
            }
            IRPattern::NonTerm(_, _nonterm) => Ok(()),
            IRPattern::Reg(..) | IRPattern::Const(_) => Ok(()),
        }
    }
}
