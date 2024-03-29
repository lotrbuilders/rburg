use super::*;
use std::fmt::Display;
use std::fmt::{self};

// This module prints the backend code for debugging

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.settings)?;

        for def in &self.definitions {
            writeln!(f, "{}", def)?;
        }
        Ok(())
    }
}

impl Display for ProgramSettings {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.implements)?;

        for (size, width) in self.register_sizes.iter().flat_map(|v| v.iter()) {
            writeln!(f, "{} => {}", size, width)?;
        }

        Ok(())
    }
}

impl Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}: {} {} \"{}\" ",
            self.name,
            self.pattern,
            match self.two_address {
                true => "?",
                false => "",
            },
            self.template.value()
        )?;
        match &self.rust_code {
            Some(_) => write!(f, "{{rust-code}}")?,
            None => (),
        }
        Ok(())
    }
}

impl Display for DefinitionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use DefinitionType::*;
        match self {
            Reg(name) => write!(f, "%{}", name)?,
            NonTerm(name) => write!(f, "{}", name)?,
            Stmt => (),
        }
        Ok(())
    }
}

impl Display for IRPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use IRPattern::*;
        match self {
            Node {
                term,
                size,
                left,
                right,
            } => {
                write!(f, "{} ", term)?;
                if let Some(size) = size {
                    write!(f, " {}", *size)?;
                }
                write!(f, " ({}", *left)?;
                if let Some(right) = right {
                    write!(f, ", {}", *right)?;
                }
                write!(f, ")")?;
            }
            NonTerm(name, nonterm) => write!(f, "{} %{}", name, nonterm)?,
            Reg(name, class) => write!(f, "{} %{}", name, class)?,
            Const(name) => write!(f, "#{}", name)?,
        }
        Ok(())
    }
}
