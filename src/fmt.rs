use super::*;
use std::fmt;
use std::fmt::Display;

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for def in &self.definitions {
            writeln!(f, "{}", def)?;
        }
        Ok(())
    }
}

impl Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}: {} \"{}\" ",
            self.name,
            self.pattern,
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
            Reg(name) => write!(f, "{}", name)?,
            Stmt => (),
        }
        Ok(())
    }
}

impl Display for IRPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use IRPattern::*;
        match self {
            Node { term, left, right } => {
                write!(f, "{} ({}", term, *left)?;
                if let Some(right) = right {
                    write!(f, ", {}", *right)?;
                }
                write!(f, ")")?;
            }
            Reg(name, class) => write!(f, "{} %{}", name, class)?,
            Const(name) => write!(f, "#{}", name)?,
        }
        Ok(())
    }
}
