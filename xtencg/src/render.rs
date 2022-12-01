use crate::instr::*;
use askama::Template;

#[derive(Template)]
#[template(path = "supported-instructions.md.j2")]
struct SupportedInstructions<'a> {
    inst_set: &'a InstructionSet,
}

pub fn supported_instructions(inst_set: &InstructionSet) -> String {
    SupportedInstructions { inst_set }.render().unwrap()
}

#[derive(Template)]
#[template(path = "xten0.rs.j2", escape = "none")]
struct Xten0<'a> {
    inst_set: &'a InstructionSet,
}

pub fn xten0(inst_set: &InstructionSet) -> String {
    Xten0 { inst_set }.render().unwrap()
}

#[derive(Template)]
#[template(path = "xten1.llrl.j2", escape = "none")]
struct Xten1<'a> {
    inst_set: &'a InstructionSet,
}

pub fn xten1(inst_set: &InstructionSet) -> String {
    Xten1 { inst_set }.render().unwrap()
}

mod filters {
    use super::*;
    use std::borrow::Cow;
    use std::error::Error;
    use std::fmt;

    #[derive(Debug, Clone, Copy)]
    pub struct InternalError(&'static str);

    impl fmt::Display for InternalError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.0.fmt(f)
        }
    }

    impl Error for InternalError {}

    pub fn internal_error(msg: &'static str) -> askama::Error {
        askama::Error::Custom(Box::new(InternalError(msg)))
    }

    pub fn mdtableescape(s: &str) -> askama::Result<String> {
        Ok(s.replace('|', "\\|"))
    }

    pub fn rsoperand(op: &Operand) -> askama::Result<&'static str> {
        match op {
            Operand::R(Some(Size::B)) => Ok("Gpr8"),
            Operand::R(Some(Size::W)) => Ok("Gpr16"),
            Operand::R(Some(Size::D)) => Ok("Gpr32"),
            Operand::R(Some(Size::O) | None) => Ok("Gpr64"),
            Operand::R(Some(Size::O2)) => Err(internal_error("Gpr128")),
            Operand::M(_) => Ok("Memory"),
            Operand::Xmm => Ok("Xmm"),
            Operand::Rel(s) | Operand::Imm(s) => match s {
                Size::B => Ok("i8"),
                Size::W => Ok("i16"),
                Size::D => Ok("i32"),
                Size::O => Ok("i64"),
                Size::O2 => Ok("i128"),
            },
            Operand::Rm(_, _) | Operand::Xmmm(_) => Err(internal_error("monomorphise")),
            Operand::Fixed(s) => Ok(s),
        }
    }

    pub fn llrlbool(value: &bool) -> askama::Result<&'static str> {
        Ok(match value {
            true => "#t",
            false => "#f",
        })
    }

    pub fn llrloperand(op: &Operand) -> askama::Result<Cow<'static, str>> {
        match op {
            Operand::R(Some(Size::B)) => Ok("Gpr8".into()),
            Operand::R(Some(Size::W)) => Ok("Gpr16".into()),
            Operand::R(Some(Size::D)) => Ok("Gpr32".into()),
            Operand::R(Some(Size::O) | None) => Ok("Gpr64".into()),
            Operand::R(Some(Size::O2)) => Err(internal_error("Gpr128")),
            Operand::M(_) => Ok("Memory".into()),
            Operand::Xmm => Ok("Xmm".into()),
            Operand::Rel(s) | Operand::Imm(s) => match s {
                Size::B => Ok("I8".into()),
                Size::W => Ok("I16".into()),
                Size::D => Ok("I32".into()),
                Size::O => Ok("I64".into()),
                Size::O2 => Err(internal_error("Size::O2 is unsupported")),
            },
            Operand::Rm(_, _) | Operand::Xmmm(_) => Err(internal_error("monomorphise")),
            Operand::Fixed(s) => Ok(s.replace('_', "|").into()),
        }
    }
}
