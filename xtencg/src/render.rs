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

mod filters {
    pub fn mdtableescape(s: &str) -> askama::Result<String> {
        Ok(s.replace('|', "\\|"))
    }
}
