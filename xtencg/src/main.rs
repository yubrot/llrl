use instr::*;
use std::collections::{btree_map::Entry as BTreeEntry, BTreeMap};

pub mod instr;

static CSV: &str = include_str!("./x86.csv");

fn main() -> Result<(), String> {
    let specs = CSV
        .lines()
        .filter_map(Spec::from_csv_line)
        .collect::<Vec<_>>();

    let inst_set = build_instruction_set(specs)?;
    validate_instruction_set(&inst_set)?;
    document_in_markdown(&inst_set);

    Ok(())
}

type InstructionSet = BTreeMap<Mnemonic, Vec<Instruction>>;

fn build_instruction_set(specs: Vec<Spec>) -> Result<InstructionSet, String> {
    let mut accum = BTreeMap::<Mnemonic, BTreeMap<Vec<Operand>, Instruction>>::new();
    for spec in specs {
        let mut instruction = Instruction::try_from(spec)?;
        instruction.normalize()?;
        let mnemonic = instruction.mnemonic().to_owned();
        let operands = instruction.operands().to_vec();
        match accum.entry(mnemonic).or_default().entry(operands) {
            BTreeEntry::Vacant(e) => discard(e.insert(instruction)),
            BTreeEntry::Occupied(e) => e.into_mut().merge(instruction)?,
        }
    }

    accum
        .into_iter()
        .map(|(mnemonic, insts)| Ok((mnemonic, insts.into_iter().map(|e| e.1).collect())))
        .collect::<Result<_, _>>()
}

fn validate_instruction_set(inst_set: &InstructionSet) -> Result<(), String> {
    let mut operator_encodings = BTreeMap::new();
    for i in inst_set.values().flat_map(|insts| insts.iter()) {
        let operator_encoding = i.encoding().operator().collect::<Vec<_>>();
        match operator_encodings.entry(operator_encoding) {
            BTreeEntry::Vacant(e) => discard(e.insert(i)),
            BTreeEntry::Occupied(e) => {
                let e = e.into_mut();
                if !i.allows_encoding_overlap_with(e) {
                    Err(format!("Ambiguous encoding between {} and {}", i, e))?;
                }
            }
        }
    }
    Ok(())
}

fn document_in_markdown(inst_set: &InstructionSet) {
    for (mnemonic, insts) in inst_set.iter() {
        println!("# {}", mnemonic);
        println!("| Instruction | Encoding | Description |");
        println!("| ----------- | -------- | ----------- |");
        for inst in insts {
            println!(
                "| {} | `{} [{}]` | {} |",
                inst,
                inst.encoding(),
                inst.operand_map(),
                inst.description().replace('|', "\\|")
            );
        }
        println!();
    }
    println!("---");
    println!("Total {} mnemonics.", inst_set.len());
}

fn discard<T>(_: T) {}
