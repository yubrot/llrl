use instr::*;
use std::collections::{btree_map::Entry as BTreeEntry, BTreeMap};
use std::env;
use std::io;
use std::process::exit;

pub mod instr;
pub mod render;

static CSV: &str = include_str!("./x86.csv");

fn main() -> io::Result<()> {
    let specs = CSV
        .lines()
        .filter_map(Spec::from_csv_line)
        .collect::<Vec<_>>();

    let inst_set = build_instruction_set(specs).map_err(to_io_error)?;
    validate_instruction_set(&inst_set).map_err(to_io_error)?;

    for arg in env::args().skip(1) {
        match arg.as_str() {
            "-d" => {
                println!("{}", render::supported_instructions(&inst_set));
                exit(0);
            }
            "-h" => {
                usage();
                exit(0);
            }
            _ => {
                println!("Unknown option: {}", arg);
                usage();
                exit(1);
            }
        }
    }

    Ok(())
}

fn usage() {
    println!("Usage:");
    println!("    xtencg -d      Show list of supported instructions in markdown");
    println!("    xtencg -h      Show help");
}

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

fn to_io_error(e: impl AsRef<str>) -> io::Error {
    io::Error::new(io::ErrorKind::Other, e.as_ref())
}

fn discard<T>(_: T) {}
