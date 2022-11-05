use instr::*;
use std::collections::{btree_map::Entry as BTreeEntry, BTreeMap, BTreeSet};
use std::env;
use std::ffi::OsStr;
use std::fs::{read_dir, File};
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::exit;

pub mod instr;
pub mod render;

static CSV: &str = include_str!("./x86.csv");

fn main() -> io::Result<()> {
    let specs = CSV
        .lines()
        .filter_map(Spec::from_csv_line)
        .collect::<Vec<_>>();

    let mut inst_set = build_instruction_set(specs).map_err(to_io_error)?;
    validate_instruction_set(&inst_set).map_err(to_io_error)?;

    for arg in env::args().skip(1) {
        match arg.as_str() {
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

    let workspace = llrl_workspace_dir()?;

    {
        let mut path = workspace.clone();
        path.extend(["xtencg", "supported-instructions.md"]);
        let mut file = File::create(path)?;
        writeln!(&mut file, "{}", render::supported_instructions(&inst_set))?;
    }

    monomorphise_instruction_set(&mut inst_set);

    {
        let mut path = workspace;
        path.extend(["xten0", "src", "asm", "inst.rs"]);
        let mut file = File::create(path)?;
        writeln!(&mut file, "{}", render::xten0(&inst_set))?;
    }

    Ok(())
}

fn usage() {
    println!("Usage:");
    println!("    xtencg         Perform code generation");
    println!("    xtencg -h      Show help");
}

fn build_instruction_set(specs: Vec<Spec>) -> Result<InstructionSet, String> {
    let mut accum = BTreeMap::<Mnemonic, BTreeMap<Vec<Operand>, Instruction>>::new();
    for spec in specs {
        let mut instruction = Instruction::try_from(spec)?;
        instruction.normalize()?;
        let mnemonic = instruction.mnemonic().to_owned();
        let operands = instruction.operands().to_vec();
        accum
            .entry(mnemonic)
            .or_default()
            .insert_with(operands, instruction, |a, b| a.merge(b))?;
    }

    Ok(accum
        .into_iter()
        .flat_map(|(_, insts)| {
            let arity_insts_map = insts
                .into_iter()
                .group_map(|(_, i)| (i.operands().len(), i));
            let (&major_arity, _) = arity_insts_map
                .iter()
                .max_by_key(|(arity, insts)| (insts.len(), -(**arity as isize)))
                .unwrap();

            arity_insts_map.into_iter().map(move |(arity, insts)| {
                let mnemonic = if arity == major_arity {
                    insts[0].mnemonic().clone()
                } else {
                    format!("{}{}", insts[0].mnemonic(), arity)
                };
                (mnemonic, insts)
            })
        })
        .collect())
}

fn validate_instruction_set(inst_set: &InstructionSet) -> Result<(), String> {
    let mut operators = BTreeMap::new();
    for i in inst_set.values().flat_map(|insts| insts.iter()) {
        let operator = i.encoding().operator().collect::<Vec<_>>();
        operators.insert_with(operator, i, |a, b| {
            a.allows_encoding_overlap_with(b)
                .then_some(())
                .ok_or_else(|| format!("Ambiguous encoding between {} and {}", a, b))
        })?;
    }
    Ok(())
}

fn monomorphise_instruction_set(inst_set: &mut InstructionSet) {
    for insts in inst_set.values_mut() {
        let it = std::mem::take(insts).into_iter();
        let mut supported_operands = BTreeSet::new();
        insts.extend(it.flat_map(|i| i.monomorphise()).filter_map(|i| {
            supported_operands
                .insert(i.operands().to_vec())
                .then_some(i)
        }));
    }
}

fn llrl_workspace_dir() -> io::Result<PathBuf> {
    let mut dir = env::current_dir()?;
    loop {
        let entries: Vec<PathBuf> = read_dir(&dir)?
            .map(|e| Ok(e?.path()))
            .collect::<Result<_, io::Error>>()?;

        if entries
            .iter()
            .any(|p| p.is_dir() && p.file_name() == Some(OsStr::new("xten0")))
        {
            return Ok(dir);
        }

        match dir.parent() {
            Some(d) => dir = d.to_path_buf(),
            None => Err(to_io_error("Cannot run xtencg outside llrl workspace"))?,
        }
    }
}

fn to_io_error(e: impl AsRef<str>) -> io::Error {
    io::Error::new(io::ErrorKind::Other, e.as_ref())
}

trait BTreeMapExt<K, V> {
    fn insert_with<E>(
        &mut self,
        key: K,
        value: V,
        merge: impl FnOnce(&mut V, V) -> Result<(), E>,
    ) -> Result<(), E>;
}

impl<K: Ord, V> BTreeMapExt<K, V> for BTreeMap<K, V> {
    fn insert_with<E>(
        &mut self,
        key: K,
        value: V,
        merge: impl FnOnce(&mut V, V) -> Result<(), E>,
    ) -> Result<(), E> {
        match self.entry(key) {
            BTreeEntry::Vacant(e) => {
                e.insert(value);
                Ok(())
            }
            BTreeEntry::Occupied(e) => merge(e.into_mut(), value),
        }
    }
}

trait IteratorExt: Iterator {
    fn group_map<K: Ord, V>(self, f: impl FnMut(Self::Item) -> (K, V)) -> BTreeMap<K, Vec<V>>;
}

impl<T: Iterator> IteratorExt for T {
    fn group_map<K: Ord, V>(self, mut f: impl FnMut(Self::Item) -> (K, V)) -> BTreeMap<K, Vec<V>> {
        self.fold(BTreeMap::<K, Vec<V>>::new(), move |mut map, item| {
            let (key, value) = f(item);
            map.entry(key).or_default().push(value);
            map
        })
    }
}
