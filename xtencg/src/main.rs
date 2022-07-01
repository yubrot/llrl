use instr::Spec;

pub mod instr;

static CSV: &str = include_str!("./x86.csv");

fn main() -> Result<(), String> {
    let specs = CSV
        .lines()
        .filter_map(Spec::from_csv_line)
        .collect::<Vec<_>>();

    for spec in specs {
        println!("{:?}", spec);
    }

    Ok(())
}
