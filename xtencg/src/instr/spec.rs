use once_cell::sync::Lazy;
use regex::Regex;

static FAR_RETURN: Lazy<Regex> = Lazy::new(|| Regex::new(r"Far +return").unwrap());

// Some operators including x87, string operators, and loop operators are unsupported
static UNSUPPORTED_OPERATORS: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(INS|OUTS|LODS|SCAS|STOS|MOVS|CMPS|REP|LOOP|ENTER|LGDT|LIDT|LLDT|SLDT|SMSW|XLAT|LSL|LTR|LAR)").unwrap()
});
static EXCEPTIONALLY_SUPPORTED_OPERATORS: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"^(MOVSD|MOVSX|CMPSD)$").unwrap());

// Some operands including segment registers, MMX registers, control registers, and debug registers are unsupported
static UNSUPPORTED_OPERANDS: Lazy<Regex> =
    Lazy::new(|| Regex::new(r":|&|Sreg|moffs|FS|GS|DR|CR|^m8|^m16|^mm").unwrap());

/// A spec of the x64 instruction.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Spec {
    pub mnemonic: String,      // ex. "movq"
    pub operands: Vec<String>, // ex. ["r64", "r/m64"]
    pub opcode: Vec<String>,   // ex. ["REX.W+", "8B", "/r"]
    pub op_en: String,         // ex. "RM"
    pub description: String,
}

impl Spec {
    pub fn from_csv_line(line: &str) -> Option<Self> {
        let cols = line.split('\t').collect::<Vec<_>>();
        exclude(cols.len() < 15)?;

        let opcode = cols[0].trim();
        let instr = cols[1].trim();
        let op_en = cols[2];
        let x64compat = cols[9].trim();
        let flags = cols[11].trim();
        let mnemonic = cols[12].trim();
        let description = cols[14].trim();

        exclude(opcode.is_empty() || instr.is_empty())?; // encoding is not always specified
        exclude(x64compat != "V")?; // 64-bit mode only
        exclude(!flags.is_empty() && !flags.starts_with("SSE"))?; // Only SSE* features are supported
        exclude(FAR_RETURN.is_match(description))?; // Far returns are unsupported

        let (mnemonic, operands) = {
            let instr = instr.replace(',', " ");
            let mut instr = instr.split_whitespace();
            let operator = instr.next().unwrap();
            let operands = instr.map(ToOwned::to_owned).collect::<Vec<_>>();

            exclude(
                UNSUPPORTED_OPERATORS.is_match(operator)
                    && !EXCEPTIONALLY_SUPPORTED_OPERATORS.is_match(operator),
            )?;

            for o in operands.iter() {
                exclude(UNSUPPORTED_OPERANDS.is_match(o))?;
            }

            if mnemonic.is_empty() {
                (operator.to_lowercase(), operands)
            } else {
                (mnemonic.to_string(), operands)
            }
        };

        let opcode = opcode
            .split_whitespace()
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>();
        let op_en = op_en.to_owned();
        let description = description.to_owned();

        Some(Self {
            mnemonic,
            operands,
            opcode,
            op_en,
            description,
        })
    }
}

fn exclude(cond: bool) -> Option<()> {
    if cond {
        None
    } else {
        Some(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn csv_line() {
        static CSV_LINE: &str = "REX.W+ 8B /r \tMOV r64, r/m64 \tRM\tW, R\t\t\t\t\t\tV\tNE\t\tmovq\t\tMove r/m64 to r64.";

        assert_eq!(
            Spec::from_csv_line(CSV_LINE),
            Some(Spec {
                mnemonic: "movq".to_string(),
                operands: vec!["r64".to_string(), "r/m64".to_string()],
                opcode: vec!["REX.W+".to_string(), "8B".to_string(), "/r".to_string()],
                op_en: "RM".to_string(),
                description: "Move r/m64 to r64.".to_string()
            })
        );
    }
}
