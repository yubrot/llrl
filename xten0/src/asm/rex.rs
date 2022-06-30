// https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html

use crate::bits::BitExt;

/// Obtain an REX prefix by encoding W, R, X, B.
///
/// The REX prefix must follow the Legacy prefixes and precede the Opcode.
pub fn encode(w: bool, r: bool, x: bool, b: bool) -> Option<u8> {
    if w || r || x || b {
        Some(
            0u8.set_bits(0b0100, 4..8)
                .set_bits(w, 3..4)
                .set_bits(r, 2..3)
                .set_bits(x, 1..2)
                .set_bits(b, 0..1),
        )
    } else {
        None // REX prefix is unnecessary
    }
}

// Encoding correctness is tested in `super::operand` along with operand conversions.
