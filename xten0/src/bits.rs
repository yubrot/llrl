use std::ops::Range;

pub trait BitExt: Sized {
    fn get_bits(self, range: Range<Self>) -> Self;
    fn set_bits(self, value: impl Into<Self>, range: Range<Self>) -> Self;
}

fn bit_mask(range: Range<u8>) -> u8 {
    ((1u8 << (range.end - range.start)) - 1u8) << range.start
}

impl BitExt for u8 {
    fn get_bits(self, range: Range<Self>) -> Self {
        let mask = bit_mask(range.clone());
        (self & mask) >> range.start
    }

    fn set_bits(self, value: impl Into<Self>, range: Range<Self>) -> Self {
        let mask = bit_mask(range.clone());
        let value = value.into() << range.start;
        assert!((value & !mask) == 0);
        (self & !mask) | value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_bits() {
        assert_eq!(0b10110011u8.get_bits(2..6), 0b1100);
    }

    #[test]
    fn set_bits() {
        assert_eq!(0b11111111u8.set_bits(0b0010, 2..6), 0b11001011);
    }
}
