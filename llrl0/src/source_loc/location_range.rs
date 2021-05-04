use super::Location;
use std::fmt;
use std::ops::Range;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, Default)]
pub struct LocationRange {
    /// An inclusive range's start position.
    pub start: Location,
    /// An exclusive range's end position.
    pub end: Location,
}

impl LocationRange {
    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }

    pub fn empty(location: Location) -> Self {
        Self::new(location, location)
    }

    pub fn from_range(text: &str, offset_range: Range<u32>) -> Self {
        let start_offset = offset_range.start as usize;
        let end_offset = offset_range.end as usize;
        let mut start = Location::new();
        start.eat(&text[..start_offset]);
        let mut end = start;
        end.eat(&text[start_offset..end_offset]);
        Self::new(start, end)
    }

    pub fn review<'a>(&self, text: &'a str) -> &'a str {
        &text[self.start.offset as usize..self.end.offset as usize]
    }
}

impl fmt::Display for LocationRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_review() {
        let original = "Hello,\nWorld!\n";
        let (a, bc) = original.split_at(5);
        let (b, c) = bc.split_at(4);
        let l0 = Location::new();
        let mut l1 = l0;
        l1.eat(a);
        let mut l2 = l1;
        l2.eat(b);
        let mut l3 = l2;
        l3.eat(c);
        assert_eq!(LocationRange::new(l0, l1).review(&original), "Hello");
        assert_eq!(LocationRange::new(l1, l2).review(&original), ",\nWo");
        assert_eq!(LocationRange::new(l2, l3).review(&original), "rld!\n");
    }

    #[test]
    fn test_from_range() {
        let original = "ABCDEF";
        let range = LocationRange::from_range(&original, 2..4);
        assert_eq!(range.start.offset, 2);
        assert_eq!(range.end.offset, 4);
    }

    #[test]
    fn test_display() {
        assert_eq!(
            LocationRange::new(
                Location {
                    offset: 5,
                    line: 2,
                    character: 1
                },
                Location {
                    offset: 10,
                    line: 3,
                    character: 3
                }
            )
            .to_string(),
            "3:2-4:4"
        );
    }
}
