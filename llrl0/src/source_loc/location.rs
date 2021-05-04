use std::fmt;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, Default)]
pub struct Location {
    /// Zero-based, in bytes offset of the string.
    pub offset: u32,
    /// Zero-based, line position of the string.
    pub line: u32,
    /// Zero-based, in bytes offset on a line of the string.
    pub character: u32,
}

impl Location {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from_offset(text: &str, offset: u32) -> Self {
        let mut loc = Self::new();
        loc.eat(&text[..offset as usize]);
        loc
    }

    pub fn eat(&mut self, substr: &str) {
        self.offset += substr.len() as u32;

        let mut lines = substr.rsplit('\n');
        let last_line_len = lines.next().unwrap().len() as u32;
        match lines.count() {
            0 => {
                self.character += last_line_len;
            }
            n => {
                self.line += n as u32;
                self.character = last_line_len;
            }
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.character + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eat() {
        let mut loc = Location::new();
        assert_eq!(loc.offset, 0);
        assert_eq!(loc.line, 0);
        assert_eq!(loc.character, 0);

        loc.eat("hello");
        assert_eq!(loc.offset, 5);
        assert_eq!(loc.line, 0);
        assert_eq!(loc.character, 5);

        loc.eat("日本語");
        assert_eq!(loc.offset, 14);
        assert_eq!(loc.line, 0);
        assert_eq!(loc.character, 14);

        loc.eat("a\nb");
        assert_eq!(loc.offset, 17);
        assert_eq!(loc.line, 1);
        assert_eq!(loc.character, 1);

        loc.eat("\n\r\n");
        assert_eq!(loc.offset, 20);
        assert_eq!(loc.line, 3);
        assert_eq!(loc.character, 0);

        loc.eat("xxx\n\nyyy");
        assert_eq!(loc.offset, 28);
        assert_eq!(loc.line, 5);
        assert_eq!(loc.character, 3);
    }

    #[test]
    fn test_from_offset() {
        let s = "Hello, World!\n";
        let loc = Location::from_offset(&s, s.len() as u32);
        assert_eq!(loc.offset, 14);
        assert_eq!(loc.line, 1);
        assert_eq!(loc.character, 0);
    }

    #[test]
    fn test_display() {
        assert_eq!(
            Location {
                offset: 10,
                line: 3,
                character: 3
            }
            .to_string(),
            "4:4"
        );
    }
}
