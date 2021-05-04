use std::borrow::Cow;

pub fn escape_char(c: char) -> Option<&'static str> {
    Some(match c {
        '\t' => "\\t",
        '\r' => "\\r",
        '\n' => "\\n",
        '"' => "\\\"",
        '\\' => "\\\\",
        _ => return None,
    })
}

pub fn unescape_char(c: char) -> Option<char> {
    Some(match c {
        't' => '\t',
        'r' => '\r',
        'n' => '\n',
        '"' => '"',
        '\\' => '\\',
        _ => return None,
    })
}

pub fn escape(input: &str) -> Cow<str> {
    replace(
        input,
        |c| escape_char(c).is_some(),
        |input, buf| {
            let c = input.chars().next()?;
            let ec = escape_char(c)?;
            buf.push_str(ec);
            Some(&input[c.len_utf8()..])
        },
    )
    .unwrap()
}

pub fn unescape(input: &str) -> Option<Cow<str>> {
    replace(
        input,
        |c| c == '\\',
        |input, buf| {
            let mut chars = input.chars();
            let c1 = chars.next()?;
            let c2 = chars.next()?;
            let ec = unescape_char(c2)?;
            buf.push(ec);
            Some(&input[c1.len_utf8() + c2.len_utf8()..])
        },
    )
}

fn replace<'a>(
    mut input: &'a str,
    mut pattern: impl FnMut(char) -> bool,
    mut replacer: impl FnMut(&'a str, &mut String) -> Option<&'a str>,
) -> Option<Cow<'a, str>> {
    if let mut found @ Some(_) = input.find(&mut pattern) {
        let mut buf = String::new();
        while let Some(index) = found {
            buf.push_str(&input[0..index]);
            input = replacer(&input[index..], &mut buf)?;
            found = input.find(&mut pattern);
        }
        buf.push_str(input);
        Some(Cow::Owned(buf))
    } else {
        Some(Cow::Borrowed(input))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unescape() {
        assert_eq!(unescape("hello"), Some(Cow::Borrowed("hello")));
        assert_eq!(
            unescape("Hello, World!\n"),
            Some(Cow::Borrowed("Hello, World!\n"))
        );
        assert_eq!(
            unescape("Hello, World!\\n"),
            Some(Cow::Owned("Hello, World!\n".to_string()))
        );
        assert_eq!(unescape("Hello, World!\\x"), None);
        assert_eq!(unescape("Hello, World!\\"), None);
        assert_eq!(
            unescape("Hello, World!\\\\"),
            Some(Cow::Owned("Hello, World!\\".to_string()))
        );
        assert_eq!(
            unescape("a \\n b \\r c \\t d \\\" e"),
            Some(Cow::Owned("a \n b \r c \t d \" e".to_string()))
        );
    }

    #[test]
    fn test_escape() {
        assert_eq!(escape("hello"), Cow::Borrowed("hello"));
        assert_eq!(
            escape("a \n b \r c \t d \" e \\ f"),
            Cow::Owned::<str>("a \\n b \\r c \\t d \\\" e \\\\ f".to_string())
        );
    }
}
