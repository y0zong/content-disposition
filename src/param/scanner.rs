//! Byte-oriented scanner for `;`-separated header values.
//!
//! The scanner understands `token` and quoted-string parameter values,
//! including the quoted-pair escapes defined by RFC 822 and referenced by
//! RFC 2183. Malformed parameters are skipped instead of rejected.

/// Scanner for a header value of the form
/// `type; name=value; name="quoted value"; ...`.
///
/// The scanner only stops on ASCII bytes (`;`, `=`, `"`, `\`, SP and HTAB),
/// which never occur inside a multi-byte UTF-8 sequence, so every position it
/// stores is a valid `str` boundary.
pub(super) struct ParamScanner<'a> {
    input: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> ParamScanner<'a> {
    pub(super) fn new(input: &'a str) -> Self {
        ParamScanner {
            input,
            bytes: input.as_bytes(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(b' ') | Some(b'\t') = self.peek() {
            self.pos += 1;
        }
    }

    /// Returns everything up to the first `;`, trimmed. This is the
    /// disposition type.
    pub(super) fn parse_disposition_type(&mut self) -> &'a str {
        let input = self.input;
        let start = self.pos;
        while let Some(byte) = self.peek() {
            if byte == b';' {
                break;
            }
            self.pos += 1;
        }
        input[start..self.pos].trim()
    }

    /// Parses the next `name=value` parameter, skipping malformed ones.
    /// Returns `None` once the end of the input is reached.
    pub(super) fn next_parameter(&mut self) -> Option<(String, String)> {
        loop {
            // Skip the `;` separators (and the whitespace around them).
            while let Some(b';') | Some(b' ') | Some(b'\t') = self.peek() {
                self.pos += 1;
            }
            if self.pos >= self.bytes.len() {
                return None;
            }

            let name_start = self.pos;
            while let Some(byte) = self.peek() {
                if byte == b'=' || byte == b';' {
                    break;
                }
                self.pos += 1;
            }
            let name = self.input[name_start..self.pos].trim();

            if self.peek() != Some(b'=') {
                // A parameter without a value is invalid; skip it.
                if self.pos >= self.bytes.len() {
                    return None;
                }
                continue;
            }
            self.pos += 1; // consume '='
            self.skip_whitespace();
            let value = self.parse_value();

            if name.is_empty() {
                continue;
            }
            return Some((name.to_ascii_lowercase(), value));
        }
    }

    /// Parses a `token` or quoted-string parameter value.
    fn parse_value(&mut self) -> String {
        if self.peek() == Some(b'"') {
            self.parse_quoted_string()
        } else {
            let start = self.pos;
            while let Some(byte) = self.peek() {
                if byte == b';' {
                    break;
                }
                self.pos += 1;
            }
            self.input[start..self.pos].trim().to_string()
        }
    }

    /// Parses an RFC 822 / RFC 2045 quoted-string. The opening quote is at the
    /// current position. Trailing garbage up to the next `;` is ignored.
    fn parse_quoted_string(&mut self) -> String {
        debug_assert_eq!(self.peek(), Some(b'"'));
        self.pos += 1;

        let mut value = String::new();
        let mut escaped = false;
        while let Some(byte) = self.peek() {
            if escaped {
                let ch = self.input[self.pos..].chars().next().unwrap();
                value.push(ch);
                self.pos += ch.len_utf8();
                escaped = false;
                continue;
            }
            match byte {
                b'\\' => {
                    self.pos += 1;
                    escaped = true;
                }
                b'"' => {
                    self.pos += 1;
                    break;
                }
                _ => {
                    let ch = self.input[self.pos..].chars().next().unwrap();
                    value.push(ch);
                    self.pos += ch.len_utf8();
                }
            }
        }

        // Be lenient about junk between the closing quote and the next `;`.
        while let Some(byte) = self.peek() {
            if byte == b';' {
                break;
            }
            self.pos += 1;
        }
        value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_quoted_value_containing_separators() {
        let mut scanner = ParamScanner::new(r#""a;b \"c\"""#);
        assert_eq!(scanner.parse_value(), r#"a;b "c""#);
    }

    #[test]
    fn skips_malformed_parameters() {
        let mut scanner = ParamScanner::new("attachment; broken; a=1; =2; b=\"x;y\"");

        assert_eq!(scanner.parse_disposition_type(), "attachment");
        assert_eq!(
            scanner.next_parameter(),
            Some(("a".to_string(), "1".to_string()))
        );
        assert_eq!(
            scanner.next_parameter(),
            Some(("b".to_string(), "x;y".to_string()))
        );
        assert_eq!(scanner.next_parameter(), None);
    }

    #[test]
    fn unterminated_quoted_value_is_tolerated() {
        let mut scanner = ParamScanner::new("\"abc");
        assert_eq!(scanner.parse_value(), "abc");
    }

    #[test]
    fn parameter_names_are_lowercased() {
        let mut scanner = ParamScanner::new("attachment; FILENAME=foo.txt");
        assert_eq!(
            scanner.next_parameter(),
            Some(("filename".to_string(), "foo.txt".to_string()))
        );
    }
}
