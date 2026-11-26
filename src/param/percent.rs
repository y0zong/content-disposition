//! `%XX` percent-decoding helpers used by the extended parameter decoder.

/// Decodes `%XX` escapes into raw bytes. Invalid escapes are passed through
/// unchanged, and `+` is *not* treated as a space (RFC 5987 uses percent
/// encoding, not form encoding).
pub(super) fn percent_decode(encoded: &str) -> Vec<u8> {
    let bytes = encoded.as_bytes();
    let mut decoded = Vec::with_capacity(bytes.len());
    let mut index = 0;
    while index < bytes.len() {
        let is_escape = bytes[index] == b'%'
            && index + 2 < bytes.len()
            && bytes[index + 1].is_ascii_hexdigit()
            && bytes[index + 2].is_ascii_hexdigit();
        if is_escape {
            decoded.push((hex_to_nybble(bytes[index + 1]) << 4) | hex_to_nybble(bytes[index + 2]));
            index += 3;
        } else {
            decoded.push(bytes[index]);
            index += 1;
        }
    }
    decoded
}

fn hex_to_nybble(byte: u8) -> u8 {
    match byte {
        b'0'..=b'9' => byte - b'0',
        b'a'..=b'f' => byte - b'a' + 10,
        b'A'..=b'F' => byte - b'A' + 10,
        _ => panic!("Not a hex character!"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_valid_escapes() {
        assert_eq!(percent_decode("%E2%82%AC"), vec![0xE2, 0x82, 0xAC]);
        assert_eq!(percent_decode("%2e"), vec![b'.']);
    }

    #[test]
    fn passes_invalid_escapes_through() {
        assert_eq!(percent_decode("100%zz"), b"100%zz".to_vec());
        assert_eq!(percent_decode("%A"), b"%A".to_vec());
        assert_eq!(percent_decode("%"), b"%".to_vec());
    }

    #[test]
    fn plus_is_not_a_space() {
        assert_eq!(percent_decode("a+b"), b"a+b".to_vec());
    }
}
