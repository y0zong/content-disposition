//! Decoding of RFC 5987 / RFC 2231 extended header parameters.
//!
//! Extended parameters carry a character set and a percent-encoded value, for
//! example `filename*=utf-8''%e2%82%ac%20rates`, or split the value over
//! several numbered segments (`filename*0*=utf-8''foo; filename*1*=%20bar`).

use std::collections::BTreeMap;

use charset::Charset;

use super::percent::percent_decode;

/// One segment of an RFC 2231 / RFC 5987 extended parameter.
struct ExtendedSegment {
    /// The key as it appears in the parameter map, so it can be removed once
    /// the group has been decoded.
    key: String,
    /// `None` for a single `name*` value, `Some(index)` for a continuation
    /// segment such as `name*0*`.
    index: Option<usize>,
    /// Whether the segment uses percent-encoded `ext-value` syntax.
    encoded: bool,
    value: String,
}

/// Recognizes RFC 2231 / RFC 5987 parameter names.
///
/// Returns the base name, the optional continuation index and whether the
/// value is percent-encoded (`name*` and `name*0*` forms).
fn parse_extended_key(key: &str) -> Option<(&str, Option<usize>, bool)> {
    let (rest, encoded) = match key.strip_suffix('*') {
        Some(rest) => (rest, true),
        None => (key, false),
    };

    let (base, suffix) = match rest.rfind('*') {
        Some(star) => (&rest[..star], &rest[star + 1..]),
        None => {
            return if encoded {
                // `name*`: a single extended value.
                Some((rest, None, true))
            } else {
                // A plain parameter that just happens to contain no '*' at all.
                None
            };
        }
    };

    if !suffix.is_empty() && suffix.bytes().all(|byte| byte.is_ascii_digit()) {
        let index = suffix.parse::<usize>().ok()?;
        return Some((base, Some(index), encoded));
    }

    if encoded {
        // Not a valid RFC 2231 key (e.g. `foo*bar*`); keep the whole remainder
        // as the base name so that decoding can still be attempted.
        Some((rest, None, true))
    } else {
        // A plain parameter whose name merely contains a '*' (e.g. `foo*bar`).
        None
    }
}

/// Decodes all RFC 2231 / RFC 5987 extended parameters in `params`, replacing
/// them with their decoded base names.
///
/// Groups that cannot be decoded (unknown character set, malformed value,
/// missing continuation segment, ...) are left untouched, so that a plain
/// parameter of the same name can still be used as a fallback.
pub(super) fn decode_extended_params(params: &mut BTreeMap<String, String>) {
    let mut groups: BTreeMap<String, Vec<ExtendedSegment>> = BTreeMap::new();
    for (key, value) in params.iter() {
        if let Some((base, index, encoded)) = parse_extended_key(key) {
            if base.is_empty() {
                continue;
            }
            groups
                .entry(base.to_string())
                .or_default()
                .push(ExtendedSegment {
                    key: key.clone(),
                    index,
                    encoded,
                    value: value.clone(),
                });
        }
    }

    for (base, mut segments) in groups {
        if let Some(decoded) = decode_extended_value(&mut segments) {
            for segment in &segments {
                params.remove(&segment.key);
            }
            params.insert(base, decoded);
        }
    }
}

/// Decodes a single extended value or a continuation group.
fn decode_extended_value(segments: &mut [ExtendedSegment]) -> Option<String> {
    // A non-continuation value (`name*`) takes precedence over continuations.
    if let Some(position) = segments.iter().position(|segment| segment.index.is_none()) {
        if let Some(decoded) = decode_ext_value(&segments[position].value) {
            return Some(decoded);
        }
    }

    let mut parts: Vec<&ExtendedSegment> = segments
        .iter()
        .filter(|segment| segment.index.is_some())
        .collect();
    if parts.is_empty() {
        return None;
    }
    parts.sort_by_key(|segment| segment.index);

    // RFC 2231, Section 3 requires the segments to be numbered 0..n-1 without
    // gaps; a missing segment means the value is incomplete, so ignore it.
    for (expected, part) in parts.iter().enumerate() {
        if part.index != Some(expected) {
            return None;
        }
    }

    let mut bytes = Vec::new();
    let mut charset = None;
    for (position, part) in parts.iter().enumerate() {
        if position == 0 && part.encoded {
            let (label, encoded) = split_ext_value(&part.value)?;
            charset = Some(Charset::for_label_no_replacement(label.as_bytes())?);
            bytes.extend_from_slice(&percent_decode(encoded));
        } else if part.encoded {
            bytes.extend_from_slice(&percent_decode(&part.value));
        } else {
            bytes.extend_from_slice(part.value.as_bytes());
        }
    }

    Some(match charset {
        Some(charset) => charset.decode_without_bom_handling(&bytes).0.into_owned(),
        // Unencoded continuations are plain text; treat them as UTF-8.
        None => String::from_utf8_lossy(&bytes).into_owned(),
    })
}

/// Decodes a single `charset'language'value-chars` extended value, as defined
/// by RFC 5987, Section 3.2.
fn decode_ext_value(value: &str) -> Option<String> {
    let (label, encoded) = split_ext_value(value)?;
    let charset = Charset::for_label_no_replacement(label.as_bytes())?;
    let bytes = percent_decode(encoded);
    Some(charset.decode_without_bom_handling(&bytes).0.into_owned())
}

/// Splits `charset'language'value-chars` into the character set label and the
/// percent-encoded value. The (optional) language tag is ignored.
fn split_ext_value(value: &str) -> Option<(&str, &str)> {
    let mut parts = value.splitn(3, '\'');
    let charset = parts.next()?;
    parts.next()?; // language tag
    let encoded = parts.next()?;
    if charset.is_empty() {
        return None;
    }
    Some((charset, encoded))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognizes_extended_parameter_names() {
        assert_eq!(
            parse_extended_key("filename*"),
            Some(("filename", None, true))
        );
        assert_eq!(
            parse_extended_key("filename*0*"),
            Some(("filename", Some(0), true))
        );
        assert_eq!(
            parse_extended_key("filename*12"),
            Some(("filename", Some(12), false))
        );
        assert_eq!(parse_extended_key("filename"), None);
        assert_eq!(parse_extended_key("foo*bar"), None);
    }

    #[test]
    fn splits_extended_values() {
        assert_eq!(
            split_ext_value("UTF-8'en'%E2%82%AC"),
            Some(("UTF-8", "%E2%82%AC"))
        );
        assert_eq!(split_ext_value("UTF-8''abc"), Some(("UTF-8", "abc")));
        assert_eq!(split_ext_value("UTF-8abc"), None);
        assert_eq!(split_ext_value("''abc"), None);
    }

    #[test]
    fn decodes_extended_values() {
        assert_eq!(decode_ext_value("UTF-8''%E2%82%AC").as_deref(), Some("€"));
        assert_eq!(decode_ext_value("x-unknown''abc"), None);
    }
}
