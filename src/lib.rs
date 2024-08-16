//
//  MIT OR BSD-0-Clause; https://github.com/staktrace/mailparse.git
//

use std::collections::{BTreeMap, HashMap};

use charset::Charset;

/// The possible disposition types in a Content-Disposition header. A more
/// comprehensive list of IANA-recognized types can be found at
/// https://www.iana.org/assignments/cont-disp/cont-disp.xhtml. This library
/// only enumerates the types most commonly found in email messages, and
/// provides the `Extension` value for holding all other types.
#[derive(Debug, Clone, PartialEq)]
pub enum DispositionType {
    /// Default value, indicating the content is to be displayed inline as
    /// part of the enclosing document.
    Inline,
    /// A disposition indicating the content is not meant for inline display,
    /// but whose content can be accessed for use.
    Attachment,
    /// A disposition indicating the content contains a form submission.
    FormData,
    /// Extension type to hold any disposition not explicitly enumerated.
    Extension(String),
}

impl Default for DispositionType {
    fn default() -> Self {
        DispositionType::Inline
    }
}

/// Convert the string represented disposition type to enum.
fn parse_disposition_type(disposition: &str) -> DispositionType {
    match &disposition.to_lowercase()[..] {
        "inline" => DispositionType::Inline,
        "attachment" => DispositionType::Attachment,
        "form-data" => DispositionType::FormData,
        extension => DispositionType::Extension(extension.to_string()),
    }
}

/// A struct to hold a more structured representation of the Content-Disposition header.
/// This is provided mostly as a convenience since this metadata is usually
/// needed to interpret the message body properly.
#[derive(Debug, Default)]
pub struct ParsedContentDisposition {
    /// The disposition type of the Content-Disposition header. If this
    /// is an extension type, the string will be lowercased.
    pub disposition: DispositionType,
    /// The additional params of Content-Disposition, e.g. filename. The
    /// keys in the map will be lowercased, and the values will have any
    /// enclosing quotes stripped.
    pub params: BTreeMap<String, String>,
}

impl ParsedContentDisposition {
    #[allow(dead_code)]
    pub fn name(&self) -> Option<String> {
        self.params.get("name").cloned()
    }
    #[allow(dead_code)]
    pub fn filename_full(&self) -> Option<String> {
        self.params.get("filename").cloned()
    }
    #[allow(dead_code)]
    pub fn filename(&self) -> Option<(String, Option<String>)> {
        let clone = self.params.get("filename").cloned();
        match clone {
            Some(c) => {
                let mut arr: Vec<&str> = c.split(".").collect();
                let last = arr.pop();
                let first = arr.join(".");
                Some(match last {
                    Some(l) => (first, Some(l.to_owned())),
                    None => (first, None),
                })
            }
            None => None,
        }
    }
}

pub fn parse_content_disposition(header: &str) -> ParsedContentDisposition {
    let params = parse_param_content(header);
    let disposition = parse_disposition_type(&params.value);
    ParsedContentDisposition {
        disposition,
        params: params.params,
    }
}

/// Used to store params for content-type and content-disposition
struct ParamContent {
    value: String,
    params: BTreeMap<String, String>,
}

/// Parse parameterized header values such as that for Content-Type
/// e.g. `multipart/alternative; boundary=foobar`
/// Note: this function is not made public as it may require
/// significant changes to be fully correct. For instance,
/// it does not handle quoted parameter values containing the
/// semicolon (';') character. It also produces a BTreeMap,
/// which implicitly does not support multiple parameters with
/// the same key. Also, the parameter values may contain language
/// information in a format specified by RFC 2184 which is thrown
/// away. The format for parameterized header values doesn't
/// appear to be strongly specified anywhere.
fn parse_param_content(content: &str) -> ParamContent {
    let mut tokens = content.split(';');
    // There must be at least one token produced by split, even if it's empty.
    let value = tokens.next().unwrap().trim();
    let mut map: BTreeMap<String, String> = tokens
        .filter_map(|kv| {
            kv.find('=').map(|idx| {
                let key = kv[0..idx].trim().to_lowercase();
                let mut value = kv[idx + 1..].trim();
                if value.starts_with('"') && value.ends_with('"') && value.len() > 1 {
                    value = &value[1..value.len() - 1];
                }
                (key, value.to_string())
            })
        })
        .collect();

    // Decode charset encoding, as described in RFC 2184, Section 4.
    let decode_key_list: Vec<String> = map
        .keys()
        .filter_map(|k| k.strip_suffix('*'))
        .map(String::from)
        // Skip encoded keys where there is already an equivalent decoded key in the map
        .filter(|k| !map.contains_key(k))
        .collect();
    let encodings = compute_parameter_encodings(&map, &decode_key_list);
    // Note that when we get here, we might still have entries in `encodings` for continuation segments
    // that didn't have a *0 segment at all. These shouldn't exist per spec so we can do whatever we want,
    // as long as we don't panic.
    for (k, (e, strip)) in encodings {
        if let Some(charset) = Charset::for_label_no_replacement(e.as_bytes()) {
            let key = format!("{}*", k);
            let percent_encoded_value = map.remove(&key).unwrap();
            let encoded_value = if strip {
                percent_decode(percent_encoded_value.splitn(3, '\'').nth(2).unwrap_or(""))
            } else {
                percent_decode(&percent_encoded_value)
            };
            let decoded_value = charset.decode_without_bom_handling(&encoded_value).0;
            map.insert(k, decoded_value.to_string());
        }
    }

    // Unwrap parameter value continuations, as described in RFC 2184, Section 3.
    let unwrap_key_list: Vec<String> = map
        .keys()
        .filter_map(|k| k.strip_suffix("*0"))
        .map(String::from)
        // Skip wrapped keys where there is already an unwrapped equivalent in the map
        .filter(|k| !map.contains_key(k))
        .collect();
    for unwrap_key in unwrap_key_list {
        let mut unwrapped_value = String::new();
        let mut index = 0;
        while let Some(wrapped_value_part) = map.remove(&format!("{}*{}", &unwrap_key, index)) {
            index += 1;
            unwrapped_value.push_str(&wrapped_value_part);
        }
        let old_value = map.insert(unwrap_key, unwrapped_value);
        assert!(old_value.is_none());
    }

    ParamContent {
        value: value.into(),
        params: map,
    }
}

/// In the returned map, the key is one of the entries from the decode_key_list,
/// (i.e. the parameter key with the trailing '*' stripped). The value is a tuple
/// containing the encoding (or empty string for no encoding found) and a flag
/// that indicates if the encoding needs to be stripped from the value. This is
/// set to true for non-continuation parameter values.
fn compute_parameter_encodings(
    map: &BTreeMap<String, String>,
    decode_key_list: &Vec<String>,
) -> HashMap<String, (String, bool)> {
    // To handle section 4.1 (combining encodings with continuations), we first
    // compute the encoding for each parameter value or parameter value segment
    // that is encoded. For continuation segments the encoding from the *0 segment
    // overwrites the continuation segment's encoding, if there is one.
    let mut encodings: HashMap<String, (String, bool)> = HashMap::new();
    for decode_key in decode_key_list {
        if let Some(unwrap_key) = decode_key.strip_suffix("*0") {
            // Per spec, there should always be an encoding. If it's missing, handle that case gracefully
            // by setting it to an empty string that we handle specially later.
            let encoding = map
                .get(&format!("{}*", decode_key))
                .unwrap()
                .split('\'')
                .next()
                .unwrap_or("");
            let continuation_prefix = format!("{}*", unwrap_key);
            for continuation_key in decode_key_list {
                if continuation_key.starts_with(&continuation_prefix) {
                    // This may (intentionally) overwite encodings previously found for the
                    // continuation segments (which are bogus). In those cases, the flag
                    // in the tuple should get updated from true to false.
                    encodings.insert(
                        continuation_key.clone(),
                        (encoding.to_string(), continuation_key == decode_key),
                    );
                }
            }
        } else if !encodings.contains_key(decode_key) {
            let encoding = map
                .get(&format!("{}*", decode_key))
                .unwrap()
                .split('\'')
                .next()
                .unwrap_or("")
                .to_string();
            let old_value = encodings.insert(decode_key.clone(), (encoding, true));
            assert!(old_value.is_none());
        }
        // else this is a continuation segment and the encoding has already been populated
        // by the initial *0 segment, so we can ignore it.
    }
    encodings
}

fn percent_decode(encoded: &str) -> Vec<u8> {
    let mut decoded = Vec::with_capacity(encoded.len());
    let mut bytes = encoded.bytes();
    let mut next = bytes.next();
    while next.is_some() {
        let b = next.unwrap();
        if b != b'%' {
            decoded.push(b);
            next = bytes.next();
            continue;
        }

        let top = match bytes.next() {
            Some(n) if n.is_ascii_hexdigit() => n,
            n => {
                decoded.push(b);
                next = n;
                continue;
            }
        };
        let bottom = match bytes.next() {
            Some(n) if n.is_ascii_hexdigit() => n,
            n => {
                decoded.push(b);
                decoded.push(top);
                next = n;
                continue;
            }
        };
        let decoded_byte = (hex_to_nybble(top) << 4) | hex_to_nybble(bottom);
        decoded.push(decoded_byte);

        next = bytes.next();
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
    fn test_parse_content_disposition() {
        let dis = parse_content_disposition("inline");
        assert_eq!(dis.disposition, DispositionType::Inline);
        assert_eq!(dis.params.get("name"), None);
        assert_eq!(dis.params.get("filename"), None);

        let dis = parse_content_disposition(
            " attachment; x=y; charset=\"fake\" ; x2=y2; name=\"King Joffrey.death\"",
        );
        assert_eq!(dis.disposition, DispositionType::Attachment);
        assert_eq!(
            dis.params.get("name"),
            Some(&"King Joffrey.death".to_string())
        );
        assert_eq!(dis.params.get("filename"), None);

        let dis = parse_content_disposition(" form-data; name=\"cover\"; filename=\"exif.jpg\"");
        assert_eq!(dis.disposition, DispositionType::FormData);
        assert_eq!(dis.name(), Some("cover".to_string()));
        assert_eq!(dis.filename_full(), Some("exif.jpg".to_string()));
        let f = dis.filename().unwrap();
        assert_eq!(f.0, "exif".to_string());
        assert_eq!(f.1, Some("jpg".to_string()));
    }
}
