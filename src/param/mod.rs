//! Parsing of the `;`-separated parameters that follow the disposition type.
//!
//! The actual scanning lives in [`scanner`], and RFC 5987 / RFC 2231 extended
//! parameter decoding lives in [`extended`].

mod extended;
mod percent;
mod scanner;

use std::collections::BTreeMap;

/// Splits a parameterized header value into its leading value (the disposition
/// type) and the parameters that follow it.
///
/// Parameter names are lowercased, quoted values are unquoted (including
/// RFC 822 quoted-pair escapes), and RFC 5987 / RFC 2231 extended values are
/// decoded into their base name.
pub(crate) fn parse_param_content(content: &str) -> (String, BTreeMap<String, String>) {
    let mut scanner = scanner::ParamScanner::new(content);
    let value = scanner.parse_disposition_type().to_string();

    let mut params = BTreeMap::new();
    while let Some((name, param_value)) = scanner.next_parameter() {
        params.insert(name, param_value);
    }

    extended::decode_extended_params(&mut params);
    (value, params)
}
