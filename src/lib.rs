//
//  MIT OR BSD-0-Clause; https://github.com/staktrace/mailparse.git
//

//! Parser for `Content-Disposition` header field values.
//!
//! The parser accepts both the MIME profile of the header field (`RFC 2183`,
//! including the parameter value continuations from `RFC 2231`) and the HTTP
//! profile (`RFC 6266`, including the extended `filename*` parameter defined in
//! `RFC 5987`):
//!
//! * the `inline` and `attachment` disposition types defined by `RFC 2183`,
//!   plus any extension token (for example `form-data`),
//! * `token` and quoted-string parameter values, with RFC 822 quoted-pair
//!   escapes (`\"`, `\\`, ...) resolved,
//! * the `filename`, `creation-date`, `modification-date`, `read-date` and
//!   `size` parameters defined by `RFC 2183`, and
//! * extended parameter values such as `filename*=utf-8''%e2%82%ac` and
//!   `filename*0*=utf-8''foo-%e2%82%ac; filename*1*=%20rates`.
//!
//! Extended values are decoded using the character set declared in the header
//! and stored under the base parameter name (e.g. `filename`); the encoded form
//! is removed from the map. If both `filename` and `filename*` are present, the
//! `filename*` value wins, as required by `RFC 6266`, Section 4.3. If an
//! extended value cannot be decoded (for example because it is malformed or its
//! character set is unknown), it is ignored in favour of the plain parameter,
//! and the raw entry is left in `params` for introspection.
//!
//! Malformed parameters are skipped rather than rejected, following the
//! guidance in `RFC 6266`, Section 3.
//!
//! # Examples
//!
//! ```
//! use content_disposition::{parse_content_disposition, DispositionType};
//!
//! let dis = parse_content_disposition(
//!     "attachment; filename=\"EURO rates\"; filename*=utf-8''%e2%82%ac%20rates",
//! );
//! assert_eq!(dis.disposition, DispositionType::Attachment);
//! assert_eq!(dis.filename_full().as_deref(), Some("€ rates"));
//! ```

mod disposition;
mod param;

pub use disposition::{parse_content_disposition, DispositionType, ParsedContentDisposition};
