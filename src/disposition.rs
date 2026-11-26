//! The `Content-Disposition` header value and its parsed representation.

use std::collections::BTreeMap;

use crate::param::parse_param_content;

/// The possible disposition types in a Content-Disposition header. A more
/// comprehensive list of IANA-recognized types can be found at
/// <https://www.iana.org/assignments/cont-disp/cont-disp.xhtml>. This library
/// only enumerates the types most commonly found in email messages, and
/// provides the `Extension` value for holding all other types.
///
/// `RFC 2183`, Section 2 defines `inline` and `attachment`; any other token is
/// kept in `Extension`. Note that `RFC 2183`, Section 2.8 and `RFC 6266`,
/// Section 4.2 recommend treating unrecognized disposition types like
/// `attachment`; see [`DispositionType::is_attachment`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DispositionType {
    /// Default value, indicating the content is to be displayed inline as
    /// part of the enclosing document.
    Inline,
    /// A disposition indicating the content is not meant for inline display,
    /// but whose content can be accessed for use.
    Attachment,
    /// A disposition indicating the content contains a form submission.
    FormData,
    /// Extension type to hold any disposition not explicitly enumerated. The
    /// value is lowercased.
    Extension(String),
}

// `#[derive(Default)]` with `#[default]` would require Rust 1.62, while this
// crate keeps a lower MSRV.
#[allow(clippy::derivable_impls)]
impl Default for DispositionType {
    fn default() -> Self {
        DispositionType::Inline
    }
}

impl DispositionType {
    /// Returns `true` if the content should be handled like an attachment.
    ///
    /// RFC 2183, Section 2.8 and RFC 6266, Section 4.2 recommend treating
    /// unrecognized disposition types as `attachment`.
    pub fn is_attachment(&self) -> bool {
        matches!(
            self,
            DispositionType::Attachment | DispositionType::Extension(_)
        )
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
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParsedContentDisposition {
    /// The disposition type of the Content-Disposition header. If this
    /// is an extension type, the string will be lowercased.
    pub disposition: DispositionType,
    /// The additional params of Content-Disposition, e.g. filename. The
    /// keys in the map will be lowercased, and the values will have any
    /// enclosing quotes stripped.
    ///
    /// Extended parameter values (RFC 5987 / RFC 2231) are decoded and stored
    /// under their base name, so `filename*=utf-8''%e2%82%ac` results in a
    /// `filename` entry containing `€`. The encoded keys are removed once they
    /// have been decoded successfully.
    ///
    /// Parameter names that appear more than once are invalid per RFC 6266,
    /// Section 4.1; the last occurrence wins.
    pub params: BTreeMap<String, String>,
}

impl ParsedContentDisposition {
    /// Returns the value of the `name` parameter, if present.
    pub fn name(&self) -> Option<String> {
        self.params.get("name").cloned()
    }

    /// Returns the complete filename suggested by the `filename` or
    /// `filename*` parameter, if present.
    ///
    /// When both `filename` and `filename*` are present, the extended
    /// `filename*` value is preferred, as required by RFC 6266, Section 4.3.
    ///
    /// The value is advisory only. Callers that use it to write the payload to
    /// disk should follow RFC 6266, Section 4.3 and strip path components and
    /// control characters before handing it to the file system.
    pub fn filename_full(&self) -> Option<String> {
        self.params.get("filename").cloned()
    }

    /// Splits the suggested filename into its stem and (optional) extension.
    ///
    /// The filename is split at the last `.`, so `archive.tar.gz` yields
    /// `("archive.tar", Some("gz"))` while `README` yields
    /// `("README", None)`. Hidden files such as `.bashrc` have an empty stem:
    /// `("", Some("bashrc"))`.
    pub fn filename(&self) -> Option<(String, Option<String>)> {
        let filename = self.params.get("filename")?;
        Some(match filename.rfind('.') {
            Some(dot) => (
                filename[..dot].to_string(),
                Some(filename[dot + 1..].to_string()),
            ),
            None => (filename.clone(), None),
        })
    }

    /// Returns the size declared by the RFC 2183, Section 2.7 `size`
    /// parameter, if present and valid.
    ///
    /// The parser returns `None` when the value is not a non-negative decimal
    /// integer or does not fit in a `u64`.
    pub fn size(&self) -> Option<u64> {
        let size = self.params.get("size")?;
        if size.is_empty() || !size.bytes().all(|byte| byte.is_ascii_digit()) {
            return None;
        }
        size.parse().ok()
    }

    /// Returns the raw value of the RFC 2183 `creation-date` parameter (an
    /// RFC 822 `date-time`), if present.
    ///
    /// The value is not validated as a date by this crate.
    pub fn creation_date(&self) -> Option<String> {
        self.params.get("creation-date").cloned()
    }

    /// Returns the raw value of the RFC 2183 `modification-date` parameter (an
    /// RFC 822 `date-time`), if present.
    ///
    /// The value is not validated as a date by this crate.
    pub fn modification_date(&self) -> Option<String> {
        self.params.get("modification-date").cloned()
    }

    /// Returns the raw value of the RFC 2183 `read-date` parameter (an RFC 822
    /// `date-time`), if present.
    ///
    /// The value is not validated as a date by this crate.
    pub fn read_date(&self) -> Option<String> {
        self.params.get("read-date").cloned()
    }
}

/// Parses a `Content-Disposition` header value.
///
/// The input is the header value only, without the `Content-Disposition:` field
/// name. Both the MIME form defined by RFC 2183 and the HTTP form defined by
/// RFC 6266 are supported, including RFC 5987 extended parameter values and
/// RFC 2231 parameter continuations.
///
/// # Example
///
/// ```
/// use content_disposition::parse_content_disposition;
///
/// let dis = parse_content_disposition("form-data; name=\"cover\"; filename=\"exif.jpg\"");
/// assert_eq!(dis.name().as_deref(), Some("cover"));
/// assert_eq!(dis.filename_full().as_deref(), Some("exif.jpg"));
/// ```
pub fn parse_content_disposition(header: &str) -> ParsedContentDisposition {
    let (value, params) = parse_param_content(header);
    ParsedContentDisposition {
        disposition: parse_disposition_type(&value),
        params,
    }
}
