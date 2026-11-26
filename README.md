# Content-Disposition

[![Crate](https://img.shields.io/crates/v/content_disposition.svg)](https://crates.io/crates/content_disposition)

A parser for `Content-Disposition` header field values. Both profiles of the
header field are supported:

* the MIME profile defined in [RFC 2183], including the `filename`,
  `creation-date`, `modification-date`, `read-date` and `size` parameters, and
  the parameter value continuations defined in [RFC 2231];
* the HTTP profile defined in [RFC 6266], including the extended `filename*`
  parameter defined in [RFC 5987]. When both `filename` and `filename*` are
  present, `filename*` is preferred, as RFC 6266 requires.

[RFC 2183]: https://www.rfc-editor.org/rfc/rfc2183
[RFC 2231]: https://www.rfc-editor.org/rfc/rfc2231
[RFC 5987]: https://www.rfc-editor.org/rfc/rfc5987
[RFC 6266]: https://www.rfc-editor.org/rfc/rfc6266

## Documentation

See the rustdoc at [docs.rs](https://docs.rs/content_disposition/).

## API

The primary entry point for this library is the following function:

```rust
fn parse_content_disposition(header: &str) -> ParsedContentDisposition
```

Example:

```rust
use content_disposition::parse_content_disposition;

let dis = parse_content_disposition(
    "attachment; filename=\"EURO rates\"; filename*=utf-8''%e2%82%ac%20rates",
);

assert_eq!(dis.filename_full(), Some("€ rates".to_string()));
```

The returned `ParsedContentDisposition` exposes the disposition type and the
parsed parameters:

* `disposition` — `Inline`, `Attachment`, `FormData`, or `Extension(String)`;
* `params` — all parameters, with lowercased names and quoted values unquoted;
* `filename_full()` — the complete suggested filename;
* `filename()` — the suggested filename split into `(stem, Option<extension>)`;
* `name()` — the `name` parameter (used by `multipart/form-data`);
* `size()` — the RFC 2183 `size` parameter parsed as a `u64`;
* `creation_date()`, `modification_date()`, `read_date()` — the RFC 2183
  `date-time` parameters.

Extended parameter values are decoded using their declared character set and
stored under the base parameter name, so `filename*=utf-8''%e2%82%ac` produces a
`filename` entry containing `€`. If an extended value cannot be decoded, the
plain `filename` parameter is used as a fallback and the raw entry is kept in
`params`.

## Crate layout

* `src/lib.rs` — crate documentation and public re-exports
* `src/disposition.rs` — disposition types, parsed header value and accessors
* `src/param/mod.rs` — parameter parsing entry point
* `src/param/scanner.rs` — `token` / quoted-string scanner
* `src/param/extended.rs` — RFC 5987 / RFC 2231 extended value decoding
* `src/param/percent.rs` — percent-decoding helpers
* `tests/content_disposition.rs` — integration tests based on the RFC examples

## MSRV policy

Currently the minimum supported Rust version (MSRV) is 1.51.0.
MSRV increases will be kept to a minimum, and will always be accompanied with a minor version bump.
