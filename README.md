# Content-Disposition
[![Crate](https://img.shields.io/crates/v/content_disposition.svg)](https://crates.io/crates/content_disposition)

## Documentation
See the rustdoc at [docs.rs](https://docs.rs/mailparse/).

## API
The primary entry point for this library is the following function:

```rust
    fn parse_content_disposition(header: &str) -> ParsedContentDisposition
```

Example
``` rust
    let dis = parse_content_disposition(" form-data; name=\"cover\"; filename=\"exif.jpg\"");

    assert_eq!(dis.disposition, DispositionType::FormData);
    assert_eq!(dis.name(), Some("cover".to_string()));
    assert_eq!(dis.filename(), Some("exif.jpg".to_string()));
```

## MSRV policy
Currently the minimum supported Rust version (MSRV) is 1.51.0.
MSRV increases will be kept to a minimum, and will always be accompanied with a minor version bump.