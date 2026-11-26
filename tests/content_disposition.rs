//! Integration tests for the public API, based on the examples in RFC 2183,
//! RFC 2231, RFC 5987 and RFC 6266.

use content_disposition::{parse_content_disposition, DispositionType};

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

#[test]
fn test_rfc2183_parameters() {
    // Example from RFC 2183, Section 3 (the trailing ';' is intentional).
    let dis = parse_content_disposition(
        "attachment; filename=genome.jpeg; \
         modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\";",
    );
    assert_eq!(dis.disposition, DispositionType::Attachment);
    assert_eq!(dis.filename_full().as_deref(), Some("genome.jpeg"));
    assert_eq!(
        dis.modification_date().as_deref(),
        Some("Wed, 12 Feb 1997 16:29:51 -0500")
    );
    assert_eq!(dis.creation_date(), None);
    assert_eq!(dis.read_date(), None);
    assert_eq!(dis.size(), None);

    let dis = parse_content_disposition(
        "attachment; filename=\"archive.tar.gz\"; \
         creation-date=\"Wed, 12 Feb 1997 16:29:51 -0500\"; \
         read-date=\"Thu, 13 Feb 1997 09:00:00 +0000\"; size=1048576",
    );
    assert_eq!(
        dis.creation_date().as_deref(),
        Some("Wed, 12 Feb 1997 16:29:51 -0500")
    );
    assert_eq!(
        dis.read_date().as_deref(),
        Some("Thu, 13 Feb 1997 09:00:00 +0000")
    );
    assert_eq!(dis.size(), Some(1048576));
    assert_eq!(
        dis.filename(),
        Some(("archive.tar".to_string(), Some("gz".to_string())))
    );
}

#[test]
fn test_size_parameter_validation() {
    assert_eq!(
        parse_content_disposition("attachment; size=0").size(),
        Some(0)
    );
    // RFC 2183 defines size as 1*DIGIT; quoted values are tolerated.
    assert_eq!(
        parse_content_disposition("attachment; size=\"1024\"").size(),
        Some(1024)
    );
    assert_eq!(
        parse_content_disposition("attachment; size=2147483648").size(),
        Some(2147483648)
    );
    assert_eq!(parse_content_disposition("attachment; size=").size(), None);
    assert_eq!(
        parse_content_disposition("attachment; size=-1").size(),
        None
    );
    assert_eq!(
        parse_content_disposition("attachment; size=1.5").size(),
        None
    );
    assert_eq!(
        parse_content_disposition("attachment; size=18446744073709551616").size(),
        None
    );
}

#[test]
fn test_quoted_strings() {
    let dis = parse_content_disposition(
        r#"attachment; filename="semi;colon.txt"; name="a \"quoted\" name""#,
    );
    assert_eq!(dis.filename_full().as_deref(), Some("semi;colon.txt"));
    assert_eq!(dis.name().as_deref(), Some("a \"quoted\" name"));

    // Quoted-pair escapes, as defined by RFC 822 and referenced by RFC 2183.
    let dis = parse_content_disposition(r#"attachment; filename="C:\\temp\\file.txt""#);
    assert_eq!(dis.filename_full().as_deref(), Some(r#"C:\temp\file.txt"#));

    // Whitespace around '=' and between the closing quote and ';' is allowed.
    let dis = parse_content_disposition("attachment; filename = \" spaced name.txt\" ;");
    assert_eq!(dis.filename_full().as_deref(), Some(" spaced name.txt"));

    // Unterminated quoted strings are tolerated.
    let dis = parse_content_disposition("attachment; filename=\"unterminated.txt");
    assert_eq!(dis.filename_full().as_deref(), Some("unterminated.txt"));
}

#[test]
fn test_rfc6266_examples() {
    let dis = parse_content_disposition("Attachment; filename=example.html");
    assert_eq!(dis.disposition, DispositionType::Attachment);
    assert_eq!(dis.filename_full().as_deref(), Some("example.html"));

    let dis = parse_content_disposition("INLINE; FILENAME= \"an example.html\"");
    assert_eq!(dis.disposition, DispositionType::Inline);
    assert_eq!(dis.filename_full().as_deref(), Some("an example.html"));

    let dis = parse_content_disposition("attachment; filename*= UTF-8''%e2%82%ac%20rates");
    assert_eq!(dis.filename_full().as_deref(), Some("€ rates"));
    assert!(!dis.params.contains_key("filename*"));

    let dis = parse_content_disposition(
        "attachment; filename=\"EURO rates\"; filename*=utf-8''%e2%82%ac%20rates",
    );
    assert_eq!(dis.filename_full().as_deref(), Some("€ rates"));
    assert!(!dis.params.contains_key("filename*"));
}

#[test]
fn test_rfc6266_filename_star_precedence() {
    // `filename*` wins regardless of the parameter order.
    let dis = parse_content_disposition(
        "attachment; filename=\"fallback.txt\"; \
         filename*=UTF-8''%E6%8A%A5%E5%91%8A.pdf",
    );
    assert_eq!(dis.filename_full().as_deref(), Some("报告.pdf"));

    let dis = parse_content_disposition(
        "attachment; filename*=UTF-8''%E6%8A%A5%E5%91%8A.pdf; filename=\"fallback.txt\"",
    );
    assert_eq!(dis.filename_full().as_deref(), Some("报告.pdf"));

    let dis = parse_content_disposition("attachment; filename*=UTF-8''%E6%8A%A5%E5%91%8A.pdf");
    assert_eq!(
        dis.filename(),
        Some(("报告".to_string(), Some("pdf".to_string())))
    );
}

#[test]
fn test_extended_value_charsets() {
    let dis = parse_content_disposition("attachment; filename*=ISO-8859-1''%A3%20rates");
    assert_eq!(dis.filename_full().as_deref(), Some("£ rates"));

    // The language tag is parsed but ignored (RFC 5987, Section 3.2.1).
    let dis = parse_content_disposition("attachment; filename*=utf-8'en'%C2%A3%20rates");
    assert_eq!(dis.filename_full().as_deref(), Some("£ rates"));

    // Character set labels are case-insensitive.
    let dis = parse_content_disposition("attachment; filename*=UtF-8''%E2%82%AC");
    assert_eq!(dis.filename_full().as_deref(), Some("€"));
}

#[test]
fn test_extended_value_fallbacks() {
    for header in [
        // Unknown character set.
        "attachment; filename=\"fallback.txt\"; filename*=x-unknown''%41",
        // Missing the two apostrophe separators.
        "attachment; filename=fallback.txt; filename*=UTF-8%20abc",
        "attachment; filename=fallback.txt; filename*=UTF-8abc",
        // Empty character set.
        "attachment; filename=fallback.txt; filename*=''abc",
    ] {
        let dis = parse_content_disposition(header);
        assert_eq!(
            dis.filename_full().as_deref(),
            Some("fallback.txt"),
            "header: {}",
            header
        );
    }

    // Without a plain fallback, an undecodable value yields no filename.
    let dis = parse_content_disposition("attachment; filename*=x-unknown''%41");
    assert_eq!(dis.filename_full(), None);
    assert!(dis.params.contains_key("filename*"));
}

#[test]
fn test_rfc2231_continuations() {
    let dis = parse_content_disposition("attachment; filename*0=\"foo\"; filename*1=\".html\"");
    assert_eq!(dis.filename_full().as_deref(), Some("foo.html"));

    let dis = parse_content_disposition(
        "attachment; filename*0*=UTF-8''foo-%E2%82%AC; filename*1*=%20rates.html",
    );
    assert_eq!(dis.filename_full().as_deref(), Some("foo-€ rates.html"));

    // Example from RFC 2231, Section 3.
    let dis = parse_content_disposition(
        "attachment; title*0*=us-ascii'en'This%20is%20even%20more%20; \
         title*1*=%2A%2A%2Afun%2A%2A%2A%20; title*2=\"is it not?\"",
    );
    assert_eq!(
        dis.params.get("title").map(String::as_str),
        Some("This is even more ***fun*** is it not?")
    );

    // Example from RFC 2231, Section 4.
    let dis = parse_content_disposition(
        "attachment; title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A",
    );
    assert_eq!(
        dis.params.get("title").map(String::as_str),
        Some("This is ***fun***")
    );

    // All continuation segments are removed after decoding.
    let dis =
        parse_content_disposition("attachment; filename*0*=UTF-8''foo; filename*1*=%20bar.txt");
    assert!(!dis.params.contains_key("filename*0*"));
    assert!(!dis.params.contains_key("filename*1*"));
}

#[test]
fn test_continuation_gaps_fall_back() {
    let dis = parse_content_disposition(
        "attachment; filename=\"fallback.txt\"; filename*0=\"foo\"; filename*2=\".txt\"",
    );
    assert_eq!(dis.filename_full().as_deref(), Some("fallback.txt"));

    let dis = parse_content_disposition("attachment; filename*0=\"foo\"; filename*2=\".txt\"");
    assert_eq!(dis.filename_full(), None);
}

#[test]
fn test_percent_decoding_scope() {
    // The plain `filename` parameter must not be percent-decoded
    // (RFC 6266, Appendix D).
    let dis = parse_content_disposition("attachment; filename=\"100%25.txt\"");
    assert_eq!(dis.filename_full().as_deref(), Some("100%25.txt"));

    // Only `%XX` sequences are decoded for extended values; '+' is a
    // literal plus sign, not a space.
    let dis = parse_content_disposition("attachment; filename*=UTF-8''a+b%20c.txt");
    assert_eq!(dis.filename_full().as_deref(), Some("a+b c.txt"));

    // Invalid percent escapes are passed through.
    let dis = parse_content_disposition("attachment; filename*=UTF-8''100%zz");
    assert_eq!(dis.filename_full().as_deref(), Some("100%zz"));
}

#[test]
fn test_case_insensitive_parameter_names() {
    let dis = parse_content_disposition("attachment; FILENAME*=UTF-8''%E2%82%AC");
    assert_eq!(dis.filename_full().as_deref(), Some("€"));

    let dis = parse_content_disposition("attachment; SIZE=42");
    assert_eq!(dis.size(), Some(42));
}

#[test]
fn test_disposition_type_is_attachment() {
    assert!(parse_content_disposition("attachment")
        .disposition
        .is_attachment());
    assert!(!parse_content_disposition("inline")
        .disposition
        .is_attachment());
    assert!(!parse_content_disposition("form-data")
        .disposition
        .is_attachment());
    assert!(parse_content_disposition("x-custom")
        .disposition
        .is_attachment());
}

#[test]
fn test_raw_utf8_and_extension_tokens() {
    // Some senders put raw UTF-8 into quoted parameter values; keep it.
    let dis = parse_content_disposition("attachment; filename=\"报告.pdf\"");
    assert_eq!(dis.filename_full().as_deref(), Some("报告.pdf"));

    let dis = parse_content_disposition("x-custom; filename=x.bin");
    assert_eq!(
        dis.disposition,
        DispositionType::Extension("x-custom".to_string())
    );
    assert_eq!(dis.filename_full().as_deref(), Some("x.bin"));

    let dis = parse_content_disposition("attachment; filename=README");
    assert_eq!(dis.filename(), Some(("README".to_string(), None)));
}

#[test]
fn test_malformed_parameters_are_skipped() {
    let dis = parse_content_disposition("attachment; =; ; filename=ok.txt; no-value");
    assert_eq!(dis.disposition, DispositionType::Attachment);
    assert_eq!(dis.filename_full().as_deref(), Some("ok.txt"));
    assert_eq!(dis.params.len(), 1);

    let dis = parse_content_disposition("");
    assert_eq!(dis.disposition, DispositionType::Extension(String::new()));
    assert!(dis.params.is_empty());
}

#[test]
fn test_many_continuation_segments_are_ordered_numerically() {
    let mut header = String::from("attachment; filename*0*=UTF-8''%2E");
    for index in 1..12 {
        header.push_str(&format!("; filename*{}*=%2E", index));
    }
    let dis = parse_content_disposition(&header);
    assert_eq!(dis.filename_full().as_deref(), Some("............"));
}

#[test]
fn test_never_panics_on_odd_input() {
    let inputs = [
        "",
        ";",
        ";;;",
        "attachment",
        "attachment;",
        "attachment; filename",
        "attachment; filename=",
        "attachment; filename=\"",
        "attachment; filename=\"\\",
        "attachment; filename=\"a\\\"",
        "attachment; =",
        "attachment; =\"",
        "attachment; filename*=UTF-8''",
        "attachment; filename*=UTF-8''%",
        "attachment; filename*=''",
        "attachment; filename*=''''''",
        "attachment; filename*0*=",
        "attachment; filename**=UTF-8''a",
        "attachment; *0=foo",
        "attachment; *=",
        "attachment; 报告=\"文件;名\"",
        "attachment; filename=\"报告",
        "attachment; filename*0*=UTF-8''报; filename*1*=%E5%91%8A",
        "attachment; filename*=UTF-8''%E6%8A",
    ];
    for input in inputs {
        let _ = parse_content_disposition(input);
    }
}
