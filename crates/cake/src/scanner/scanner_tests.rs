use lexeme_sets::c_preprocessor::CPreprocessor;

use crate::scanner::table_scanner::DFAScanner;

use super::lexeme_sets;

#[test]
fn test_basic() {
    let mut scanner = DFAScanner::build_lexeme_set_scanner::<CPreprocessor>();
    scanner.scan_string(
        "#include <stdio.h> void main() { int x = 0; while (x != 999) x++; return x; }",
    );
}

#[test]
fn test_string_literal() {
    let test = r#"#include "test.h""#;
    let mut scanner = DFAScanner::load_lexeme_set_scanner::<CPreprocessor>();
    scanner.scan_string(test);
}
