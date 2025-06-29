/// tests are stored in this directory as well
use std::{fs, path::PathBuf};

use crate::{platform::Platform, scanner::lexemes::LexemeSet};

use super::{Preprocessor, TokenStream};

fn tokenize_all<T: LexemeSet>(mut tokenizer: impl TokenStream<T>, delimiter: &str) -> String {
    let mut tokenized = String::new();

    while let Some((_, token, _)) = tokenizer.advance() {
        tokenized.push_str(token);
        tokenized.push_str(delimiter);
    }

    tokenized
}

fn text_test_harness(filename: &str) -> (Preprocessor, PathBuf) {
    const RELATIVE_PATH: &str = "src/scanner/preprocessor/preprocessor_tests";
    let working_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(RELATIVE_PATH);
    let file = working_dir.clone().join(filename);
    let contents = fs::read_to_string(&file).expect("filename in test should be valid");
    let pp = Preprocessor::new(file, contents, Platform::new(working_dir.clone()));
    (pp, working_dir)
}

#[test]
fn test_conditional_inclusion() {
    let (pp, working_dir) = text_test_harness("conditional_inclusion.c");
    let tokenized = tokenize_all(pp, " ");
    fs::write(working_dir.join("conditional_inclusion_out.c"), tokenized).expect("failed to write");
}

#[test]
fn test_basic_macro() {
    let (pp, working_dir) = text_test_harness("basic_macro.c");
    let tokenized = tokenize_all(pp, " ");
    fs::write(working_dir.join("basic_macro_out.c"), tokenized).expect("failed to write");
}

#[test]
fn test_basic_include() {
    let (pp, working_dir) = text_test_harness("basic_include.c");
    let tokenized = tokenize_all(pp, " ");
    fs::write(working_dir.join("basic_include_out.c"), tokenized).expect("failed to write");
}

#[test]
fn test_remove_comment() {
    let (pp, working_dir) = text_test_harness("remove_comments.c");
    let tokenized = tokenize_all(pp, " ");
    fs::write(working_dir.join("remove_comments_out.c"), tokenized).expect("failed to write");
}
