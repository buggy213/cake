//! Test inputs live in this directory alongside their expected output.

use std::{fs, path::PathBuf};

use crate::platform::Platform;

use super::{Preprocessor, TokenStream};

/// Runs the preprocessor over `filename` and returns its tokens, separated by
/// spaces, in the same shape as the `_out.c` files.
fn preprocess(filename: &str) -> String {
    const RELATIVE_PATH: &str = "src/scanner/preprocessor/preprocessor_tests";
    let working_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(RELATIVE_PATH);
    let file = working_dir.join(filename);
    let contents = fs::read_to_string(&file).expect("filename in test should be valid");

    let mut pp = Preprocessor::new(file, contents, Platform::new(working_dir));
    let mut tokens: Vec<String> = Vec::new();
    while let Some((_, token)) = pp.advance() {
        tokens.push(pp.text(token).to_string());
    }

    if let Some(error) = pp.take_error() {
        panic!("preprocessing {filename} failed: {error}");
    }

    tokens.join(" ")
}

/// Compares against the expected output committed next to the input.
fn check(filename: &str, expected_filename: &str) {
    const RELATIVE_PATH: &str = "src/scanner/preprocessor/preprocessor_tests";
    let working_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(RELATIVE_PATH);
    let expected = fs::read_to_string(working_dir.join(expected_filename))
        .expect("expected output should exist");

    assert_eq!(preprocess(filename), expected.trim());
}

#[test]
fn test_conditional_inclusion() {
    check("conditional_inclusion.c", "conditional_inclusion_out.c");
}

#[test]
fn test_basic_macro() {
    check("basic_macro.c", "basic_macro_out.c");
}

#[test]
fn test_basic_include() {
    check("basic_include.c", "basic_include_out.c");
}

#[test]
fn test_remove_comment() {
    check("remove_comments.c", "remove_comments_out.c");
}

/// Preprocesses `source` written to a temporary file in the test directory.
fn preprocess_source(name: &str, source: &str) -> String {
    const RELATIVE_PATH: &str = "src/scanner/preprocessor/preprocessor_tests";
    let working_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(RELATIVE_PATH);
    let file = working_dir.join(name);
    fs::write(&file, source).expect("failed to write test source");

    let mut pp = Preprocessor::new(file.clone(), source.to_string(), Platform::new(working_dir));
    let mut tokens: Vec<String> = Vec::new();
    while let Some((_, token)) = pp.advance() {
        tokens.push(pp.text(token).to_string());
    }
    let error = pp.take_error();
    let _ = fs::remove_file(&file);

    if let Some(error) = error {
        panic!("preprocessing failed: {error}");
    }

    tokens.join(" ")
}

#[test]
fn test_object_macro_is_not_expanded_recursively() {
    let out = preprocess_source("recursive_macro.c", "#define foo foo bar\nfoo;\n");
    assert_eq!(out, "foo bar ;");
}

#[test]
fn test_function_macro_invocation_spanning_lines() {
    let source = "#define add(a, b) ((a) + (b))\nint x = add(1,\n              2);\n";
    let out = preprocess_source("multiline_macro.c", source);
    assert_eq!(out, "int x = ( ( 1 ) + ( 2 ) ) ;");
}

#[test]
fn test_macro_name_without_arguments_is_not_expanded() {
    let source = "#define f(x) (x)\nint g = f;\n";
    let out = preprocess_source("bare_macro_name.c", source);
    assert_eq!(out, "int g = f ;");
}

#[test]
fn test_stringify_and_paste() {
    let source = concat!(
        "#define str(x) #x\n",
        "#define cat(a, b) a ## b\n",
        "char *s = str(1 + 2);\n",
        "int cat(foo, bar) = 0;\n"
    );
    let out = preprocess_source("stringify_paste.c", source);
    assert_eq!(out, "char * s = \"1 + 2\" ; int foobar = 0 ;");
}

#[test]
fn test_nested_conditionals() {
    let source = concat!(
        "#define A 1\n",
        "#if defined(A) && A > 0\n",
        "  #if A > 5\n",
        "    int big;\n",
        "  #elif A > 0\n",
        "    int small;\n",
        "  #endif\n",
        "#else\n",
        "  int none;\n",
        "#endif\n"
    );
    let out = preprocess_source("nested_conditionals.c", source);
    assert_eq!(out, "int small ;");
}

#[test]
fn test_skipped_branch_is_not_evaluated() {
    // the `#if` inside the dead branch would divide by zero if evaluated
    let source = concat!(
        "#if 0\n",
        "#if 1 / 0\n",
        "int bad;\n",
        "#endif\n",
        "#else\n",
        "int good;\n",
        "#endif\n"
    );
    let out = preprocess_source("dead_branch.c", source);
    assert_eq!(out, "int good ;");
}

#[test]
fn test_adjacent_string_literals_are_concatenated() {
    let source = "char *s = \"foo\" \"bar\" \"baz\";\n";
    let out = preprocess_source("string_concat.c", source);
    assert_eq!(out, "char * s = \"foobarbaz\" ;");
}

#[test]
fn test_undef() {
    let source = "#define X 1\n#undef X\nint x = X;\n";
    let out = preprocess_source("undef.c", source);
    assert_eq!(out, "int x = X ;");
}

#[test]
fn test_variadic_macro() {
    let source = "#define call(f, ...) f(__VA_ARGS__)\ncall(g, 1, 2);\n";
    let out = preprocess_source("variadic.c", source);
    assert_eq!(out, "g ( 1 , 2 ) ;");
}

#[test]
fn test_line_splice() {
    let source = "#define long_macro 1 + \\\n  2\nint x = long_macro;\n";
    let out = preprocess_source("splice.c", source);
    assert_eq!(out, "int x = 1 + 2 ;");
}
