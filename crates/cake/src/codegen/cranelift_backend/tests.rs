use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

use bumpalo::Bump;
use cranelift::prelude::FunctionBuilderContext;

use crate::semantics::resolver::resolve_ast_tests::{ResolveHarnessInput, resolve_harness};

use super::CraneliftBackend;

fn test_artifact_dir(test_name: &'static str) -> PathBuf {
    let workspace = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| std::env::temp_dir());

    let test_artifacts = workspace.join("test_artifacts");
    let test_artifact = test_artifacts.join(test_name);
    // error is expected if it already exists
    _ = std::fs::create_dir_all(&test_artifact);
    test_artifact
}

fn compile_code(test_name: &'static str, code: &'static str) {
    let working_dir = test_artifact_dir(test_name);

    let input = ResolveHarnessInput { code };
    let resolved = resolve_harness(input);

    let resolved_ast_file = working_dir.join("resolved_ast");
    dbg!(&resolved);
    std::fs::write(&resolved_ast_file, format!("{:#?}", &resolved))
        .expect("failed to write resolved AST to file");

    let mut cranelift_backend = CraneliftBackend::new("test_compile_expr",  &resolved.symtab);
    let mut function_builder_ctx = FunctionBuilderContext::new();
    cranelift_backend.lower_translation_unit(&resolved, &mut function_builder_ctx);

    let object_file_name = format!("{test_name}.o");
    let object_file = working_dir.join(object_file_name);
    cranelift_backend.finish_and_write(&object_file);

    let binary_file = working_dir.join(test_name);
    let mut compile_command = Command::new("clang");
    compile_command
        .arg("-no-pie") // cranelift is not generating position independent assembly
        .arg("-o")
        .arg(binary_file)
        .arg(object_file);

    compile_command
        .spawn()
        .expect("unable to launch compiler")
        .wait()
        .expect("it didn't run");

    // generate reference binary using clang, for differential testing
    /*
    let code_file = working_dir.join("code.c");
    let binary_file_name_clang = format!("{test_name}.clang");
    let binary_file_clang = working_dir.join(binary_file_name_clang);
    let mut compile_command = Command::new("clang");
    compile_command
        .arg("-no-pie") // cranelift is not generating position independent assembly
        .arg("-o")
        .arg(binary_file_clang)
        .arg(code_file);

    compile_command
        .spawn()
        .expect("unable to launch compiler")
        .wait()
        .expect("it didn't run");
    */
}

fn run_code(
    test_name: &'static str,
    stdin: &'static str,
    args: &'static [&'static str],
    expected_stdout: &'static str,
    expected_exit: i32,
) {
    let working_dir = test_artifact_dir(test_name);
    let executable = working_dir.join(test_name);

    let mut executable_command = Command::new(executable);
    let executable_command = executable_command.args(args);
    let mut child = executable_command
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .stdin(Stdio::piped())
        .spawn()
        .expect("unable to launch executable");

    child
        .stdin
        .take()
        .expect("child has no stdin")
        .write(stdin.as_bytes())
        .expect("failed to write to stdin");

    let output = child.wait_with_output().expect("failed to wait on child");

    let stdout_string =
        String::from_utf8(output.stdout.clone()).expect("stdout contained weird chars");
    println!("{}", &stdout_string);
    dbg!(output.status.code());

    assert_eq!(output.stdout.as_slice(), expected_stdout.as_bytes());
    assert_eq!(
        output.status.code().expect("unexpected signal"),
        expected_exit
    );
}

fn test_harness(
    test_name: &'static str,
    code: &'static str,
    expected_stdout: &'static str,
    expected_exit: i32,
) {
    compile_code(test_name, code);
    run_code(test_name, "", &[], expected_stdout, expected_exit);
}

macro_rules! make_test {
    // expected output format
    // 1st line is expected exit code, all subsequent lines are expected stdout
    ($name:ident) => {
        paste::paste! {
            #[test]
            fn [<test_$name>]() {
                let code = include_str!(concat!("test_data/", stringify!($name), ".c"));
                let expected = include_str!(concat!("test_data/", stringify!($name), ".out"));
                let (expected_exit, expected_stdout) = expected.split_once("\n").expect("malformed test output specification");
                let expected_exit = expected_exit.parse::<i32>().expect("malformed test output specification");
                test_harness(stringify!($name), code, expected_stdout, expected_exit);
            }
        }
    };

    // define argv, stdin, stdout, and exit manually
    ($name:ident, $(($argv:expr, $stdin:literal, $stdout:literal, $exit:literal)),+) => {
        paste::paste! {
            #[test]
            fn [<test_$name>]() {
                let code = include_str!(concat!("test_data/", stringify!($name), ".c"));
                compile_code(stringify!($name), code);
                $(
                    run_code(stringify!($name), $stdin, $argv, $stdout, $exit);
                )+
            }
        }
    };
}

make_test!(compile_expr);
make_test!(compile_variables);
make_test!(compile_function_call);
make_test!(compile_string_literal);
make_test!(array);
make_test!(conditional, 
    (&[], "", "argc=4? n\n", 0),
    (&["x", "x", "x"], "", "argc=4? y\n", 0)
);
make_test!(complicated_conditional);
make_test!(while);
make_test!(calculator,
    (&[], "3\n4\n+\n", "Enter first number: \nEnter second number: \nChoose operation (+, -, *, /): \nThe answer is: 7\n", 0),
    (&[], "9\n2\n-\n", "Enter first number: \nEnter second number: \nChoose operation (+, -, *, /): \nThe answer is: 7\n", 0),
    (&[], "4\n3\n*\n", "Enter first number: \nEnter second number: \nChoose operation (+, -, *, /): \nThe answer is: 12\n", 0),
    (&[], "20\n4\n/\n", "Enter first number: \nEnter second number: \nChoose operation (+, -, *, /): \nThe answer is: 5\n", 0)
);
make_test!(short_circuit);
make_test!(for_loop);
make_test!(continue_break);
make_test!(nested_continue);
make_test!(switch,
    (&[], "1\n", "one!\n", 0),
    (&[], "2\n", "two!\n", 0),
    (&[], "3\n", "three!\n", 0),
    (&[], "4\n", "nope\n", 0)
);
make_test!(varargs_call);
make_test!(globals);
make_test!(augmented_assignment);
make_test!(initializer);
make_test!(struct);
make_test!(struct_assign);
make_test!(struct_param);
make_test!(union_punning);
make_test!(multi_array);
make_test!(sizeof);
