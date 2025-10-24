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

    let allocator = Bump::new();
    let mut cranelift_backend =
        CraneliftBackend::new("test_compile_expr", &allocator, &resolved.symtab);
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

// TODO: support interactive input
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
    dbg!(stdout_string);
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

#[test]
fn test_compile_expr() {
    let code = r#"
    int main(int argc, char *argv[]) {
        return 5 + 3 * 2 / (6 % 5);
    }
    "#;

    test_harness("expr", code, "", 11);
}

#[test]
fn test_compile_variables() {
    let code = r#"
    int main(int argc, char *argv[]) {
        int two;
        int three;
        two = 2;
        three = 3;
        return two + three;
    }
    "#;

    test_harness("variables", code, "", 5);
}

#[test]
fn test_compile_function_call() {
    let code = r#"
    int square_three() {
        return 3 * 3;
    }

    int main(int argc, char *argv[]) {
        return square_three();
    }
    "#;

    test_harness("function_call", code, "", 9);
}

#[test]
fn test_compile_string_literal() {
    let code = r#"
    int puts(const char *str);

    int main(int argc, char *argv[]) {
        puts("Hello world!");
        return 0;
    }
    "#;

    test_harness("string_literal", code, "Hello world!\n", 0);
}

#[test]
fn test_array() {
    let code = r#"
    int puts(const char *str);

    int main(int argc, char *argv[]) {
        char buf[8];
        buf[0] = 'a';
        buf[1] = 'r';
        buf[2] = 'g';
        buf[3] = 'c';
        buf[4] = ':';
        buf[5] = ' ';
        buf[6] = '0' + argc;
        buf[7] = '\0';

        puts(buf);
        
        return 0;
    }
    "#;
    test_harness("array", code, "argc: 1\n", 0);
}

#[test]
fn test_conditional() {
    let code = r#"
    int puts(const char *str);
    char *strcpy(char *dest, const char *src);

    int main(int argc, char *argv[]) {
        char buf[64];
        strcpy(buf, "argc=4? x");
        
        if (argc == 4) {
            buf[8] = 'y';
        }
        else {
            buf[8] = 'n';
        }
        puts(buf);
        return 0;
    }
    "#;

    let test_name = "conditional";
    compile_code(test_name, code);
    run_code(test_name, "", &[], "argc=4? n\n", 0);
    run_code(test_name, "", &["x", "x", "x"], "argc=4? y\n", 0);
}

#[test]
fn test_complicated_conditional() {
    let code = r#"
    int main(int argc, char *argv[]) {
        if (0) {
        }
        else if (1) {
        }
        else {
            return 15;
        }
        return 0;
    }
    "#;

    compile_code("complicated_conditional", code);
}

#[test]
fn test_while() {
    let code = r#"
    int puts(const char *str);
    void my_strcpy(char *dest, const char *src) {
        while (*src) {
            *dest = *src;
            dest = dest + 1;
            src = src + 1;
        }
    }

    int main(int argc, char *argv[]) {
        char buf[32];
        my_strcpy(buf, "hello from my_strcpy");
        puts(buf);
        return 0;
    }
    "#;

    test_harness("while", code, "hello from my_strcpy\n", 0);
}

#[test]
fn test_calculator() {
    let code = r#"
    int puts(const char *str);
    char *gets(char *str);
    char *strcpy(char *dest, const char *src);
    int strcmp(const char *str1, const char *str2);
    int atoi(const char *str);
    unsigned long strlen(const char *str);

    int add(int a, int b) {
        return a + b;
    }
    int subtract(int a, int b) {
        return a - b;
    }
    int multiply(int a, int b) {
        return a * b;
    }
    int divide(int a, int b) {
        return a / b;
    }

    int main(int argc, char *argv[]) {
        char buf[32];
        int a;
        int b;
        puts("Enter first number: ");
        gets(buf);
        a = atoi(buf);
        puts("Enter second number: ");
        gets(buf);
        b = atoi(buf);
        
        int (*calc)(int, int);
        
        puts("Choose operation (+, -, *, /): ");
        gets(buf);

        if (strcmp(buf, "+") == 0) {
            calc = &add;
        }
        else if (strcmp(buf, "-") == 0) {
            calc = &subtract;
        }
        else if (strcmp(buf, "*") == 0) {
            calc = &multiply;
        }
        else if (strcmp(buf, "/") == 0) {
            calc = &divide;
        }
        else {
            return 0 - 1;
        }

        int answer;
        answer = calc(a, b);
        
        unsigned long idx;
        strcpy(buf, "The answer is: ");
        idx = strlen(buf);

        do {
            int lsd;
            lsd = answer % 10;
            answer = answer / 10;
            buf[idx] = '0' + lsd;
            idx = idx + 1;
        } while (answer);
        buf[idx] = '\0';

        puts(buf);

        return 0;
    }
    "#;

    compile_code("calculator", code);
}

#[test]
fn test_short_circuit() {
    let code = r#"
    int puts(const char *str);
    int return_false_and_print() {
        puts("false");
        return 0;
    }

    int return_true_and_print() {
        puts("true");
        return 1;
    }

    int main(int argc, char *argv[]) {
        if (return_false_and_print() && return_true_and_print()) {
        }
        puts("sep");
        if (return_true_and_print() || return_false_and_print()) {
        }
        return 0;
    }
    "#;

    let test_name = "short_circuit";
    compile_code(test_name, code);
    run_code(test_name, "", &[], "false\nsep\ntrue\n", 0);
}

#[test]
fn test_for_loop() {
    let code = r#"
    int puts(const char *str);
    void my_strcpy_for(char *dest, char *src) {
        char *tmp;
        for (tmp = src; *tmp != '\0'; tmp = tmp + 1, dest = dest + 1) {
            *dest = *tmp;
        }
        *dest = '\0';
    }

    int main(int argc, char *argv[]) {
        char buf[32];

        my_strcpy_for(buf, "Hello world!");
        puts(buf);
        return 0;
    }
    "#;

    let test_name = "for_loop";
    compile_code(test_name, code);
    run_code(test_name, "", &[], "Hello world!\n", 0);
}

#[test]
fn test_continue_break() {
    let code = r#"
    int puts(const char *str);
    void itoa(int value, char *buf) {
        int value_copy;
        value_copy = value;
        
        int digits;
        digits = 0;

        do {
            value_copy = value_copy / 10;
            digits = digits + 1;
        } while (value_copy);

        buf[digits] = '\0';
        do {
            int lsd;
            lsd = value % 10;
            value = value / 10;
            buf[digits - 1] = '0' + lsd;
            digits = digits - 1;
        } while (value);
    }

    int main(int argc, char *argv[]) {
        int i;
        char buf[32];
        i = 0;
        while (1) {
            itoa(i, buf);
            if (i > 10) {
                break;
            }

            if (i % 2 == 1) {
                i = i + 1;
                continue;
            }

            puts(buf);
            i = i + 1;
        }

        for (i = 0;; i = i + 1) {
            itoa(i, buf);
            if (i > 10) {
                break;
            }
            
            if (i % 2 == 0) {
                continue;
            }

            puts(buf);
        }

        return 0;
    }
    "#;

    let test_name = "continue_break";
    compile_code(test_name, code);
    run_code(test_name, "", &[], "0\n2\n4\n6\n8\n10\n1\n3\n5\n7\n9\n", 0);
}

#[test]
fn test_nested_continue() {
    let code = r#"
    int puts(const char *str);
    void itoa(int value, char *buf) {
        int value_copy;
        value_copy = value;
        
        int digits;
        digits = 0;

        do {
            value_copy = value_copy / 10;
            digits = digits + 1;
        } while (value_copy);

        buf[digits] = '\0';
        do {
            int lsd;
            lsd = value % 10;
            value = value / 10;
            buf[digits - 1] = '0' + lsd;
            digits = digits - 1;
        } while (value);
    }

    int main(int argc, char *argv[]) {
        char buf[16];
        int sum;
        sum = 0;
        
        int i, j;
        for (i = 0; i < 10; i = i + 1) {
            if (i % 2 == 0) {
                continue;
            }
            for (j = 0; j < 10; j = j + 1) {
                if (j % 2 == 0) {
                    continue;
                }
                sum = sum + i * 10 + j;
            }
        }

        itoa(sum, buf);
        puts(buf);
        return 0;
    }
    "#;

    let test_name = "nested_continue";
    compile_code(test_name, code);
    run_code(test_name, "", &[], "1375\n", 0);
}

#[test]
fn test_switch() {
    let code = r#"
    int puts(const char *str);
    char *gets(char *str);

    int main() {
        char buf[16];
        gets(buf);

        int n;
        n = buf[0] - '0';

        switch (n) {
            case 1:
                puts("one!");
                break;
            case 2:
                puts("two!");
                break;
            case 3:
                puts("three!");
                break;
            default:
                puts("nope");
                break;
        }

        return 0;
    }
    "#;

    let test_name = "switch";
    compile_code(test_name, code);
    run_code(test_name, "1\n", &[], "one!\n", 0);
    run_code(test_name, "2\n", &[], "two!\n", 0);
    run_code(test_name, "3\n", &[], "three!\n", 0);
    run_code(test_name, "4\n", &[], "nope\n", 0);
}

#[test]
fn test_varargs_call() {
    let code = r#"
    int printf(const char *fmt, ...);
    int main(int argc, char *argv[]) {
        const char *hello;
        hello = "hello!";
        printf("argc: %d, %s", argc, hello);
        return 0;
    }
    "#;

    test_harness("varargs_call", code, "argc: 1, hello!", 0);
}

#[test]
fn test_globals() {
    let code = r#"
    int printf(const char *fmt, ...);
    int counter;
    
    int fetch_add() {
        int old_counter;
        old_counter = counter;
        counter = counter + 1;
        return old_counter;
    }

    int main(int argc, char *argv[]) {
        int x;
        x = fetch_add();
        printf("x: %d, ", x);
        x = fetch_add();
        printf("x: %d, ", x);
        return 0;
    }
    "#;

    test_harness("globals", code, "x: 0, x: 1, ", 0);
}

#[test]
fn test_augmented_assignment() {
    let code = r#"
    int printf(const char *fmt, ...);
    void my_strcpy(char *dest, const char *src) {
        while (*src) {
            *(dest++) = *(src++);
        }
        *dest = '\0';
    }
    int main(int argc, char *argv[]) {
        int i;
        int j;
        j = 1;
        for (i = 0; i < 10; i += 1) {
            j *= 2;
        }
        printf("%d ", j);
        
        int x;
        x = 10;
        printf("%d ", ++x);
        printf("%d ", x++);

        char buf[64];
        my_strcpy(buf, "test test");
        printf("%s", buf);

        return x;
    }
    "#;

    test_harness("augmented_assignment", code, "1024 11 11 test test", 12);
}

#[test]
fn test_initializer() {
    let code = r#"
    int main(int argc, char *argv[]) {
        int inited = 7;
        return inited;
    }
    "#;

    test_harness("initializer", code, "", 7);
}

#[test]
fn test_struct() {
    let code = r#"
    int printf(const char *fmt, ...);
    struct data {
        int a;
        float b;
    };
    
    int main() {
        struct data x;
        x.a = 5;
        x.b = 2.0f;

        printf("x.a = %d, x.b = %.1f", x.a, x.b);

        return 0;
    }
    "#;

    test_harness("struct", code, "x.a = 5, x.b = 2.0", 0);
}

#[test]
fn test_sizeof() {
    let code = r#"
    struct data {
        int a;
        float b;
    };
    
    int main() {
        int i;
        float *p;
        char a[52];
        struct data s;
        struct data sa[18];

        printf("%d %d %d %d %d %d %d %d %d", 
            sizeof(int), 
            sizeof i, 
            sizeof(void *), 
            sizeof p, 
            sizeof *p,
            sizeof (char[52]),
            sizeof a,
            sizeof (struct data),
            sizeof s,
            sizeof (struct data [18]),
            sizeof sa
        );

        return 0;
    }
    "#;

    test_harness("sizeof", code, "4 4 8 8 4 52 52 8 8 144 144", 0);
}
