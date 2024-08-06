use cake::scanner::fa::FA;
use cake::scanner::regex::Regex;
use cake::scanner::alphabet::AsciiChar;

fn compile_regex(re_str: &str, quiet: bool) -> FA<AsciiChar> {
    let regex = Regex::from_str(re_str).expect("failed to parse regex");
    if !quiet { println!("regex = {:?}", regex); }

    let nfa = FA::nfa_from_re(&regex);
    if !quiet { println!("nfa = {:?}", nfa); }

    let dfa = FA::dfa_from_nfa(&nfa);
    if !quiet { println!("dfa = {:?}", dfa); }

    let dfa = FA::minimize_dfa(&dfa, false);
    if !quiet { println!("minimized = {:?}", dfa); }

    dfa
}

fn run_vectors(tests: &Vec<(&str, bool)>, dfa: &FA<AsciiChar>, re_str: &str) {
    for (test, expected_result) in tests {
        let result = FA::simulate_dfa(&dfa, test);
        assert_eq!(result, *expected_result, 
            "'{}' failed on input '{}', expect match: {}, actual match: {}",
            re_str,
            test,
            expected_result,
            result
        );
    }
}

#[test]
fn basic() {
    let re_str = r"a(b|c)*";
    let dfa = compile_regex(re_str, false);

    let test_vectors = vec![
        ("a", true),
        ("b", false),
        ("x", false),
        ("ab", true),
        ("ac", true),
        ("abcbc", true),
        ("acbcb", true),
        ("bcbc", false),
        ("abbbbbbbbbb", true)
    ];

    run_vectors(&test_vectors, &dfa, re_str)
}

#[test]
fn basic_extended_regex() {
    let re_str = r"[a-fA-Z]+";
    let dfa = compile_regex(re_str, false);

    let test_vectors = vec![
        ("a", true),
        ("b", true),
        ("x", false),
        ("ABCDEF", true),
        ("", false),
        ("g", false),
        ("GG", true),
        ("1234", false),
        ("qqqqq", false)
    ];

    run_vectors(&test_vectors, &dfa, re_str)
}

#[test]
fn optional_regex() {
    let re_str = r"(a*b)?c";
    let _dfa = compile_regex(re_str, false);
}

#[test]
fn floating_point_regex() {
    let re_str = r"[\-+]?([0-9]*\.)?[0-9]+([eE][\-+]?[0-9]+)?";
    let dfa = compile_regex(re_str, false);

    let test_vectors = vec![
        ("a", false),
        ("b", false),
        ("x", false),
        ("ABCDEF", false),
        ("", false),
        ("g", false),
        ("GG", false),
        ("1234", true),
        ("qqqqq", false),
        ("-99.99e12", true),
        ("-99.99e1a2", false),
        ("3..14", false),
        ("314.1592e-2", true)
    ];

    run_vectors(&test_vectors, &dfa, re_str)
}