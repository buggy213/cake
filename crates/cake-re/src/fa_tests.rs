use crate::fa::FA;
use crate::regex::Regex;

fn compile_regex(re_str: &str, quiet: bool) -> FA {
    let regex = Regex::from_str(re_str).expect("failed to parse regex");
    if !quiet {
        println!("regex = {:?}", regex);
    }

    let nfa = FA::nfa_from_re(&regex);
    if !quiet {
        println!("nfa = {:?}", nfa);
    }

    let dfa = FA::dfa_from_nfa(&nfa);
    if !quiet {
        println!("dfa = {:?}", dfa);
    }

    let dfa = FA::minimize_dfa(&dfa, false);
    if !quiet {
        println!("minimized = {:?}", dfa);
    }

    dfa
}

fn compile_regex_set(re_strs: &[&str]) -> FA {
    let regexes: Vec<Regex> = re_strs
        .iter()
        .map(|re_str| Regex::from_str(re_str).expect("failed to compile regex"))
        .collect();

    let combined_nfa = FA::combine_res(&regexes);

    let dfa = FA::dfa_from_nfa(&combined_nfa);

    let dfa = FA::minimize_dfa(&dfa, true);

    dfa
}

fn run_vectors(tests: &Vec<(&str, bool)>, dfa: &FA, re_str: &str) {
    for (test, expected_result) in tests {
        let (result, _) = FA::simulate_dfa(&dfa, test);
        assert_eq!(
            result, *expected_result,
            "'{}' failed on input '{}', expect match: {}, actual match: {}",
            re_str, test, expected_result, result
        );
    }
}

fn run_vectors_multi_action(tests: &Vec<(&str, i32)>, dfa: &FA, re_strs: &[&str]) {
    for (test, expected_action) in tests {
        let (_, action) = FA::simulate_dfa(&dfa, test);
        let action = action.expect("must run with multi-action DFA");
        assert_eq!(
            action, *expected_action,
            "'{:?}' failed on input '{}', expected action: {}, actual action: {}",
            re_strs, test, expected_action, action
        )
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
        ("abbbbbbbbbb", true),
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
        ("qqqqq", false),
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
        ("314.1592e-2", true),
    ];

    run_vectors(&test_vectors, &dfa, re_str)
}

#[test]
fn is_preprocessing_number() {
    let re_str = r"\.?[0-9]([_a-zA-Z0-9]|[eEpP][+\-]|\.)*";
    let dfa = compile_regex(re_str, true);

    let test_vectors = vec![
        ("1Ex", true),
        ("1E1", true),
        ("42", true),
        ("0.442.442e+p+10", true),
        ("int", false),
    ];

    run_vectors(&test_vectors, &dfa, re_str);
}

#[test]
fn multiple_regex_priority() {
    let re_strs = [r"ab+", r"cd+", r"(cdd)|(ef)"];

    let dfa = compile_regex_set(&re_strs);

    let test_vectors = vec![
        ("xx", -1),
        ("abbbbb", 0),
        ("cdd", 1),
        ("cd", 1),
        ("ef", 2),
        ("bad", -1),
    ];

    run_vectors_multi_action(&test_vectors, &dfa, &re_strs);
}
