use cake::scanner::FA;
use cake::scanner::regex::Regex;

#[test]
fn basic() {
    let re_str = r"a(b|c)*";

    let regex = Regex::from_str(re_str).expect("failed to parse regex");
    println!("regex = {:?}", regex);

    let nfa = FA::nfa_from_re(&regex);
    println!("nfa = {:?}", nfa);

    let dfa = FA::dfa_from_nfa(&nfa);
    println!("dfa = {:?}", dfa);

    let dfa = FA::minimize_dfa(&dfa, false);
    println!("minimized = {:?}", dfa);

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

    for (test, expected_result) in test_vectors {
        let result = FA::simulate_dfa(&dfa, test);
        assert_eq!(result, expected_result, 
            "'{}' failed on input '{}', expect match: {}, actual match: {}",
            re_str,
            test,
            expected_result,
            result
        );
    }
}