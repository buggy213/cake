use super::*;
use crate::scanner::table_scanner::DFAScanner;

fn text_test_harness<'text>(text: &'text str) -> (CTokenStream<'text>, ParserState) {
    let scanner = DFAScanner::load_lexeme_set_scanner::<CLexemes>();
    let toks = CTokenStream::new(scanner, text.as_bytes());
    let state = ParserState::new();

    (toks, state)
}

fn assert_types_eq(actual: &ParserState, expected: &ParserState) {
    assert_eq!(actual.enum_types, expected.enum_types);
    assert_eq!(actual.structure_types, expected.structure_types);
    assert_eq!(actual.union_types, expected.union_types);
    assert_eq!(actual.function_types, expected.function_types);
}

fn make_identifier(state: &mut ParserState, name: &str) -> ExpressionNode {
    let ident = Identifier::new(state.current_scope, name.to_string());
    ExpressionNode::Identifier(ident)
}

macro_rules! make_expr {
    ($expr_type:path, $($subexpr:expr),+) => {
        $expr_type(
            $(Box::new($subexpr)),+,

        )
    };
}

macro_rules! make_constant {
    ($constant_type:path, $value:expr) => {
        ExpressionNode::Constant($constant_type($value))
    };
}

#[test]
fn test_parse_expr_basic() {
    let basic_expr = r#"
    a = a + 1
    "#;
    let (mut toks, mut state) = text_test_harness(basic_expr);

    let lhs = Box::new(make_identifier(&mut state, "a"));

    let rhs = {
        let lhs = lhs.clone();
        let rhs = ExpressionNode::Constant(Constant::Int(1));
        Box::new(ExpressionNode::Add(lhs, Box::new(rhs)))
    };

    let parsed = parse_expr(&mut toks, &mut state).expect("failed to parse");
    assert_eq!(parsed, ExpressionNode::SimpleAssign(lhs, rhs));
}

#[test]
fn test_parse_expr_precedence() {
    let basic_expr = r#"
    a || b && c + d / e
    "#;
    let (mut toks, mut state) = text_test_harness(basic_expr);

    // (a || (b && (c + (d / e))))
    let a = make_identifier(&mut state, "a");
    let b = make_identifier(&mut state, "b");
    let c = make_identifier(&mut state, "c");
    let d = make_identifier(&mut state, "d");
    let e = make_identifier(&mut state, "e");

    let expr = {
        let rhs = {
            let rhs = {
                let rhs = make_expr!(ExpressionNode::Divide, d, e);
                make_expr!(ExpressionNode::Add, c, rhs)
            };
            make_expr!(ExpressionNode::LogicalAnd, b, rhs)
        };
        make_expr!(ExpressionNode::LogicalOr, a, rhs)
    };

    let parsed = parse_expr(&mut toks, &mut state).expect("failed to parse");
    assert_eq!(parsed, expr);
}

#[test]
fn test_parse_expr_parenthesized() {
    let basic_expr = r#"
    a * (b + c)
    "#;
    let (mut toks, mut state) = text_test_harness(basic_expr);

    // (a || (b && (c + (d / e))))
    let a = make_identifier(&mut state, "a");
    let b = make_identifier(&mut state, "b");
    let c = make_identifier(&mut state, "c");

    let expr = {
        let rhs = { make_expr!(ExpressionNode::Add, b, c) };
        make_expr!(ExpressionNode::Multiply, a, rhs)
    };

    let parsed = parse_expr(&mut toks, &mut state).expect("failed to parse");
    assert_eq!(parsed, expr);
}

#[test]
fn test_parse_expr_cast() {
    let basic_expr = r#"
    (const void *) (1 - 1)
    "#;
    let (mut toks, mut state) = text_test_harness(basic_expr);

    let expr = {
        let cast_target_type = {
            let const_void = CType::Void {
                qualifier: TypeQualifier::Const,
            };
            CType::PointerType {
                pointee_type: Box::new(const_void),
                qualifier: TypeQualifier::empty(),
            }
        };

        let casted_expr = {
            let lhs = make_constant!(Constant::Int, 1);
            let rhs = make_constant!(Constant::Int, 1);
            make_expr!(ExpressionNode::Subtract, lhs, rhs)
        };

        ExpressionNode::Cast(Box::new(casted_expr), cast_target_type)
    };

    let parsed = parse_expr(&mut toks, &mut state).expect("failed to parse");
    assert_eq!(parsed, expr);
}

#[test]
fn test_parse_expr_member_access() {
    let basic_expr = r#"
    a.b
    "#;
    let (mut toks, mut state) = text_test_harness(basic_expr);

    let a = make_identifier(&mut state, "a");
    let b_ident = Identifier::new(state.current_scope, "b".to_string());
    let expr = ExpressionNode::DotAccess(Box::new(a), b_ident);

    let parsed = parse_expr(&mut toks, &mut state).expect("failed to parse");
    assert_eq!(parsed, expr);
}

#[test]
fn test_parse_expr_arrow_access() {
    let basic_expr = r#"
    a->b
    "#;
    let (mut toks, mut state) = text_test_harness(basic_expr);

    let a = make_identifier(&mut state, "a");
    let b_ident = Identifier::new(state.current_scope, "b".to_string());
    let expr = ExpressionNode::ArrowAccess(Box::new(a), b_ident);

    let parsed = parse_expr(&mut toks, &mut state).expect("failed to parse");
    assert_eq!(parsed, expr);
}

#[test]
fn test_parse_expr_member_access_left_associativity() {
    let basic_expr = r#"
    a.b.c
    "#;
    let (mut toks, mut state) = text_test_harness(basic_expr);

    // Should parse as (a.b).c, not a.(b.c)
    let a = make_identifier(&mut state, "a");
    let b_ident = Identifier::new(state.current_scope, "b".to_string());
    let c_ident = Identifier::new(state.current_scope, "c".to_string());

    let inner_dot = ExpressionNode::DotAccess(Box::new(a), b_ident);
    let expr = ExpressionNode::DotAccess(Box::new(inner_dot), c_ident);

    let parsed = parse_expr(&mut toks, &mut state).expect("failed to parse");
    assert_eq!(parsed, expr);
}

#[test]
fn test_parse_expr_member_access_with_arithmetic() {
    let basic_expr = r#"
    a.b + c.d
    "#;
    let (mut toks, mut state) = text_test_harness(basic_expr);

    // Should parse as (a.b) + (c.d)
    let a = make_identifier(&mut state, "a");
    let b_ident = Identifier::new(state.current_scope, "b".to_string());
    let c = make_identifier(&mut state, "c");
    let d_ident = Identifier::new(state.current_scope, "d".to_string());

    let a_dot_b = ExpressionNode::DotAccess(Box::new(a), b_ident);
    let c_dot_d = ExpressionNode::DotAccess(Box::new(c), d_ident);
    let expr = ExpressionNode::Add(Box::new(a_dot_b), Box::new(c_dot_d));

    let parsed = parse_expr(&mut toks, &mut state).expect("failed to parse");
    assert_eq!(parsed, expr);
}

#[test]
fn test_parse_subscript_expr() {
    let basic_expr = r#"buf[0] = 'a'"#;
    let (mut toks, mut state) = text_test_harness(basic_expr);

    let expr = {
        let lhs = {
            let buf = make_identifier(&mut state, "buf");
            let index = make_constant!(Constant::Int, 0);
            make_expr!(ExpressionNode::ArraySubscript, buf, index)
        };

        let rhs = {
            let a = c"a".to_bytes()[0] as i32;
            make_constant!(Constant::Int, a)
        };

        make_expr!(ExpressionNode::SimpleAssign, lhs, rhs)
    };

    let parsed = parse_expr(&mut toks, &mut state).expect("failed to parse");
    assert_eq!(parsed, expr);
}

#[test]
fn test_parse_hello_world() {
    let hello_world = r#"
    int main(int argc, char **argv) {
        printf("Hello world!");
        return 0;
    }
    "#;

    let mut dummy_state = ParserState::new();
    let translation_unit = {
        let main = {
            dummy_state.open_scope(ScopeType::FunctionScope);
            let return_type = CType::BasicType {
                qualifier: TypeQualifier::empty(),
                basic_type: BasicType::Int,
            };

            let argc_type = CType::BasicType {
                qualifier: TypeQualifier::empty(),
                basic_type: BasicType::Int,
            };

            let argv_type = {
                let char_type = CType::BasicType {
                    qualifier: TypeQualifier::empty(),
                    basic_type: BasicType::Char,
                };

                let pointer_to_char_type = CType::PointerType {
                    qualifier: TypeQualifier::empty(),
                    pointee_type: Box::new(char_type),
                };

                CType::PointerType {
                    qualifier: TypeQualifier::empty(),
                    pointee_type: Box::new(pointer_to_char_type),
                }
            };

            let fn_type = FunctionType {
                parameter_types: vec![
                    (Some(String::from("argc")), argc_type),
                    (Some(String::from("argv")), argv_type),
                ],
                return_type: return_type,
                function_specifier: FunctionSpecifier::None,
                varargs: false,
                prototype_scope: dummy_state.current_scope,
            };

            let fn_type_idx = dummy_state.add_function_type(fn_type);

            let body = {
                dummy_state.open_scope(ScopeType::BlockScope);
                let printf = {
                    let printf_ident = make_identifier(&mut dummy_state, "printf");
                    let printf_arg = ExpressionNode::StringLiteral("Hello world!".to_string());
                    let printf_expr =
                        ExpressionNode::FunctionCall(Box::new(printf_ident), vec![printf_arg]);
                    ASTNode::ExpressionStatement(Box::new(printf_expr), dummy_state.current_scope)
                };
                let return_stmt = {
                    let zero = ExpressionNode::Constant(Constant::Int(0));
                    ASTNode::ReturnStatement(Some(Box::new(zero)), dummy_state.current_scope)
                };
                dummy_state.close_scope().unwrap();
                ASTNode::CompoundStatement(vec![printf, return_stmt], dummy_state.current_scope)
            };

            dummy_state.close_scope().unwrap();

            let fn_type = CType::FunctionTypeRef {
                symtab_idx: fn_type_idx,
            };

            let fn_declaration = Declaration::new(
                Identifier::new(dummy_state.current_scope, String::from("main")),
                fn_type,
                StorageClass::None,
                false,
                FunctionSpecifier::None,
                None,
            );

            ASTNode::FunctionDefinition(Box::new(fn_declaration), Box::new(body))
        };

        ASTNode::TranslationUnit(vec![main], dummy_state.current_scope)
    };

    let (mut toks, mut state) = text_test_harness(hello_world);
    let translation_unit_parsed =
        parse_translation_unit(&mut toks, &mut state).expect("parse failed");

    assert_eq!(translation_unit, translation_unit_parsed);
    assert_types_eq(&state, &dummy_state);
}

#[test]
fn test_parse_hello_world_unnamed_args() {
    let hello_world = r#"
    int main(int, char **) {
        printf("Hello world!");
        return 0;
    }
    "#;

    let mut dummy_state = ParserState::new();
    let translation_unit = {
        let main = {
            dummy_state.open_scope(ScopeType::FunctionScope);
            let return_type = CType::BasicType {
                qualifier: TypeQualifier::empty(),
                basic_type: BasicType::Int,
            };

            let argc_type = CType::BasicType {
                qualifier: TypeQualifier::empty(),
                basic_type: BasicType::Int,
            };

            let argv_type = {
                let char_type = CType::BasicType {
                    qualifier: TypeQualifier::empty(),
                    basic_type: BasicType::Char,
                };

                let pointer_to_char_type = CType::PointerType {
                    qualifier: TypeQualifier::empty(),
                    pointee_type: Box::new(char_type),
                };

                CType::PointerType {
                    qualifier: TypeQualifier::empty(),
                    pointee_type: Box::new(pointer_to_char_type),
                }
            };

            let fn_type = FunctionType {
                parameter_types: vec![(None, argc_type), (None, argv_type)],
                return_type: return_type,
                function_specifier: FunctionSpecifier::None,
                varargs: false,
                prototype_scope: dummy_state.current_scope,
            };

            let fn_type_idx = dummy_state.add_function_type(fn_type);

            let body = {
                dummy_state.open_scope(ScopeType::BlockScope);
                let printf = {
                    let printf_ident = make_identifier(&mut dummy_state, "printf");
                    let printf_arg = ExpressionNode::StringLiteral("Hello world!".to_string());
                    let printf_expr =
                        ExpressionNode::FunctionCall(Box::new(printf_ident), vec![printf_arg]);
                    ASTNode::ExpressionStatement(Box::new(printf_expr), dummy_state.current_scope)
                };
                let return_stmt = {
                    let zero = ExpressionNode::Constant(Constant::Int(0));
                    ASTNode::ReturnStatement(Some(Box::new(zero)), dummy_state.current_scope)
                };
                dummy_state.close_scope().unwrap();
                ASTNode::CompoundStatement(vec![printf, return_stmt], dummy_state.current_scope)
            };

            dummy_state.close_scope().unwrap();

            let fn_type = CType::FunctionTypeRef {
                symtab_idx: fn_type_idx,
            };

            let fn_declaration = Declaration::new(
                Identifier::new(dummy_state.current_scope, String::from("main")),
                fn_type,
                StorageClass::None,
                false,
                FunctionSpecifier::None,
                None,
            );

            ASTNode::FunctionDefinition(Box::new(fn_declaration), Box::new(body))
        };

        ASTNode::TranslationUnit(vec![main], dummy_state.current_scope)
    };

    let (mut toks, mut state) = text_test_harness(hello_world);
    let translation_unit_parsed =
        parse_translation_unit(&mut toks, &mut state).expect("parse failed");

    assert_eq!(translation_unit, translation_unit_parsed);
    assert_types_eq(&state, &dummy_state);
}

#[test]
fn parse_function_definition_bad_1() {
    // no arguments
    let function_bad_1 = r#"
    int main {
        printf("Hello world!");
        return 0;
    }
    "#;

    let (mut toks, mut state) = text_test_harness(&function_bad_1);
    assert!(parse_external_declaration(&mut toks, &mut state).is_err());
}

#[test]
fn parse_function_definition_bad_2() {
    // pointer to function
    let function_bad_2 = r#"
    int (*main)(int argc, char *argv[]) {
        printf("Hello world!");
        return 0;
    }
    "#;

    let (mut toks, mut state) = text_test_harness(&function_bad_2);
    assert!(parse_external_declaration(&mut toks, &mut state).is_err());
}

#[test]
fn parse_abstract_type_test_basic() {
    let abstract_type_str = r#"
    int
    "#;

    let abstract_type = CType::BasicType {
        qualifier: TypeQualifier::empty(),
        basic_type: BasicType::Int,
    };

    let (mut toks, mut state) = text_test_harness(&abstract_type_str);
    let base_type_parsed = parse_type_name(&mut toks, &mut state).expect("parse failed");
    assert_eq!(abstract_type, base_type_parsed);
}

// test lookahead to distinguish grouping parens in (abstract) declarator and function params
#[test]
fn parse_abstract_type_test_1() {
    let ptr_abstract_type_str = r#"
    int (*)
    "#;

    let base_type = CType::BasicType {
        qualifier: TypeQualifier::empty(),
        basic_type: BasicType::Int,
    };

    let abstract_type = CType::PointerType {
        qualifier: TypeQualifier::empty(),
        pointee_type: Box::new(base_type),
    };

    let (mut toks, mut state) = text_test_harness(&ptr_abstract_type_str);
    let abstract_type_parsed = parse_type_name(&mut toks, &mut state).expect("parse failed");
    assert_eq!(abstract_type, abstract_type_parsed);
}

// test lookahead to distinguish grouping parens in (abstract) declarator and function params
#[test]
fn parse_abstract_type_test_2() {
    let fn_abstract_type_str = r#"
    int ()
    "#;

    let base_type = CType::BasicType {
        qualifier: TypeQualifier::empty(),
        basic_type: BasicType::Int,
    };

    let mut dummy_state = ParserState::new();
    let abstract_type = {
        let fn_type_idx = {
            dummy_state.open_scope(ScopeType::FunctionScope);
            let fn_type = FunctionType {
                parameter_types: vec![],
                return_type: base_type,
                function_specifier: FunctionSpecifier::None,
                varargs: false,
                prototype_scope: dummy_state.current_scope,
            };

            dummy_state.close_scope().unwrap();
            dummy_state.add_function_type(fn_type)
        };
        CType::FunctionTypeRef {
            symtab_idx: fn_type_idx,
        }
    };

    let (mut toks, mut state) = text_test_harness(&fn_abstract_type_str);
    let abstract_type_parsed = parse_type_name(&mut toks, &mut state).expect("parse failed");

    assert_eq!(abstract_type, abstract_type_parsed);
    assert_types_eq(&state, &dummy_state);
}

#[test]
fn parse_abstract_type_test_3() {
    // "array of constant pointers to functions which take an unsigned int and varargs and return int"
    let abstract_type_str = r#"
    int (*const [])(unsigned int, ...)
    "#;

    let base_type = CType::BasicType {
        qualifier: TypeQualifier::empty(),
        basic_type: BasicType::Int,
    };
    let mut dummy_state = ParserState::new();
    let abstract_type = {
        let const_ptr = {
            let fn_type = {
                let uint_type = {
                    CType::BasicType {
                        qualifier: TypeQualifier::empty(),
                        basic_type: BasicType::UInt,
                    }
                };
                dummy_state.open_scope(ScopeType::FunctionScope);
                let anon_fn_type = FunctionType {
                    parameter_types: vec![(None, uint_type)],
                    return_type: base_type,
                    function_specifier: FunctionSpecifier::None,
                    varargs: true,
                    prototype_scope: dummy_state.current_scope,
                };

                dummy_state.close_scope().unwrap();
                let anon_fn_type_idx = dummy_state.add_function_type(anon_fn_type);

                CType::FunctionTypeRef {
                    symtab_idx: anon_fn_type_idx,
                }
            };

            CType::PointerType {
                qualifier: TypeQualifier::Const,
                pointee_type: Box::new(fn_type),
            }
        };

        CType::IncompleteArrayType {
            qualifier: TypeQualifier::empty(),
            element_type: Box::new(const_ptr),
        }
    };

    let (mut toks, mut state) = text_test_harness(&abstract_type_str);
    let abstract_type_parsed = parse_type_name(&mut toks, &mut state).expect("parse failed");

    assert_eq!(abstract_type, abstract_type_parsed);
    assert_types_eq(&state, &dummy_state);
}

#[test]
fn parse_struct_declaration_incomplete() {
    let incomplete_struct_str = r#"struct test s;"#;
    let mut dummy_state = ParserState::new();
    let struct_type = StructureType::new_incomplete_structure_type(String::from("test"));
    let struct_type_idx = dummy_state.add_structure_type(struct_type);
    let struct_type = CType::StructureTypeRef {
        qualifier: TypeQualifier::empty(),
        symtab_idx: struct_type_idx,
    };

    let (mut toks, mut state) = text_test_harness(&incomplete_struct_str);
    let declaration_parsed = parse_declaration(&mut toks, &mut state).expect("parse failed");
    let declaration_parsed_expected = ASTNode::Declaration(vec![Declaration::new(
        Identifier::new(dummy_state.current_scope, String::from("s")),
        struct_type,
        StorageClass::None,
        false,
        FunctionSpecifier::None,
        None,
    )]);

    assert_eq!(declaration_parsed, declaration_parsed_expected);
    assert_types_eq(&state, &dummy_state);
}

#[test]
fn parse_struct_declaration_anonymous() {
    let anonymous_struct = r#"
    struct {
        int k;
    } s;
    "#;

    let mut dummy_state = ParserState::new();
    let tag = None;
    let members = vec![(
        String::from("k"),
        CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::Int,
        },
    )];
    let struct_type = StructureType::new_complete_structure_type(tag, members);

    let struct_type_idx = dummy_state.add_structure_type(struct_type);
    let struct_type = CType::StructureTypeRef {
        qualifier: TypeQualifier::empty(),
        symtab_idx: struct_type_idx,
    };

    let (mut toks, mut state) = text_test_harness(&anonymous_struct);
    let declaration_parsed = parse_declaration(&mut toks, &mut state).expect("parse failed");
    let declaration_parsed_expected = ASTNode::Declaration(vec![Declaration::new(
        Identifier::new(dummy_state.current_scope, String::from("s")),
        struct_type,
        StorageClass::None,
        false,
        FunctionSpecifier::None,
        None,
    )]);

    assert_eq!(declaration_parsed, declaration_parsed_expected);
    assert_types_eq(&state, &dummy_state);
}

#[test]
fn parse_struct_declaration() {
    let complete_struct = r#"
    struct complete {
        int k;
    } t;
    "#;

    let mut dummy_state = ParserState::new();
    let tag = Some(String::from("complete"));
    let members = vec![(
        String::from("k"),
        CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::Int,
        },
    )];
    let struct_type = StructureType::new_complete_structure_type(tag, members);

    let struct_type_idx = dummy_state.add_structure_type(struct_type);
    let struct_type = CType::StructureTypeRef {
        qualifier: TypeQualifier::empty(),
        symtab_idx: struct_type_idx,
    };

    let (mut toks, mut state) = text_test_harness(&complete_struct);
    let declaration_parsed = parse_declaration(&mut toks, &mut state).expect("parse failed");
    let declaration_parsed_expected = ASTNode::Declaration(vec![Declaration::new(
        Identifier::new(dummy_state.current_scope, String::from("t")),
        struct_type,
        StorageClass::None,
        false,
        FunctionSpecifier::None,
        None,
    )]);

    assert_eq!(declaration_parsed, declaration_parsed_expected);
    assert_types_eq(&state, &dummy_state);
}

// parser does not check for directly / indirectly recursive struct (which would have infinite size)
#[test]
fn parse_struct_declaration_recursive() {
    let recursive_struct = r#"
    struct recursive {
        struct inner {
            int k;
        } inner;
        struct recursive* next;
    } recursive_struct;
    "#;

    let mut dummy_state = ParserState::new();
    let outer_struct_type = {
        let k = (
            String::from("k"),
            CType::BasicType {
                qualifier: TypeQualifier::empty(),
                basic_type: BasicType::Int,
            },
        );

        let tag = Some(String::from("inner"));
        let members = vec![k];
        let inner_struct_type = StructureType::new_complete_structure_type(tag, members);
        let inner_struct_type_idx = dummy_state.add_structure_type(inner_struct_type);
        let inner = CType::StructureTypeRef {
            qualifier: TypeQualifier::empty(),
            symtab_idx: inner_struct_type_idx,
        };
        let next = {
            let recursive_struct_type =
                StructureType::new_incomplete_structure_type(String::from("recursive"));
            let recursive_struct_type_idx = dummy_state.add_structure_type(recursive_struct_type);
            let ptr = CType::StructureTypeRef {
                qualifier: TypeQualifier::empty(),
                symtab_idx: recursive_struct_type_idx,
            };
            CType::PointerType {
                qualifier: TypeQualifier::empty(),
                pointee_type: Box::new(ptr),
            }
        };

        let tag = Some(String::from("recursive"));
        let members = vec![(String::from("inner"), inner), (String::from("next"), next)];
        StructureType::new_complete_structure_type(tag, members)
    };

    let outer_struct_type_idx = dummy_state.add_structure_type(outer_struct_type);
    let outer_struct_type = CType::StructureTypeRef {
        qualifier: TypeQualifier::empty(),
        symtab_idx: outer_struct_type_idx,
    };

    let (mut toks, mut state) = text_test_harness(&recursive_struct);
    let declaration_parsed = parse_declaration(&mut toks, &mut state).expect("parse failed");
    let declaration_parsed_expected = ASTNode::Declaration(vec![Declaration::new(
        Identifier::new(dummy_state.current_scope, String::from("recursive_struct")),
        outer_struct_type,
        StorageClass::None,
        false,
        FunctionSpecifier::None,
        None,
    )]);

    assert_eq!(declaration_parsed, declaration_parsed_expected);
    assert_types_eq(&state, &dummy_state);
}

#[test]
fn parse_declaration_without_declarators() {
    let complete_struct = r#"
    struct s {
        int s;
    };
    "#;

    let mut dummy_state = ParserState::new();
    let tag = Some(String::from("s"));
    let members = vec![(
        String::from("s"),
        CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::Int,
        },
    )];
    let struct_type = StructureType::new_complete_structure_type(tag, members);

    let struct_type_idx = dummy_state.add_structure_type(struct_type);
    let struct_type = CType::StructureTypeRef {
        qualifier: TypeQualifier::empty(),
        symtab_idx: struct_type_idx,
    };

    let (mut toks, mut state) = text_test_harness(&complete_struct);
    let declaration_parsed = parse_declaration(&mut toks, &mut state).expect("parse failed");
    let declaration_parsed_expected =
        ASTNode::EmptyDeclaration(struct_type, dummy_state.current_scope);

    assert_eq!(declaration_parsed, declaration_parsed_expected);
    assert_types_eq(&state, &dummy_state);
}

#[test]
fn parse_typedef_basic() {
    let typedef_basic = r#"
    typedef int *int_ptr;
    int_ptr iptr;
    "#;

    let dummy_state = ParserState::new();
    let int_ptr_type = {
        let int = CType::BasicType {
            qualifier: TypeQualifier::empty(),
            basic_type: BasicType::Int,
        };

        CType::PointerType {
            qualifier: TypeQualifier::empty(),
            pointee_type: Box::new(int),
        }
    };

    let int_ptr = {
        Declaration::new(
            Identifier::new(dummy_state.current_scope, String::from("int_ptr")),
            int_ptr_type.clone(),
            StorageClass::None,
            true,
            FunctionSpecifier::None,
            None,
        )
    };

    let iptr = {
        Declaration::new(
            Identifier::new(dummy_state.current_scope, String::from("iptr")),
            int_ptr_type,
            StorageClass::None,
            false,
            FunctionSpecifier::None,
            None,
        )
    };

    let (mut toks, mut state) = text_test_harness(&typedef_basic);
    let typedef_parsed = parse_declaration(&mut toks, &mut state).expect("parse failed");
    let declaration_parsed = parse_declaration(&mut toks, &mut state).expect("parse failed");

    assert_eq!(typedef_parsed, ASTNode::Declaration(vec![int_ptr]));
    assert_eq!(declaration_parsed, ASTNode::Declaration(vec![iptr]));
}

#[test]
fn parse_bad_switch() {
    let bad_switch = r#"
    switch (expr) {
        case 1:
        case 2:
        case 3:
    }
    "#;

    let _dummy_state = ParserState::new();
    let (mut toks, mut state) = text_test_harness(&bad_switch);
    let res = parse_statement(&mut toks, &mut state);

    assert!(res.is_err(), "Bad switch statement");
}

#[test]
//
fn function_typedef_constraint() {
    let _fn_typedef_constraint = r#"
    typedef int F(void);         // type F is function with no parameters returning int
    F f, g;                      // f and g both have type compatible with F
    F f { /* ... */ }            // WRONG: syntax/constraint error
    F g() { /* ... */ }          // WRONG: declares that g returns a function
    int f(void) { /* ... */ }    // RIGHT: f has type compatible with F
    int g() { /* ... */ }        // RIGHT: g has type compatible with F
    F *e(void) { /* ... */ }     // e returns a pointer to a function
    F *((e))(void) { /* ... */ } // same: parentheses irrelevant
    int (*fp)(void);             // fp points to a function that has type F
    F *Fp;                       // Fp points to a function that has type F
    "#;

    // todo!("write the test (maybe copilot can do it)")
}
