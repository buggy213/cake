use super::*;
use cake_lex::DFAScanner;

fn text_test_harness<'text>(text: &'text str) -> (CTokenStream<'text>, ParserState) {
    let c_table = CLexemes::load_table();
    let scanner = DFAScanner::new(c_table);
    let toks = CTokenStream::new(scanner, text.as_bytes());
    let state = ParserState::new();

    (toks, state)
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

    assert_eq!(
        parse_expr(&mut toks, &mut state),
        Ok(ExpressionNode::SimpleAssign(lhs, rhs))
    );
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

    assert_eq!(parse_expr(&mut toks, &mut state), Ok(expr));
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

    assert_eq!(parse_expr(&mut toks, &mut state), Ok(expr));
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
            let return_type = QualifiedType {
                base_type: CType::BasicType {
                    basic_type: BasicType::Int,
                },
                qualifier: TypeQualifier::empty(),
            };

            let argc_type = QualifiedType {
                base_type: CType::BasicType {
                    basic_type: BasicType::Int,
                },
                qualifier: TypeQualifier::empty(),
            };

            let argv_type = {
                let char_type = QualifiedType {
                    base_type: CType::BasicType {
                        basic_type: BasicType::Char,
                    },
                    qualifier: TypeQualifier::empty(),
                };

                let pointer_to_char_type = QualifiedType {
                    base_type: CType::PointerType {
                        pointee_type: Box::new(char_type),
                    },
                    qualifier: TypeQualifier::empty(),
                };

                QualifiedType {
                    base_type: CType::PointerType {
                        pointee_type: Box::new(pointer_to_char_type),
                    },
                    qualifier: TypeQualifier::empty(),
                }
            };

            let fn_type = CanonicalType::FunctionType {
                parameter_types: vec![
                    (Some(String::from("argc")), argc_type),
                    (Some(String::from("argv")), argv_type),
                ],
                return_type: Box::new(return_type),
                function_specifier: FunctionSpecifier::None,
                varargs: false,
                prototype_scope: dummy_state.current_scope,
            };

            let fn_type_idx = dummy_state.add_type(fn_type);

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
                    ASTNode::ReturnStatement(Some(Box::new(zero)))
                };
                dummy_state.close_scope().unwrap();
                ASTNode::CompoundStatement(vec![printf, return_stmt], dummy_state.current_scope)
            };

            dummy_state.close_scope().unwrap();

            let fn_type = QualifiedType {
                base_type: CType::FunctionTypeRef {
                    symtab_idx: fn_type_idx,
                },
                qualifier: TypeQualifier::empty(),
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
    assert_eq!(dummy_state.types, state.types);
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

    let abstract_type = QualifiedType {
        base_type: CType::BasicType {
            basic_type: BasicType::Int,
        },
        qualifier: TypeQualifier::empty(),
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

    let base_type = QualifiedType {
        base_type: CType::BasicType {
            basic_type: BasicType::Int,
        },
        qualifier: TypeQualifier::empty(),
    };

    let abstract_type = {
        QualifiedType {
            base_type: CType::PointerType {
                pointee_type: Box::new(base_type),
            },
            qualifier: TypeQualifier::empty(),
        }
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

    let base_type = QualifiedType {
        base_type: CType::BasicType {
            basic_type: BasicType::Int,
        },
        qualifier: TypeQualifier::empty(),
    };

    let mut dummy_state = ParserState::new();
    let abstract_type = {
        let fn_type_idx = {
            dummy_state.open_scope(ScopeType::FunctionScope);
            let fn_type = CanonicalType::FunctionType {
                parameter_types: vec![],
                return_type: Box::new(base_type),
                function_specifier: FunctionSpecifier::None,
                varargs: false,
                prototype_scope: dummy_state.current_scope,
            };
            dummy_state.close_scope().unwrap();
            dummy_state.add_type(fn_type)
        };
        QualifiedType {
            base_type: CType::FunctionTypeRef {
                symtab_idx: fn_type_idx,
            },
            qualifier: TypeQualifier::empty(),
        }
    };

    let (mut toks, mut state) = text_test_harness(&fn_abstract_type_str);
    let abstract_type_parsed = parse_type_name(&mut toks, &mut state).expect("parse failed");
    assert_eq!(abstract_type, abstract_type_parsed);
    assert_eq!(state.types, dummy_state.types);
}

#[test]
fn parse_abstract_type_test_3() {
    // "array of constant pointers to functions which take an unsigned int and varargs and return int"
    let abstract_type_str = r#"
    int (*const [])(unsigned int, ...)
    "#;

    let base_type = QualifiedType {
        base_type: CType::BasicType {
            basic_type: BasicType::Int,
        },
        qualifier: TypeQualifier::empty(),
    };
    let mut dummy_state = ParserState::new();
    let abstract_type = {
        let const_ptr = {
            let fn_type = {
                let uint_type = {
                    QualifiedType {
                        base_type: CType::BasicType {
                            basic_type: BasicType::UInt,
                        },
                        qualifier: TypeQualifier::empty(),
                    }
                };
                dummy_state.open_scope(ScopeType::FunctionScope);
                let anon_fn_type = CanonicalType::FunctionType {
                    parameter_types: vec![(None, uint_type)],
                    return_type: Box::new(base_type),
                    function_specifier: FunctionSpecifier::None,
                    varargs: true,
                    prototype_scope: dummy_state.current_scope,
                };
                dummy_state.close_scope().unwrap();
                let anon_fn_type_idx = dummy_state.add_type(anon_fn_type);

                QualifiedType {
                    base_type: CType::FunctionTypeRef {
                        symtab_idx: anon_fn_type_idx,
                    },
                    qualifier: TypeQualifier::empty(),
                }
            };

            QualifiedType {
                base_type: CType::PointerType {
                    pointee_type: Box::new(fn_type),
                },
                qualifier: TypeQualifier::Const,
            }
        };

        QualifiedType {
            base_type: CType::IncompleteArrayType {
                element_type: Box::new(const_ptr),
            },
            qualifier: TypeQualifier::empty(),
        }
    };

    let (mut toks, mut state) = text_test_harness(&abstract_type_str);
    let abstract_type_parsed = parse_type_name(&mut toks, &mut state).expect("parse failed");
    assert_eq!(abstract_type, abstract_type_parsed);
    assert_eq!(state.types, dummy_state.types);
}

#[test]
fn parse_struct_declaration_incomplete() {
    let incomplete_struct_str = r#"struct test s;"#;
    let mut dummy_state = ParserState::new();
    let struct_type = CanonicalType::IncompleteStructureType {
        tag: String::from("test"),
    };
    let struct_type_idx = dummy_state.add_type(struct_type);
    let struct_type = QualifiedType {
        base_type: CType::StructureTypeRef {
            symtab_idx: struct_type_idx,
        },
        qualifier: TypeQualifier::empty(),
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
    assert_eq!(state.types, dummy_state.types);
}

#[test]
fn parse_struct_declaration_anonymous() {
    let anonymous_struct = r#"
    struct {
        int k;
    } s;
    "#;

    let mut dummy_state = ParserState::new();
    let struct_type = CanonicalType::StructureType {
        tag: None,
        members: vec![(
            String::from("k"),
            QualifiedType {
                base_type: CType::BasicType {
                    basic_type: BasicType::Int,
                },
                qualifier: TypeQualifier::empty(),
            },
        )],
    };

    let struct_type_idx = dummy_state.add_type(struct_type);
    let struct_type = QualifiedType {
        base_type: CType::StructureTypeRef {
            symtab_idx: struct_type_idx,
        },
        qualifier: TypeQualifier::empty(),
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
    assert_eq!(state.types, dummy_state.types);
}

#[test]
fn parse_struct_declaration() {
    let complete_struct = r#"
    struct complete {
        int k;
    } t;
    "#;

    let mut dummy_state = ParserState::new();
    let struct_type = CanonicalType::StructureType {
        tag: Some(String::from("complete")),
        members: vec![(
            String::from("k"),
            QualifiedType {
                base_type: CType::BasicType {
                    basic_type: BasicType::Int,
                },
                qualifier: TypeQualifier::empty(),
            },
        )],
    };

    let struct_type_idx = dummy_state.add_type(struct_type);
    let struct_type = QualifiedType {
        base_type: CType::StructureTypeRef {
            symtab_idx: struct_type_idx,
        },
        qualifier: TypeQualifier::empty(),
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
    assert_eq!(state.types, dummy_state.types);
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
            QualifiedType {
                base_type: CType::BasicType {
                    basic_type: BasicType::Int,
                },
                qualifier: TypeQualifier::empty(),
            },
        );
        let inner_struct_type = CanonicalType::StructureType {
            tag: Some(String::from("inner")),
            members: vec![k],
        };
        let inner_struct_type_idx = dummy_state.add_type(inner_struct_type);
        let inner = QualifiedType {
            base_type: CType::StructureTypeRef {
                symtab_idx: inner_struct_type_idx,
            },
            qualifier: TypeQualifier::empty(),
        };
        let next = {
            let recursive_struct_type = CanonicalType::IncompleteStructureType {
                tag: String::from("recursive"),
            };
            let recursive_struct_type_idx = dummy_state.add_type(recursive_struct_type);
            let ptr = QualifiedType {
                base_type: CType::StructureTypeRef {
                    symtab_idx: recursive_struct_type_idx,
                },
                qualifier: TypeQualifier::empty(),
            };
            QualifiedType {
                base_type: CType::PointerType {
                    pointee_type: Box::new(ptr),
                },
                qualifier: TypeQualifier::empty(),
            }
        };

        CanonicalType::StructureType {
            tag: Some(String::from("recursive")),
            members: vec![(String::from("inner"), inner), (String::from("next"), next)],
        }
    };

    let outer_struct_type_idx = dummy_state.add_type(outer_struct_type);
    let outer_struct_type = QualifiedType {
        base_type: CType::StructureTypeRef {
            symtab_idx: outer_struct_type_idx,
        },
        qualifier: TypeQualifier::empty(),
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
    assert_eq!(state.types, dummy_state.types);
}

#[test]
fn parse_typedef_basic() {
    let typedef_basic = r#"
    typedef int *int_ptr;
    int_ptr iptr;
    "#;

    let dummy_state = ParserState::new();
    let int_ptr_type = {
        let int = QualifiedType {
            base_type: CType::BasicType {
                basic_type: BasicType::Int,
            },
            qualifier: TypeQualifier::empty(),
        };

        QualifiedType {
            base_type: CType::PointerType {
                pointee_type: Box::new(int),
            },
            qualifier: TypeQualifier::empty(),
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
