use cake_lex::DFAScanner;
use super::*;

fn text_test_harness<'text>(text: &'text str) -> (CTokenStream<'text>, ParserState) {
    let c_table = CLexemes::load_table();
    let scanner = DFAScanner::new(c_table);
    let toks = CTokenStream::new(scanner, text.as_bytes());
    let state = ParserState::new();

    (toks, state)
}

fn make_identifier(state: &mut ParserState, name: &str) -> ExpressionNode {
    let ident = Identifier::new(state.current_scope, name.to_string());
    ExpressionNode::Identifier(ident, None)
}

macro_rules! make_expr {
    ($expr_type:path, $($subexpr:expr),+) => {
        $expr_type(
            $(Box::new($subexpr)),+,
            None
        )
    };
}

#[test]
fn test_parse_expr_basic() {
    let basic_expr = r#"
    a = a + 1
    "#;
    let (mut toks, mut state) = text_test_harness(basic_expr);
    
    let lhs = Box::new(
        make_identifier(&mut state, "a")
    );

    let rhs = {
        let lhs = lhs.clone();
        let rhs = ExpressionNode::Constant(
            Constant::Int(1)
        );
        Box::new(
            ExpressionNode::Add(
                lhs, 
                Box::new(rhs), 
                None
            )
        )
    };

    assert_eq!(
        parse_expr(&mut toks, &mut state),
        Ok(ExpressionNode::SimpleAssign(
            lhs, 
            rhs, 
            None
        ))
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

    assert_eq!(
        parse_expr(&mut toks, &mut state),
        Ok(expr)
    );
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
                base_type: CType::BasicType { basic_type: BasicType::Int },
                qualifier: TypeQualifier::empty(),
            };

            let argc_type = QualifiedType {
                base_type: CType::BasicType { basic_type: BasicType::Int },
                qualifier: TypeQualifier::empty()
            };

            let argv_type = {
                let char_type = QualifiedType {
                    base_type: CType::BasicType { basic_type: BasicType::Char },
                    qualifier: TypeQualifier::empty(),
                };

                let pointer_to_char_type = QualifiedType {
                    base_type: CType::PointerType { pointee_type: Box::new(char_type) },
                    qualifier: TypeQualifier::empty(),
                };

                QualifiedType {
                    base_type: CType::PointerType { pointee_type: Box::new(pointer_to_char_type) },
                    qualifier: TypeQualifier::empty(),
                }
            };

            let fn_type = CanonicalType::FunctionType { 
                parameter_types: vec![(Some(String::from("argc")), argc_type), (Some(String::from("argv")), argv_type)], 
                return_type: Box::new(return_type), 
                function_specifier: FunctionSpecifier::None, 
                varargs: false, 
                prototype_scope: dummy_state.current_scope
            };

            let fn_type_idx = dummy_state.add_type(fn_type);

            let body = {
                dummy_state.open_scope(ScopeType::BlockScope);
                let printf = {
                    let printf_ident = make_identifier(&mut dummy_state, "printf");
                    let printf_arg = ExpressionNode::StringLiteral("Hello world!".to_string());
                    let printf_expr = ExpressionNode::FunctionCall(
                        Box::new(printf_ident), 
                        vec![printf_arg], 
                        None
                    );
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
                base_type: CType::FunctionTypeRef { symtab_idx: fn_type_idx },
                qualifier: TypeQualifier::empty(),
            };

            let fn_declaration = Declaration::new(
                Identifier::new(dummy_state.current_scope, String::from("main")), 
                fn_type, 
                StorageClass::None, 
                FunctionSpecifier::None, 
                None
            );
            
            ASTNode::FunctionDefinition(
                Box::new(fn_declaration),
                Box::new(body), 
                dummy_state.current_scope
            )

        };

        ASTNode::TranslationUnit(vec![main], dummy_state.current_scope)
    };

    let (mut toks, mut state) = text_test_harness(hello_world);
    let translation_unit_parsed = parse_translation_unit(&mut toks, &mut state).expect("parse failed");
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