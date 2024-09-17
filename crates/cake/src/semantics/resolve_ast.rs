use std::rc::Rc;

use thiserror::Error;

use crate::{
    parser::ast::{ASTNode, Constant, Declaration, ExpressionNode},
    semantics::{
        symtab::{ScopeType, StorageClass, Symbol, TypeIdx},
        types::{CType, CanonicalType, TypeQualifier},
    },
};

use super::{
    constexpr::integer_constant_eval,
    symtab::{Scope, SymbolTable, SymtabError},
    types::{AggregateMember, BasicType, QualifiedType},
};

#[derive(Debug, Error)]
enum ASTResolveError {
    #[error("function declaration must have storage class of extern or static")]
    BadFunctionStorageClass,
    #[error("symbol redeclaration error")]
    SymbolRedeclaration,
    #[error("function redefinition error")]
    FunctionRedefinition,
    #[error("function declared static after being declared extern")]
    StaticFunctionAfterExternFunction,
    #[error("symtab error")]
    SymtabError(#[from] SymtabError),
    #[error("declaration cannot have type void")]
    VoidDeclaration,
    #[error("redefined enum / union / struct tag")]
    TagRedefinition,
    #[error("parser type table corrupted")]
    CorruptedParserTypeTable,
    #[error("symtab type table corrupted")]
    CorruptedSymtabTypeTable,
    #[error("restrict qualifier only applies to pointers")]
    BadRestrictQualifier,
    #[error("object type is required")]
    ObjectTypeRequired,
    #[error("function return type is incomplete")]
    IncompleteReturnType,
    #[error("redeclared label name within same function")]
    RedeclaredLabel(#[source] SymtabError),
    #[error("error while evaluating compile-time integer constant")]
    IntegerConstantExprError,
    #[error("case labels only allowed within switch statements")]
    UnexpectedCaseLabel,
    #[error("only one default case within a switch statement")]
    MultipleDefaultLabels,
}

struct SwitchStatement {
    enclosing_switch: Option<usize>,
    case_values: Vec<Constant>,
    has_default: bool,
}

struct ResolverState {
    current_switch_statement: Option<usize>,
    switch_statements: Vec<SwitchStatement>,
}

impl ResolverState {
    fn add_switch(&mut self) {
        let new_switch = SwitchStatement {
            enclosing_switch: self.current_switch_statement,
            case_values: Default::default(),
            has_default: false,
        };
        let new_switch_idx = self.switch_statements.len();
        self.switch_statements.push(new_switch);
        self.current_switch_statement = Some(new_switch_idx);
    }

    fn current_switch(&self) -> Option<&SwitchStatement> {
        self.current_switch_statement
            .and_then(|idx| self.switch_statements.get(idx))
    }

    fn current_switch_mut(&mut self) -> Option<&mut SwitchStatement> {
        self.current_switch_statement
            .and_then(|idx| self.switch_statements.get_mut(idx))
    }

    fn close_switch(&mut self) {
        match self.current_switch() {
            Some(cur_switch) => {
                self.current_switch_statement = cur_switch.enclosing_switch;
            }
            None => {
                debug_assert!(false)
            }
        }
    }
}

/// 1. resolve declarations (place values into symbol table and enforce rules about redeclaration)
/// 2. resolve identifiers (i.e. check that there is a corresponding definition)
///    - todo: consider using numeric indices rather than string-based hashtable lookup for everything
/// 3. perform type checking for expressions
/// 4. evaluate compile time constants
/// goal: by the end of `resolve_ast`, the code is guaranteed to be free of compilation (though maybe not link-time) errors
pub fn resolve_ast(
    symtab: &mut SymbolTable,
    ast: &mut ASTNode,
    parser_types: &[CanonicalType],
    resolve_state: &mut ResolverState,
) -> Result<(), ASTResolveError> {
    match ast {
        ASTNode::TranslationUnit(definitions, _) => {
            for defn in definitions {
                resolve_ast(symtab, defn, parser_types, resolve_state)?;
            }

            Ok(())
        }
        ASTNode::FunctionDefinition(fn_declaration, body) => {
            resolve_function_definition(symtab, &fn_declaration, &body)
        }
        ASTNode::Declaration(declaration_list) => {
            for decl in declaration_list {
                resolve_declaration(symtab, decl, parser_types)?;
            }

            Ok(())
        }
        ASTNode::Label(labelee, ident) => {
            match symtab.add_label(ident.scope, ident.name.clone(), Rc::clone(labelee)) {
                Ok(_) => Ok(()),
                Err(e @ SymtabError::LabelAlreadyDeclared(_)) => {
                    Err(ASTResolveError::RedeclaredLabel(e))
                }
                Err(e) => Err(ASTResolveError::SymtabError(e)),
            }
        }
        ASTNode::CaseLabel(labelee, case_value) => {
            let value = resolve_integer_constant_expression(symtab, case_value)?;
            let replacement_node = Box::new(ExpressionNode::Constant(value));
            let _ = std::mem::replace(case_value, replacement_node);

            match resolve_state.current_switch_mut() {
                Some(SwitchStatement { case_values, .. }) => {
                    case_values.push(value);
                }
                None => return Err(ASTResolveError::UnexpectedCaseLabel),
            }

            Ok(())
        }
        ASTNode::DefaultLabel(_) => match resolve_state.current_switch_mut() {
            Some(SwitchStatement { has_default, .. }) => {
                if *has_default {
                    return Err(ASTResolveError::MultipleDefaultLabels);
                }
                Ok(())
            }
            None => Err(ASTResolveError::UnexpectedCaseLabel),
        },
        ASTNode::CompoundStatement(inner_statements, _) => {
            for stmt in inner_statements {
                resolve_ast(symtab, stmt, parser_types, resolve_state)?;
            }

            Ok(())
        }
        ASTNode::ExpressionStatement(_, _) => todo!(),
        ASTNode::NullStatement => Ok(()),
        ASTNode::IfStatement(_, _, _, _) => todo!(),
        ASTNode::SwitchStatement(controlling_expr, body, _) => {
            resolve_state.add_switch();
            todo!();
            resolve_state.close_switch();
            todo!()
        }
        ASTNode::WhileStatement(_, _, _) => todo!(),
        ASTNode::DoWhileStatement(_, _, _) => todo!(),
        ASTNode::ForStatement(_, _, _, _, _) => todo!(),
        ASTNode::GotoStatement(_) => todo!(),
        ASTNode::ContinueStatement => todo!(),
        ASTNode::BreakStatement => todo!(),
        ASTNode::ReturnStatement(_) => todo!(),
    }
}

fn resolve_function_definition(
    symtab: &mut SymbolTable,
    fn_declaration: &Declaration,
    body: &ASTNode,
) -> Result<(), ASTResolveError> {
    let scope = fn_declaration.name.scope;
    debug_assert!(scope.scope_type == ScopeType::FileScope);

    // check that storage class is extern, static, or none
    if fn_declaration.is_typedef {
        return Err(ASTResolveError::BadFunctionStorageClass);
    }
    let declared_static = match fn_declaration.storage_class {
        StorageClass::Extern | StorageClass::None => false, // no storage class is equivalent to extern
        StorageClass::Static => true,
        _ => {
            return Err(ASTResolveError::BadFunctionStorageClass);
        }
    };

    match symtab.direct_lookup_symbol_mut(scope, &fn_declaration.name.name) {
        Some(sym) => {
            match sym {
                Symbol::Function {
                    internal_linkage,
                    defined,
                } => {
                    // function was previously defined
                    if *defined {
                        return Err(ASTResolveError::FunctionRedefinition);
                    }

                    // declared static -> declared extern / w/o storage class is ok
                    // but declared extern -> declared static is not ok
                    if !*internal_linkage && declared_static {
                        return Err(ASTResolveError::StaticFunctionAfterExternFunction);
                    }

                    *defined = true;
                }
                _ => {
                    // collides with other identifier
                    return Err(ASTResolveError::SymbolRedeclaration);
                }
            }
        }
        None => {
            // first declaration and definition of function
            let fn_symbol = Symbol::Function {
                internal_linkage: declared_static,
                defined: true,
            };
            symtab.add_symbol(scope, fn_declaration.name.name.clone(), fn_symbol)?;
        }
    }

    todo!()
}

fn resolve_external_declaration() {}

// resolving declarations is tricky
// 1. file scope declarations
//
fn resolve_declaration(
    symtab: &mut SymbolTable,
    declaration: &mut Declaration,
    parser_types: &[CanonicalType],
) -> Result<(), ASTResolveError> {
    // first, resolve declaration type
    let declaration_type_category = resolve_declaration_type(
        symtab,
        &mut declaration.qualified_type,
        declaration.name.scope,
        parser_types,
    )?;

    match symtab.direct_lookup_symbol(declaration.name.scope, &declaration.name.name) {
        Some(_) => {
            // symbol is already declared
            return Err(ASTResolveError::SymbolRedeclaration);
        }
        None => {}
    }

    todo!()
}

enum TypeCategory {
    Object,
    Function,
    Incomplete,
}

impl CanonicalType {
    fn type_category(&self) -> TypeCategory {
        match self {
            CanonicalType::IncompleteEnumType { .. } => TypeCategory::Incomplete,
            CanonicalType::IncompleteUnionType { .. } => TypeCategory::Incomplete,
            CanonicalType::IncompleteStructureType { .. } => TypeCategory::Incomplete,
            CanonicalType::UnionType { .. } => TypeCategory::Object,
            CanonicalType::StructureType { .. } => TypeCategory::Object,
            CanonicalType::EnumerationType { .. } => TypeCategory::Object,
            CanonicalType::FunctionType { .. } => TypeCategory::Function,
        }
    }
}

/// ensure that type used in declaration is valid.
/// directly mutates declarations within the AST to index into symtab
fn resolve_declaration_type(
    symtab: &mut SymbolTable,
    qualified_type: &mut QualifiedType,
    scope: Scope,
    parser_types: &[CanonicalType],
) -> Result<TypeCategory, ASTResolveError> {
    // check type qualifiers
    // restrict can only be applied to pointers
    if qualified_type.qualifier.contains(TypeQualifier::Restrict) {
        if !matches!(qualified_type.base_type, CType::PointerType { .. }) {
            return Err(ASTResolveError::BadRestrictQualifier);
        }
    }

    // check base type
    match &mut qualified_type.base_type {
        // basic type is always ok
        CType::BasicType { .. } => return Ok(TypeCategory::Object),
        CType::Void => return Ok(TypeCategory::Incomplete),

        CType::IncompleteArrayType { element_type } | CType::ArrayType { element_type, .. } => {
            let element_type_category =
                resolve_declaration_type(symtab, element_type, scope, parser_types)?;
            if !matches!(element_type_category, TypeCategory::Object) {
                return Err(ASTResolveError::ObjectTypeRequired);
            }

            return Ok(TypeCategory::Incomplete);
        }
        CType::PointerType { pointee_type } => {
            resolve_declaration_type(symtab, pointee_type, scope, parser_types)?;
            return Ok(TypeCategory::Object);
        }
        CType::FunctionTypeRef {
            symtab_idx: ast_idx,
        } => {
            let mut function_type = parser_types[ast_idx.0].clone();
            match &mut function_type {
                CanonicalType::FunctionType {
                    parameter_types,
                    return_type,
                    prototype_scope,
                    varargs,
                    ..
                } => {
                    let return_type_category = resolve_declaration_type(
                        symtab,
                        return_type,
                        *prototype_scope,
                        parser_types,
                    )?;

                    // return type cannot be function type or array type
                    let return_type_is_array = matches!(
                        return_type.base_type,
                        CType::ArrayType { .. } | CType::IncompleteArrayType { .. }
                    );
                    let return_type_is_fn = matches!(return_type_category, TypeCategory::Function);
                    if return_type_is_fn || return_type_is_array {
                        return Err(ASTResolveError::IncompleteReturnType);
                    }

                    // handle special case of f(void)
                    let void_only = (
                        None,
                        QualifiedType {
                            base_type: CType::Void,
                            qualifier: TypeQualifier::empty(),
                        },
                    );
                    if parameter_types.len() == 1 && parameter_types[0] == void_only && !(*varargs)
                    {
                        parameter_types.clear();
                    }

                    for (_, param_type) in parameter_types {
                        let param_type_category =
                            resolve_declaration_type(symtab, param_type, scope, parser_types)?;

                        // adjust types - array / function decay
                        let param_type_category = match &mut param_type.base_type {
                            CType::IncompleteArrayType { element_type }
                            | CType::ArrayType {
                                size: _,
                                element_type,
                            } => {
                                let pointee_type = std::mem::replace(
                                    element_type,
                                    Box::new(QualifiedType {
                                        base_type: CType::Void,
                                        qualifier: TypeQualifier::empty(),
                                    }),
                                );
                                param_type.base_type = CType::PointerType { pointee_type };
                                TypeCategory::Object
                            }
                            CType::FunctionTypeRef { symtab_idx } => {
                                param_type.base_type = CType::PointerType {
                                    pointee_type: Box::new(QualifiedType {
                                        base_type: CType::FunctionTypeRef {
                                            symtab_idx: *symtab_idx,
                                        },
                                        qualifier: TypeQualifier::empty(),
                                    }),
                                };
                                TypeCategory::Object
                            }
                            _ => param_type_category,
                        };

                        // incomplete types allowed in function prototypes, but not definitions
                        let is_fn_definition: bool = todo!();
                        if is_fn_definition {
                            if matches!(param_type_category, TypeCategory::Incomplete) {
                                return Err(ASTResolveError::ObjectTypeRequired);
                            }
                        }
                    }
                }
                _ => {
                    return Err(ASTResolveError::CorruptedParserTypeTable);
                }
            }
            let fn_type_idx = symtab.add_type(function_type);
            *ast_idx = fn_type_idx;
            return Ok(TypeCategory::Function);
        }

        CType::EnumTypeRef {
            symtab_idx: ast_idx,
        } => match &parser_types[ast_idx.0] {
            incomplete_enum @ CanonicalType::IncompleteEnumType { tag } => {
                let symtab_type_is_enum = |t: &CanonicalType| {
                    matches!(
                        t,
                        CanonicalType::EnumerationType { .. }
                            | CanonicalType::IncompleteEnumType { .. }
                    )
                };
                let resolved_type_category = resolve_incomplete_type(
                    symtab,
                    ast_idx,
                    tag,
                    scope,
                    symtab_type_is_enum,
                    incomplete_enum,
                )?;
                return Ok(resolved_type_category);
            }
            complete_enum @ CanonicalType::EnumerationType { tag, members } => {
                let symtab_type_is_incomplete_enum =
                    |t: &CanonicalType| matches!(t, CanonicalType::IncompleteEnumType { .. });
                let incomplete_standin = CanonicalType::IncompleteEnumType { tag: String::new() };
                let ast_enum_type_idx = if let Some(tag) = tag {
                    // named enum type, check symbol table
                    if let Some(type_idx) = symtab.direct_lookup_tag_type_idx(scope, tag) {
                        type_idx
                    } else {
                        let new_enum_type_idx = symtab.add_type(incomplete_standin);
                        symtab.add_tag(scope, tag.clone(), new_enum_type_idx)?;
                        new_enum_type_idx
                    }
                } else {
                    // anonymous enums always define a new type (6.7.2.3 5)
                    let new_enum_type_idx = symtab.add_type(incomplete_standin);
                    new_enum_type_idx
                };

                for enum_variant in members {
                    let symbol = Symbol::Constant(Constant::Int(enum_variant.1));
                    symtab.add_symbol(scope, enum_variant.0.clone(), symbol)?;
                }

                complete_type(
                    symtab,
                    ast_enum_type_idx,
                    symtab_type_is_incomplete_enum,
                    complete_enum.clone(),
                )?;

                *ast_idx = ast_enum_type_idx;
                return Ok(TypeCategory::Object);
            }
            _ => {
                return Err(ASTResolveError::CorruptedParserTypeTable);
            }
        },
        CType::StructureTypeRef {
            symtab_idx: ast_idx,
        } => match &parser_types[ast_idx.0] {
            incomplete_struct @ CanonicalType::IncompleteStructureType { tag } => {
                let symtab_type_is_struct = |t: &CanonicalType| {
                    matches!(
                        t,
                        CanonicalType::IncompleteStructureType { .. }
                            | CanonicalType::StructureType { .. }
                    )
                };
                let resolved_type_category = resolve_incomplete_type(
                    symtab,
                    ast_idx,
                    tag,
                    scope,
                    symtab_type_is_struct,
                    incomplete_struct,
                )?;
                return Ok(resolved_type_category);
            }
            CanonicalType::StructureType { tag, members } => {
                let mut new_struct_members = members.clone();
                let incomplete_standin =
                    CanonicalType::IncompleteStructureType { tag: String::new() };
                let symtab_type_is_incomplete_struct =
                    |t: &CanonicalType| matches!(t, CanonicalType::IncompleteStructureType { .. });

                let ast_struct_type_idx = if let Some(tag) = tag {
                    if let Some(type_idx) = symtab.direct_lookup_tag_type_idx(scope, tag) {
                        type_idx
                    } else {
                        let new_struct_type_idx = symtab.add_type(incomplete_standin);
                        symtab.add_tag(scope, tag.clone(), new_struct_type_idx)?;
                        new_struct_type_idx
                    }
                } else {
                    // anonymous structs always define a new type
                    symtab.add_type(incomplete_standin)
                };

                resolve_member_types(symtab, &mut new_struct_members, scope, parser_types)?;
                let resolved_struct = CanonicalType::StructureType {
                    tag: tag.clone(),
                    members: new_struct_members,
                };

                complete_type(
                    symtab,
                    ast_struct_type_idx,
                    symtab_type_is_incomplete_struct,
                    resolved_struct,
                )?;

                *ast_idx = ast_struct_type_idx;
                return Ok(TypeCategory::Object);
            }
            _ => {
                return Err(ASTResolveError::CorruptedParserTypeTable);
            }
        },

        CType::UnionTypeRef {
            symtab_idx: ast_idx,
        } => match &parser_types[ast_idx.0] {
            incomplete_union @ CanonicalType::IncompleteUnionType { tag } => {
                let symtab_type_is_union = |t: &CanonicalType| {
                    matches!(
                        t,
                        CanonicalType::IncompleteUnionType { .. } | CanonicalType::UnionType { .. }
                    )
                };
                let resolved_type_category = resolve_incomplete_type(
                    symtab,
                    ast_idx,
                    tag,
                    scope,
                    symtab_type_is_union,
                    incomplete_union,
                )?;
                return Ok(resolved_type_category);
            }
            CanonicalType::UnionType { tag, members } => {
                let mut new_union_members = members.clone();
                let incomplete_standin = CanonicalType::IncompleteUnionType { tag: String::new() };
                let symtab_type_is_incomplete_union =
                    |t: &CanonicalType| matches!(t, CanonicalType::IncompleteUnionType { .. });

                let ast_union_type_idx = if let Some(tag) = tag {
                    if let Some(type_idx) = symtab.direct_lookup_tag_type_idx(scope, tag) {
                        type_idx
                    } else {
                        let new_union_type_idx = symtab.add_type(incomplete_standin);
                        symtab.add_tag(scope, tag.clone(), new_union_type_idx)?;
                        new_union_type_idx
                    }
                } else {
                    // anonymous unions always define a new type
                    symtab.add_type(incomplete_standin)
                };

                resolve_member_types(symtab, &mut new_union_members, scope, parser_types)?;
                let resolved_union = CanonicalType::StructureType {
                    tag: tag.clone(),
                    members: new_union_members,
                };

                complete_type(
                    symtab,
                    ast_union_type_idx,
                    symtab_type_is_incomplete_union,
                    resolved_union,
                )?;

                *ast_idx = ast_union_type_idx;
                return Ok(TypeCategory::Object);
            }
            _ => {
                return Err(ASTResolveError::CorruptedParserTypeTable);
            }
        },
    }
}

fn resolve_incomplete_type(
    symtab: &mut SymbolTable,
    ast_idx: &mut TypeIdx,
    tag: &String,
    scope: Scope,
    predicate: fn(&CanonicalType) -> bool,
    incomplete_ast_type: &CanonicalType,
) -> Result<TypeCategory, ASTResolveError> {
    if let Some(type_idx) = symtab.lookup_tag_type_idx(scope, tag) {
        let symtab_type = symtab.get_type(type_idx);
        if predicate(symtab_type) {
            *ast_idx = type_idx;
            return Ok(symtab_type.type_category());
        }
    }

    // 6.7.2.3 3 technically forbids forward references to enums, but clang and gcc allow it (as will we)
    let new_type_idx = symtab.add_type(incomplete_ast_type.clone());
    symtab.add_tag(scope, tag.clone(), new_type_idx)?;
    *ast_idx = new_type_idx;
    Ok(TypeCategory::Incomplete)
}

fn complete_type(
    symtab: &mut SymbolTable,
    type_idx: TypeIdx,
    predicate: fn(&CanonicalType) -> bool,
    completed: CanonicalType,
) -> Result<(), ASTResolveError> {
    match symtab.get_type_mut(type_idx) {
        CanonicalType::FunctionType { .. } => {
            return Err(ASTResolveError::CorruptedSymtabTypeTable);
        }
        incomplete_type @ CanonicalType::IncompleteEnumType { .. }
        | incomplete_type @ CanonicalType::IncompleteUnionType { .. }
        | incomplete_type @ CanonicalType::IncompleteStructureType { .. }
            if predicate(incomplete_type) =>
        {
            *incomplete_type = completed;
            return Ok(());
        }
        _ => {
            return Err(ASTResolveError::TagRedefinition);
        }
    }
}

fn resolve_member_types(
    symtab: &mut SymbolTable,
    members: &mut Vec<AggregateMember>,
    scope: Scope,
    parser_types: &[CanonicalType],
) -> Result<(), ASTResolveError> {
    for (_, member_type) in members {
        let member_type_category =
            resolve_declaration_type(symtab, member_type, scope, parser_types)?;
        if !matches!(member_type_category, TypeCategory::Object) {
            return Err(ASTResolveError::ObjectTypeRequired);
        }
    }

    Ok(())
}

fn resolve_integer_constant_expression(
    symtab: &SymbolTable,
    expr: &ExpressionNode,
) -> Result<Constant, ASTResolveError> {
    let constant = integer_constant_eval(symtab, expr);
    match constant {
        Ok(c) => match c {
            Constant::Int(_) => todo!(),
            Constant::LongInt(_) => todo!(),
            Constant::UInt(_) => todo!(),
            Constant::ULongInt(_) => todo!(),
            Constant::Float(_) => Err(ASTResolveError::IntegerConstantExprError),
            Constant::Double(_) => Err(ASTResolveError::IntegerConstantExprError),
        },
        Err(e) => Err(ASTResolveError::IntegerConstantExprError),
    }
}

fn resolve_expr() {}
