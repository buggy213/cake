use crate::{
    parser::ast::Identifier,
    semantics::{
        symtab::{Linkage, Scope, ScopeType, StorageClass, SymbolTable},
        types::{AggregateMember, FunctionTypeInner, QualifiedType},
    },
};
use crate::{
    parser::ast::{Constant, Declaration},
    semantics::{
        symtab::{CanonicalTypeIdx, Symbol},
        types::{CType, CanonicalType, TypeQualifier},
    },
};

use super::ASTResolveError;

pub(super) fn resolve_declaration(
    symtab: &mut SymbolTable,
    declaration: &Declaration,
    parser_types: &[CanonicalType],
) -> Result<(), ASTResolveError> {
    // TODO: think if there's a less expensive / jank way to do this
    // do we really need to preserve parser AST?
    let mut type_copy = declaration.qualified_type.clone();

    // first, resolve declaration type
    let declaration_type_category =
        resolve_declaration_type(symtab, &mut type_copy, declaration.name.scope, parser_types)?;

    // place resolved type into symbol table
    let resolved_type_idx = symtab.add_qualified_type(type_copy);

    // then, place identifier into symbol table
    match symtab.direct_lookup_symbol(declaration.name.scope, &declaration.name.name) {
        Some(_) => {
            // symbol is already declared
            return Err(ASTResolveError::SymbolRedeclaration);
        }
        None => {
            let symbol = match declaration_type_category {
                TypeCategory::Object => {
                    // resolve linkage rules here for declarations
                    // TODO: how to extend this to
                    let linkage = match declaration.storage_class {
                        StorageClass::Extern => Linkage::External,
                        StorageClass::Static => Linkage::Internal,
                        StorageClass::Auto | StorageClass::Register => {
                            todo!("only dealing with top-level declarations right now, neither of these should be allowed")
                        }
                        StorageClass::None => Linkage::External,
                    };

                    Symbol::Object {
                        object_type: resolved_type_idx,
                        linkage,
                    }
                }
                TypeCategory::Function => {
                    // within block scope, declaration of function can only have storage class extern

                    // declaration.name.scope.scope_type
                    todo!()
                }
                TypeCategory::Incomplete => {
                    // cannot declare an incomplete object
                    return Err(ASTResolveError::IncompleteDeclaration);
                }
            };
            symtab.add_symbol(
                declaration.name.scope,
                declaration.name.name.clone(),
                symbol,
            )?;
        }
    }

    Ok(())
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

/// ensure that type used in declaration is valid. intent is that caller
/// takes ownership of qualified_type from parser AST (right now it's a clone)
/// then places it into symbol table's types list; symbols will reference it through a TypeIdx
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
    // function types should be unqualified (their return value + parameters can be qualified)
    if !qualified_type.qualifier.is_empty() {
        if matches!(qualified_type.base_type, CType::FunctionTypeRef { .. }) {
            assert!(false, "warning: function types should not be qualified");
        }
    }

    // check base type
    match &mut qualified_type.base_type {
        // basic type is always ok
        CType::BasicType { .. } => return Ok(TypeCategory::Object),
        // void type is incomplete, i.e.
        // `void x;` is not a valid declaration
        CType::Void => return Ok(TypeCategory::Incomplete),

        CType::IncompleteArrayType { element_type } | CType::ArrayType { element_type, .. } => {
            let element_type_category =
                resolve_declaration_type(symtab, element_type, scope, parser_types)?;

            // element type must be an object (not function, not incomplete)
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
                CanonicalType::FunctionType(FunctionTypeInner {
                    parameter_types,
                    return_type,
                    prototype_scope,
                    varargs,
                    ..
                }) => {
                    // Note: gcc / clang seem to disallow defining a new type (struct / enum / union)
                    // inside the return type for C++ but not in C (I'm not totally sure why?)
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

                    // handle special case of f(void) by removing it
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
                        resolve_declaration_type(symtab, param_type, scope, parser_types)?;

                        // adjust types - array / function decay (array / function parameters converted to
                        // pointers to array / function types instead)
                        match &mut param_type.base_type {
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
                            }
                            _ => (),
                        };

                        // incomplete types allowed in function prototypes, but not function definitions
                        // we check this within resolve_function_definition
                    }
                }
                _ => {
                    return Err(ASTResolveError::CorruptedParserTypeTable);
                }
            }
            let fn_type_idx = symtab.add_canonical_type(function_type);
            // now a index into symtab's type table, rather than parser type table
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
                        let new_enum_type_idx = symtab.add_canonical_type(incomplete_standin);
                        symtab.add_tag(scope, tag.clone(), new_enum_type_idx)?;
                        new_enum_type_idx
                    }
                } else {
                    // anonymous enums always define a new type (6.7.2.3 5)
                    let new_enum_type_idx = symtab.add_canonical_type(incomplete_standin);
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
                        let new_struct_type_idx = symtab.add_canonical_type(incomplete_standin);
                        symtab.add_tag(scope, tag.clone(), new_struct_type_idx)?;
                        new_struct_type_idx
                    }
                } else {
                    // anonymous structs always define a new type
                    symtab.add_canonical_type(incomplete_standin)
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
                        let new_union_type_idx = symtab.add_canonical_type(incomplete_standin);
                        symtab.add_tag(scope, tag.clone(), new_union_type_idx)?;
                        new_union_type_idx
                    }
                } else {
                    // anonymous unions always define a new type
                    symtab.add_canonical_type(incomplete_standin)
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

// Tries to resolve incomplete types (which will be most types) from AST
// e.g. `struct node k` where `struct node` was defined earlier
// is considered an incomplete type before resolution
// If appropriate type to resolve to is not found, then creates a new one
fn resolve_incomplete_type(
    symtab: &mut SymbolTable,
    ast_idx: &mut CanonicalTypeIdx,
    tag: &str,
    scope: Scope,
    predicate: fn(&CanonicalType) -> bool,
    incomplete_ast_type: &CanonicalType,
) -> Result<TypeCategory, ASTResolveError> {
    // lexical lookup; caller provides predicate to check if it should be resolved
    // i.e. if we have
    // struct node { struct node *next; } n;
    // union node m;
    // even though they have the same tag, you definitely cannot resolve one to the other
    if let Some(type_idx) = symtab.lookup_tag_type_idx(scope, tag) {
        let symtab_type = symtab.get_type(type_idx);
        if predicate(symtab_type) {
            *ast_idx = type_idx;
            return Ok(symtab_type.type_category());
        }
    }

    // 6.7.2.3 3 technically forbids forward references to enums, but clang and gcc allow it (as will cake)
    // also, if the tag is already declared in our direct scope, add_tag will error.
    let new_type_idx = symtab.add_canonical_type(incomplete_ast_type.clone());
    symtab.add_tag(scope, tag.to_string(), new_type_idx)?;
    *ast_idx = new_type_idx;
    Ok(TypeCategory::Incomplete)
}

fn complete_type(
    symtab: &mut SymbolTable,
    type_idx: CanonicalTypeIdx,
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
            // structs / unions cannot contain function type / incomplete type
            // (they can contain pointer to function / incomplete; the latter is necessary for self-referential types to work)
            return Err(ASTResolveError::ObjectTypeRequired);
        }
    }

    Ok(())
}

pub(crate) fn resolve_function_definition(
    symtab: &mut SymbolTable,
    fn_declaration: &Declaration,
) -> Result<Identifier, ASTResolveError> {
    let scope = fn_declaration.name.scope;
    assert!(
        scope.scope_type == ScopeType::FileScope,
        "grammar only allows function definitions at file scope"
    );

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

    // TODO
    todo!("check that function parameters are not incomplete within definition");
    /*
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

                    // declared static -> declared extern or w/o storage class is ok (it will still only have internal linkage)
                    // but declared extern (or w/o storage class) -> declared static is not ok
                    if !*internal_linkage && declared_static {
                        return Err(ASTResolveError::StaticFunctionAfterExternFunction);
                    }

                    // check that the two are compatible

                    *defined = true;
                }
                _ => {
                    // collides with other, non-function identifier
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

    Ok(fn_declaration.name.clone())
     */
}
