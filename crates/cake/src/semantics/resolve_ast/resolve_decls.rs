use crate::semantics::symtab::{
    Linkage, Scope, ScopeType, StorageClass, SymbolTable, TaggedTypeIdx,
};
use crate::{
    parser::ast::{Constant, Declaration},
    semantics::symtab::Symbol,
};

use crate::types::{
    AggregateMember, EnumType, EnumTypeIdx, FunctionType, FunctionTypeIdx, QualifiedType,
    StructureType, StructureTypeIdx, UnionType, UnionTypeIdx,
};
use crate::types::{CType, TypeQualifier};

use super::{ASTResolveError, ParserTypes};
#[derive(Debug, PartialEq, Eq)]
enum TypeCategory {
    Object,
    Function,
    Incomplete,
}

trait HasTypeCategory {
    fn type_category(&self) -> TypeCategory;
}

impl HasTypeCategory for EnumType {
    fn type_category(&self) -> TypeCategory {
        if self.is_complete() {
            TypeCategory::Object
        } else {
            TypeCategory::Incomplete
        }
    }
}

impl HasTypeCategory for StructureType {
    fn type_category(&self) -> TypeCategory {
        if self.is_complete() {
            TypeCategory::Object
        } else {
            TypeCategory::Incomplete
        }
    }
}

impl HasTypeCategory for UnionType {
    fn type_category(&self) -> TypeCategory {
        if self.is_complete() {
            TypeCategory::Object
        } else {
            TypeCategory::Incomplete
        }
    }
}

impl HasTypeCategory for FunctionType {
    fn type_category(&self) -> TypeCategory {
        TypeCategory::Function
    }
}

impl CType {
    fn type_category(&self, symtab: &SymbolTable) -> TypeCategory {
        match self {
            CType::BasicType { .. } => TypeCategory::Object,
            CType::IncompleteArrayType { .. } => TypeCategory::Incomplete,
            CType::ArrayType { .. } => TypeCategory::Object,
            CType::PointerType { .. } => TypeCategory::Object,
            CType::Void => TypeCategory::Incomplete,
            CType::StructureTypeRef { symtab_idx } => {
                symtab.get_structure_type(*symtab_idx).type_category()
            }
            CType::UnionTypeRef { symtab_idx } => {
                symtab.get_union_type(*symtab_idx).type_category()
            }
            CType::EnumTypeRef { symtab_idx } => symtab.get_enum_type(*symtab_idx).type_category(),
            CType::FunctionTypeRef { symtab_idx } => {
                symtab.get_function_type(*symtab_idx).type_category()
            }
        }
    }
}

pub(super) fn resolve_declaration(
    symtab: &mut SymbolTable,
    declaration: &Declaration,
    parser_types: ParserTypes,
) -> Result<(), ASTResolveError> {
    // TODO: think if there's a less expensive / jank way to do this
    // do we really need to preserve parser AST?
    let mut type_copy = declaration.qualified_type.clone();

    // first, resolve declaration type
    let declaration_type_category =
        resolve_declaration_type(symtab, &mut type_copy, declaration.name.scope, parser_types)?;

    // functions are always resolved at file scope
    let scope = if declaration_type_category == TypeCategory::Function {
        Scope::new_file_scope()
    } else {
        declaration.name.scope
    };

    // then, place identifier into symbol table
    match symtab.direct_lookup_symbol(scope, &declaration.name.name) {
        Some(_) => {
            // Technically, this is allowed by standard
            // most code should not need this (barring forward declaration of function prototypes
            // which is handled by resolve_function_definition).
            // revisit it if it's an issue
            return Err(ASTResolveError::SymbolRedeclaration);
        }
        None => {
            let symbol = match declaration_type_category {
                TypeCategory::Object => {
                    // resolve linkage rules here for declarations
                    // i don't think this is fully standards compliant
                    let linkage = match declaration.storage_class {
                        StorageClass::Extern => Linkage::External,
                        StorageClass::Static => Linkage::Internal,
                        StorageClass::Auto | StorageClass::Register => {
                            todo!("support auto, register storage class");
                        }
                        StorageClass::None => {
                            if declaration.name.scope.scope_type == ScopeType::FileScope {
                                Linkage::External
                            } else {
                                Linkage::None
                            }
                        }
                    };

                    // place resolved type into symbol table
                    let resolved_type_idx = symtab.add_qualified_type(type_copy);

                    Symbol::Object {
                        object_type: resolved_type_idx,
                        linkage,
                    }
                }
                TypeCategory::Function => {
                    // this is definitely not standards compliant
                    let linkage = match declaration.storage_class {
                        StorageClass::Static => Linkage::Internal,
                        StorageClass::Extern | StorageClass::None => Linkage::External,
                        StorageClass::Auto | StorageClass::Register => {
                            return Err(ASTResolveError::BadFunctionStorageClass);
                        }
                    };

                    let fn_type_idx = match type_copy.base_type {
                        CType::FunctionTypeRef { symtab_idx } => symtab_idx,
                        _ => {
                            return Err(ASTResolveError::CorruptedSymtabTypeTable);
                        }
                    };

                    Symbol::Function {
                        function_type: fn_type_idx,
                        internal_linkage: linkage == Linkage::Internal,
                        defined: false,
                    }
                }
                TypeCategory::Incomplete => {
                    // cannot declare an incomplete object
                    return Err(ASTResolveError::IncompleteDeclaration);
                }
            };

            symtab.add_symbol(scope, declaration.name.name.clone(), symbol)?;
        }
    }

    Ok(())
}

// Resolves empty declaration. This is generally only used for declaring a new type
// (i.e. non-anonymous struct / union / enum)
pub(super) fn resolve_empty_declaration(
    symtab: &mut SymbolTable,
    declared_type: &CType,
    scope: Scope,
    parser_types: ParserTypes,
) -> Result<(), ASTResolveError> {
    let declared_type_copy = declared_type.clone();
    let mut qualified = QualifiedType {
        base_type: declared_type_copy,
        qualifier: TypeQualifier::empty(),
    };
    _ = resolve_declaration_type(symtab, &mut qualified, scope, parser_types)?;

    Ok(())
}

/// ensure that type used in declaration is valid. intent is that caller
/// takes ownership of qualified_type from parser AST (right now it's a clone)
/// then places it into symbol table's types list; symbols will reference it through a TypeIdx
fn resolve_declaration_type(
    symtab: &mut SymbolTable,
    qualified_type: &mut QualifiedType,
    scope: Scope,
    parser_types: ParserTypes,
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
            let function_type = adjust_function_type(symtab, *ast_idx, parser_types)?;
            let fn_type_idx = symtab.add_function_type(function_type);
            // now a index into symtab's type table, rather than parser type table
            *ast_idx = fn_type_idx;
            return Ok(TypeCategory::Function);
        }

        CType::EnumTypeRef {
            symtab_idx: ast_idx,
        } => {
            let enum_type = &parser_types.enum_types[*ast_idx];
            let tag = enum_type.tag();

            if !enum_type.is_complete() {
                // tag will never be None, not allowed by grammar
                let tag = tag.expect("empty tag for incomplete type disallowed by grammar");
                return resolve_incomplete_enum(symtab, ast_idx, parser_types, tag, scope);
            } else {
                let incomplete_standin = EnumType::new_incomplete_enum_type(String::new());
                let ast_enum_type_idx = if let Some(tag) = tag {
                    // named enum type, check symbol table
                    if let Some(type_idx) = symtab.direct_lookup_tag_type_idx(scope, tag) {
                        match type_idx {
                            TaggedTypeIdx::EnumTypeIdx(enum_type_idx) => enum_type_idx,
                            _ => return Err(ASTResolveError::TagRedefinition),
                        }
                    } else {
                        let new_enum_type_idx = symtab.add_enum_type(incomplete_standin);
                        symtab.add_tag(
                            scope,
                            tag.to_string(),
                            TaggedTypeIdx::EnumTypeIdx(new_enum_type_idx),
                        )?;
                        new_enum_type_idx
                    }
                } else {
                    // anonymous enums always define a new type (6.7.2.3 5)
                    let new_enum_type_idx = symtab.add_enum_type(incomplete_standin);
                    new_enum_type_idx
                };

                for enum_variant in enum_type.members() {
                    let symbol = Symbol::Constant(Constant::Int(enum_variant.1));
                    symtab.add_symbol(scope, enum_variant.0.clone(), symbol)?;
                }

                complete_enum(symtab, ast_enum_type_idx, enum_type.clone())?;

                *ast_idx = ast_enum_type_idx;
                return Ok(TypeCategory::Object);
            }
        }

        CType::StructureTypeRef {
            symtab_idx: ast_idx,
        } => {
            let structure_type = &parser_types.structure_types[*ast_idx];
            let tag = structure_type.tag();

            if !structure_type.is_complete() {
                // tag will never be None, not allowed by grammar
                let tag = tag.expect("empty tag for incomplete type disallowed by grammar");
                return resolve_incomplete_struct(symtab, ast_idx, parser_types, tag, scope);
            } else {
                let incomplete_standin =
                    StructureType::new_incomplete_structure_type(String::new());
                let ast_struct_type_idx = if let Some(tag) = tag {
                    // named enum type, check symbol table
                    if let Some(type_idx) = symtab.direct_lookup_tag_type_idx(scope, tag) {
                        match type_idx {
                            TaggedTypeIdx::StructureTypeIdx(struct_type_idx) => struct_type_idx,
                            _ => return Err(ASTResolveError::TagRedefinition),
                        }
                    } else {
                        let new_struct_type_idx = symtab.add_structure_type(incomplete_standin);
                        symtab.add_tag(
                            scope,
                            tag.to_string(),
                            TaggedTypeIdx::StructureTypeIdx(new_struct_type_idx),
                        )?;
                        new_struct_type_idx
                    }
                } else {
                    // anonymous enums always define a new type (6.7.2.3 5)
                    let new_struct_type_idx = symtab.add_structure_type(incomplete_standin);
                    new_struct_type_idx
                };

                let mut new_struct_members = Vec::from(structure_type.members());
                resolve_member_types(symtab, &mut new_struct_members, scope, parser_types)?;

                let completed_struct = StructureType::new_complete_structure_type(
                    tag.map(String::from),
                    new_struct_members,
                );
                complete_struct(symtab, ast_struct_type_idx, completed_struct);

                *ast_idx = ast_struct_type_idx;
                return Ok(TypeCategory::Object);
            }
        }

        CType::UnionTypeRef {
            symtab_idx: ast_idx,
        } => {
            let union_type = &parser_types.union_types[*ast_idx];
            let tag = union_type.tag();

            if !union_type.is_complete() {
                // tag will never be None, not allowed by grammar
                let tag = tag.expect("empty tag for incomplete type disallowed by grammar");
                return resolve_incomplete_union(symtab, ast_idx, parser_types, tag, scope);
            } else {
                let incomplete_standin = UnionType::new_incomplete_union_type(String::new());
                let ast_union_type_idx = if let Some(tag) = tag {
                    // named enum type, check symbol table
                    if let Some(type_idx) = symtab.direct_lookup_tag_type_idx(scope, tag) {
                        match type_idx {
                            TaggedTypeIdx::UnionTypeIdx(union_type_idx) => union_type_idx,
                            _ => return Err(ASTResolveError::TagRedefinition),
                        }
                    } else {
                        let new_union_type_idx = symtab.add_union_type(incomplete_standin);
                        symtab.add_tag(
                            scope,
                            tag.to_string(),
                            TaggedTypeIdx::UnionTypeIdx(new_union_type_idx),
                        )?;
                        new_union_type_idx
                    }
                } else {
                    // anonymous enums always define a new type (6.7.2.3 5)
                    let new_union_type_idx = symtab.add_union_type(incomplete_standin);
                    new_union_type_idx
                };

                let mut new_union_members = Vec::from(union_type.members());
                resolve_member_types(symtab, &mut new_union_members, scope, parser_types)?;

                let completed_union =
                    UnionType::new_complete_union_type(tag.map(String::from), new_union_members);
                complete_union(symtab, ast_union_type_idx, completed_union);

                *ast_idx = ast_union_type_idx;
                return Ok(TypeCategory::Object);
            }
        }
    }
}

// Tries to resolve incomplete types (which will be most types) from AST
// e.g. `struct node k` where `struct node` was defined earlier
// is considered an incomplete type before resolution
// If appropriate type to resolve to is not found, then creates a new one
fn resolve_incomplete_type(
    symtab: &mut SymbolTable,

    ast_idx: &mut TaggedTypeIdx,
    parser_types: ParserTypes,

    tag: &str,
    scope: Scope,
) -> Result<TypeCategory, ASTResolveError> {
    // lexical lookup; input must be compatible with looked up type in order to resolve
    // i.e. if we have
    // struct node { struct node *next; } n;
    // union node m;
    // even though they have the same tag, you definitely cannot resolve one to the other
    if let Some(type_idx) = symtab.lookup_tag_type_idx(scope, tag) {
        if TaggedTypeIdx::same_type(type_idx, *ast_idx) {
            *ast_idx = type_idx;
            // only called for enums, structs, unions
            return Ok(TypeCategory::Object);
        }
    }

    // 6.7.2.3 3 technically forbids forward references to enums, but clang and gcc allow it (as will cake)
    // also, if the tag is already declared in our direct scope, add_tag will error.
    let new_type_idx = match ast_idx {
        TaggedTypeIdx::EnumTypeIdx(enum_type_idx) => TaggedTypeIdx::EnumTypeIdx(
            symtab.add_enum_type(parser_types.enum_types[*enum_type_idx].clone()),
        ),
        TaggedTypeIdx::StructureTypeIdx(structure_type_idx) => TaggedTypeIdx::StructureTypeIdx(
            symtab.add_structure_type(parser_types.structure_types[*structure_type_idx].clone()),
        ),
        TaggedTypeIdx::UnionTypeIdx(union_type_idx) => TaggedTypeIdx::UnionTypeIdx(
            symtab.add_union_type(parser_types.union_types[*union_type_idx].clone()),
        ),
    };

    symtab.add_tag(scope, tag.to_string(), new_type_idx)?;
    *ast_idx = new_type_idx;
    Ok(TypeCategory::Incomplete)
}

fn resolve_incomplete_enum(
    symtab: &mut SymbolTable,
    ast_idx: &mut EnumTypeIdx,
    parser_types: ParserTypes,
    tag: &str,
    scope: Scope,
) -> Result<TypeCategory, ASTResolveError> {
    let mut dummy = TaggedTypeIdx::EnumTypeIdx(*ast_idx);
    let type_category = resolve_incomplete_type(symtab, &mut dummy, parser_types, tag, scope)?;
    match dummy {
        TaggedTypeIdx::EnumTypeIdx(enum_type_idx) => *ast_idx = enum_type_idx,
        _ => unreachable!("resolve_incomplete_type will not change type of index"),
    }

    Ok(type_category)
}

fn resolve_incomplete_struct(
    symtab: &mut SymbolTable,
    ast_idx: &mut StructureTypeIdx,
    parser_types: ParserTypes,
    tag: &str,
    scope: Scope,
) -> Result<TypeCategory, ASTResolveError> {
    let mut dummy = TaggedTypeIdx::StructureTypeIdx(*ast_idx);
    let type_category = resolve_incomplete_type(symtab, &mut dummy, parser_types, tag, scope)?;
    match dummy {
        TaggedTypeIdx::StructureTypeIdx(structure_type_idx) => *ast_idx = structure_type_idx,
        _ => unreachable!("resolve_incomplete_type will not change type of index"),
    }

    Ok(type_category)
}

fn resolve_incomplete_union(
    symtab: &mut SymbolTable,
    ast_idx: &mut UnionTypeIdx,
    parser_types: ParserTypes,
    tag: &str,
    scope: Scope,
) -> Result<TypeCategory, ASTResolveError> {
    let mut dummy = TaggedTypeIdx::UnionTypeIdx(*ast_idx);
    let type_category = resolve_incomplete_type(symtab, &mut dummy, parser_types, tag, scope)?;
    match dummy {
        TaggedTypeIdx::UnionTypeIdx(union_type_idx) => *ast_idx = union_type_idx,
        _ => unreachable!("resolve_incomplete_type will not change type of index"),
    }

    Ok(type_category)
}

fn complete_enum(
    symtab: &mut SymbolTable,
    type_idx: EnumTypeIdx,
    completed: EnumType,
) -> Result<(), ASTResolveError> {
    let symtab_entry = symtab.get_enum_type_mut(type_idx);
    if symtab_entry.is_complete() {
        return Err(ASTResolveError::TagRedefinition);
    }

    *symtab_entry = completed;
    Ok(())
}

fn complete_struct(
    symtab: &mut SymbolTable,
    type_idx: StructureTypeIdx,
    completed: StructureType,
) -> Result<(), ASTResolveError> {
    let symtab_entry = symtab.get_structure_type_mut(type_idx);
    if symtab_entry.is_complete() {
        return Err(ASTResolveError::TagRedefinition);
    }

    *symtab_entry = completed;
    Ok(())
}

fn complete_union(
    symtab: &mut SymbolTable,
    type_idx: UnionTypeIdx,
    completed: UnionType,
) -> Result<(), ASTResolveError> {
    let symtab_entry = symtab.get_union_type_mut(type_idx);
    if symtab_entry.is_complete() {
        return Err(ASTResolveError::TagRedefinition);
    }

    *symtab_entry = completed;
    Ok(())
}

fn adjust_function_type(
    symtab: &mut SymbolTable,
    parser_type_idx: FunctionTypeIdx,
    parser_types: ParserTypes,
) -> Result<FunctionType, ASTResolveError> {
    let mut function_type = parser_types.function_types[parser_type_idx].clone();
    let FunctionType {
        parameter_types,
        return_type,
        function_specifier,
        varargs,
        prototype_scope,
    } = &mut function_type;

    // Note: gcc / clang seem to disallow defining a new type (struct / enum / union)
    // inside the return type for C++ but not in C (I'm not totally sure why?)
    let return_type_category =
        resolve_declaration_type(symtab, return_type, *prototype_scope, parser_types)?;

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
    if parameter_types.len() == 1 && parameter_types[0] == void_only && !(*varargs) {
        parameter_types.clear();
    }

    for (_, param_type) in parameter_types {
        resolve_declaration_type(symtab, param_type, *prototype_scope, parser_types)?;

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
        // we check this within `resolve_function_definition`
    }

    Ok(function_type)
}

fn resolve_member_types(
    symtab: &mut SymbolTable,
    members: &mut Vec<AggregateMember>,
    scope: Scope,
    parser_types: ParserTypes,
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
    parser_types: ParserTypes,
) -> Result<FunctionTypeIdx, ASTResolveError> {
    let scope = fn_declaration.name.scope;
    assert!(
        scope.scope_type == ScopeType::FileScope,
        "grammar only allows function definitions at file scope"
    );

    // adjust function type
    let parser_type_idx = match fn_declaration.qualified_type.base_type {
        CType::FunctionTypeRef { symtab_idx } => symtab_idx,
        _ => {
            return Err(ASTResolveError::CorruptedParserTypeTable);
        }
    };

    let adjusted_type = adjust_function_type(symtab, parser_type_idx, parser_types)?;

    // parameter types must not be incomplete at definition time
    for (_, ty) in &adjusted_type.parameter_types {
        if ty.base_type.type_category(symtab) == TypeCategory::Incomplete {
            return Err(ASTResolveError::IncompleteParameter);
        }
    }

    // see resolve_declaration function path, same issue
    let definition_internal_linkage = match fn_declaration.storage_class {
        StorageClass::Extern | StorageClass::None => false,
        StorageClass::Static => true,
        StorageClass::Auto | StorageClass::Register => {
            return Err(ASTResolveError::BadFunctionStorageClass)
        }
    };

    match symtab.direct_lookup_symbol_mut(scope, &fn_declaration.name.name) {
        Some(sym) => {
            if let Symbol::Function {
                function_type,
                internal_linkage,
                defined,
            } = sym
            {
                // no redefinition
                if *defined {
                    return Err(ASTResolveError::FunctionRedefinition);
                }
                *defined = true;

                // ensure linkage is consistent
                if definition_internal_linkage != *internal_linkage {
                    return Err(ASTResolveError::InconsistentFunctionLinkage);
                }

                // check that definitions are compatible
                let idx = *function_type;
                let symtab_inner = symtab.get_function_type(idx);

                // whether they are varargs must be equal
                if symtab_inner.varargs != adjusted_type.varargs {
                    return Err(ASTResolveError::IncompatibleFunctionDeclarations);
                }

                // return types must be compatible
                if symtab_inner.return_type != adjusted_type.return_type {
                    return Err(ASTResolveError::IncompatibleFunctionDeclarations);
                }

                for (left, right) in std::iter::zip(
                    symtab_inner.parameter_types.iter(),
                    adjusted_type.parameter_types.iter(),
                ) {
                    if left.1 != right.1 {
                        return Err(ASTResolveError::IncompatibleFunctionDeclarations);
                    }
                }

                // replace canonical type in symtab
                let symtab_record = symtab.get_function_type_mut(idx);
                *symtab_record = adjusted_type;
                return Ok(idx);
            } else {
                return Err(ASTResolveError::SymbolRedeclaration);
            }
        }
        None => {
            // first declaration and definition
            let fn_type_idx = symtab.add_function_type(adjusted_type);
            let fn_symbol = Symbol::Function {
                function_type: fn_type_idx,
                internal_linkage: definition_internal_linkage,
                defined: true,
            };

            symtab.add_symbol(scope, fn_declaration.name.name.clone(), fn_symbol)?;
            return Ok(fn_type_idx);
        }
    }
}
