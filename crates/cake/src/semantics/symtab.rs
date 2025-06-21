use std::collections::HashMap;

use thiserror::Error;

use crate::{
    parser::ast::Constant,
    semantics::resolved_ast::NodeRef,
    types::{
        CType, EnumType, EnumTypeIdx, FunctionType, FunctionTypeIdx, StructureType,
        StructureTypeIdx, UnionType, UnionTypeIdx,
    },
};

// "function prototype scope" not included,
// just ignore symbol table when processing a function prototype
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScopeType {
    BlockScope,
    FunctionScope,
    FileScope,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Scope {
    pub(crate) scope_type: ScopeType,
    pub(crate) parent_scope: Option<usize>,
    pub(crate) index: usize,
}

impl Scope {
    pub(crate) fn new_file_scope() -> Self {
        Self {
            scope_type: ScopeType::FileScope,
            parent_scope: None,
            index: 0,
        }
    }

    pub(crate) fn new(scope_type: ScopeType, parent_scope: usize, index: usize) -> Self {
        Self {
            scope_type,
            parent_scope: Some(parent_scope),
            index,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum StorageClass {
    Extern,
    Static,
    Auto,
    Register,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Linkage {
    External,
    Internal,
    None,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Symbol {
    Object {
        object_type: TypeIdx,
        linkage: Linkage,
    },
    // used only for enumeration constants
    Constant(Constant),
    Function {
        function_type: FunctionTypeIdx,

        // 2 types of linkage: internal and external
        // by default, functions have external linkage, i.e. the linker can see the function and use it to resolve function calls in other translation units
        // internal linkage means that the linker is not allowed to use a function definition to resolve calls in other translation units (i.e. it is local to the current translation unit)
        // TODO: maybe implement inline function linkage rules (actual inlining optimization can be done using any function we have definition of in this translation unit)
        internal_linkage: bool,
        defined: bool,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct TypeIdx(pub(crate) usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TaggedTypeIdx {
    EnumTypeIdx(EnumTypeIdx),
    StructureTypeIdx(StructureTypeIdx),
    UnionTypeIdx(UnionTypeIdx),
}

impl From<EnumTypeIdx> for TaggedTypeIdx {
    fn from(value: EnumTypeIdx) -> Self {
        Self::EnumTypeIdx(value)
    }
}

impl From<StructureTypeIdx> for TaggedTypeIdx {
    fn from(value: StructureTypeIdx) -> Self {
        Self::StructureTypeIdx(value)
    }
}

impl From<UnionTypeIdx> for TaggedTypeIdx {
    fn from(value: UnionTypeIdx) -> Self {
        Self::UnionTypeIdx(value)
    }
}

impl TaggedTypeIdx {
    pub(crate) fn same_type(a: Self, b: Self) -> bool {
        match (a, b) {
            (TaggedTypeIdx::EnumTypeIdx(_), TaggedTypeIdx::EnumTypeIdx(_)) => true,
            (TaggedTypeIdx::StructureTypeIdx(_), TaggedTypeIdx::StructureTypeIdx(_)) => true,
            (TaggedTypeIdx::UnionTypeIdx(_), TaggedTypeIdx::UnionTypeIdx(_)) => true,
            _ => false,
        }
    }
}

// monolithic symbol table contains all symbols in program
// in particular, every symbol has an associated identifier and type
// will likely need to add storage / linkage / layout information as well
#[derive(Debug, Default, PartialEq)]
pub(crate) struct SymbolTable {
    // these are built during parse; types may need to be merged during resolve
    scopes: Vec<Scope>,

    // these are built during resolve phase
    symbols: Vec<HashMap<String, Symbol>>,
    labels: Vec<HashMap<String, NodeRef>>,
    tags: Vec<HashMap<String, TaggedTypeIdx>>,

    types: Vec<CType>,

    enum_types: Vec<EnumType>,
    structure_types: Vec<StructureType>,
    union_types: Vec<UnionType>,

    function_types: Vec<FunctionType>,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub(crate) enum SymtabError {
    #[error("Symbol `{0}` already declared")]
    AlreadyDeclared(String),
    #[error("type provided is not a tag")]
    NotATag(String),
    #[error("label `{0}` already declared within function")]
    LabelAlreadyDeclared(String),
}

impl SymbolTable {
    pub(crate) fn new_with_scopes(scopes: Vec<Scope>) -> Self {
        let symbols = vec![HashMap::new(); scopes.len()];
        let labels = vec![HashMap::new(); scopes.len()];
        let tags = vec![HashMap::new(); scopes.len()];
        Self {
            scopes,

            symbols,
            labels,
            tags,

            ..Default::default()
        }
    }

    pub(crate) fn num_scopes(&self) -> usize {
        self.scopes.len()
    }

    pub(crate) fn get_scopes(&self) -> &[Scope] {
        &self.scopes
    }

    // look in current scope and all parent scopes
    pub(crate) fn lookup_tag_type_idx(&self, scope: Scope, tag: &str) -> Option<TaggedTypeIdx> {
        let mut current_scope = scope;
        loop {
            match self.direct_lookup_tag_type_idx(current_scope, tag) {
                Some(tag_type) => return Some(tag_type),
                None => match current_scope.parent_scope {
                    Some(parent) => current_scope = self.scopes[parent],
                    None => return None,
                },
            }
        }
    }

    // only look in current scope
    pub(crate) fn direct_lookup_tag_type_idx(
        &self,
        scope: Scope,
        tag: &str,
    ) -> Option<TaggedTypeIdx> {
        let direct_lookup = self.tags[scope.index].get(tag);
        direct_lookup.copied()
    }

    pub(crate) fn lookup_symbol(&self, scope: Scope, name: &str) -> Option<&Symbol> {
        let mut current_scope = scope;
        loop {
            match self.direct_lookup_symbol(current_scope, name) {
                Some(sym) => return Some(sym),
                None => match current_scope.parent_scope {
                    Some(parent) => current_scope = self.scopes[parent],
                    None => return None,
                },
            }
        }
    }

    pub(crate) fn direct_lookup_symbol(&self, scope: Scope, name: &str) -> Option<&Symbol> {
        self.symbols[scope.index].get(name)
    }

    pub(crate) fn direct_lookup_symbol_mut(
        &mut self,
        scope: Scope,
        name: &str,
    ) -> Option<&mut Symbol> {
        self.symbols[scope.index].get_mut(name)
    }

    pub(crate) fn lookup_label(&self, scope: Scope, name: &str) -> Option<NodeRef> {
        let mut scope = scope;
        loop {
            if scope.scope_type == ScopeType::FunctionScope {
                break;
            } else if let Some(parent) = scope.parent_scope {
                scope = self.scopes[parent];
            } else {
                // maybe not unrecoverable, but grammar should 100% prevent this from happening
                unreachable!("label statement declared with no function scope as ancestor (should be impossible)");
            }
        }

        let fn_scope = &self.labels[scope.index];
        fn_scope.get(name).copied()
    }

    pub(crate) fn all_symbols_at_scope(
        &self,
        scope: Scope,
    ) -> impl Iterator<Item = (&String, &Symbol)> {
        self.symbols[scope.index].iter()
    }

    pub(crate) fn scope_from_idx(&self, idx: usize) -> Scope {
        self.scopes[idx]
    }

    pub(crate) fn add_qualified_type(&mut self, ctype: CType) -> TypeIdx {
        let type_idx = TypeIdx(self.types.len());
        self.types.push(ctype);
        type_idx
    }

    pub(crate) fn add_enum_type(&mut self, enum_type: EnumType) -> EnumTypeIdx {
        EnumTypeIdx::from_push(&mut self.enum_types, enum_type)
    }

    pub(crate) fn add_structure_type(&mut self, structure_type: StructureType) -> StructureTypeIdx {
        StructureTypeIdx::from_push(&mut self.structure_types, structure_type)
    }

    pub(crate) fn add_union_type(&mut self, union_type: UnionType) -> UnionTypeIdx {
        UnionTypeIdx::from_push(&mut self.union_types, union_type)
    }

    pub(crate) fn add_function_type(&mut self, function_type: FunctionType) -> FunctionTypeIdx {
        FunctionTypeIdx::from_push(&mut self.function_types, function_type)
    }

    pub(crate) fn add_tag(
        &mut self,
        scope: Scope,
        name: String,
        tag: TaggedTypeIdx,
    ) -> Result<(), SymtabError> {
        if self.tags[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }

        self.tags[scope.index].insert(name, tag);

        Ok(())
    }

    pub(crate) fn add_symbol(
        &mut self,
        scope: Scope,
        name: String,
        symbol: Symbol,
    ) -> Result<(), SymtabError> {
        if self.symbols[scope.index].contains_key(&name) {
            return Err(SymtabError::AlreadyDeclared(name));
        }
        self.symbols[scope.index].insert(name, symbol);
        Ok(())
    }

    pub(crate) fn add_label(
        &mut self,
        scope: Scope,
        name: String,
        labeled_statement: NodeRef,
    ) -> Result<(), SymtabError> {
        // label names must be unique within a function
        // -> easiest is just to associate labels w/ function scopes.
        let mut scope = scope;
        loop {
            if scope.scope_type == ScopeType::FunctionScope {
                break;
            } else if let Some(parent) = scope.parent_scope {
                scope = self.scopes[parent];
            } else {
                // maybe not unrecoverable, but grammar should 100% prevent this from happening
                panic!("label statement declared with no function scope as ancestor (should be impossible)");
            }
        }

        let fn_scope = &mut self.labels[scope.index];
        if fn_scope.contains_key(&name) {
            return Err(SymtabError::LabelAlreadyDeclared(name));
        }

        fn_scope.insert(name, labeled_statement);
        Ok(())
    }

    pub(crate) fn get_qualified_type(&self, idx: TypeIdx) -> &CType {
        &self.types[idx.0 as usize]
    }

    pub(crate) fn get_enum_type(&self, idx: EnumTypeIdx) -> &EnumType {
        &self.enum_types[idx]
    }

    pub(crate) fn get_enum_type_mut(&mut self, idx: EnumTypeIdx) -> &mut EnumType {
        &mut self.enum_types[idx]
    }

    pub(crate) fn get_structure_type(&self, idx: StructureTypeIdx) -> &StructureType {
        &self.structure_types[idx]
    }

    pub(crate) fn get_structure_type_mut(&mut self, idx: StructureTypeIdx) -> &mut StructureType {
        &mut self.structure_types[idx]
    }

    pub(crate) fn get_union_type(&self, idx: UnionTypeIdx) -> &UnionType {
        &self.union_types[idx]
    }

    pub(crate) fn get_union_type_mut(&mut self, idx: UnionTypeIdx) -> &mut UnionType {
        &mut self.union_types[idx]
    }

    pub(crate) fn get_function_type(&self, idx: FunctionTypeIdx) -> &FunctionType {
        &self.function_types[idx]
    }

    pub(crate) fn get_function_type_mut(&mut self, idx: FunctionTypeIdx) -> &mut FunctionType {
        &mut self.function_types[idx]
    }

    pub(crate) fn new_scope(&mut self, parent: Option<Scope>, scope_type: ScopeType) -> Scope {
        if let None = parent {
            debug_assert!(scope_type == ScopeType::FileScope);
        }

        let new_scope = Scope {
            scope_type,
            parent_scope: parent.map(|s| s.index),
            index: self.scopes.len(),
        };

        self.scopes.push(new_scope);
        // a little bit inefficient to keep around many empty hashmaps, but hopefully it's ok
        self.symbols.push(Default::default());
        self.labels.push(Default::default());
        self.tags.push(Default::default());
        new_scope
    }

    pub(crate) fn get_parent_scope(&self, scope: Scope) -> Option<Scope> {
        scope.parent_scope.map(|idx| self.scopes[idx])
    }
}
