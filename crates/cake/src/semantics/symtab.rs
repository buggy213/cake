use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
    rc::Rc,
};

use thiserror::Error;

use crate::parser::ast::{ASTNode, Constant, NodeRef};

use super::types::{CType, CanonicalType, QualifiedType};

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
        function_type: CanonicalTypeIdx,

        // 2 types of linkage: internal and external
        // by default, functions have external linkage, i.e. the linker can see the function and use it to resolve function calls in other translation units
        // internal linkage means that the linker is not allowed to use a function definition to resolve calls in other translation units (i.e. it is local to the current translation unit)
        // TODO: maybe implement inline function linkage rules (actual inlining optimization can be done using any function we have definition of in this translation unit)
        internal_linkage: bool,
        defined: bool,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct CanonicalTypeIdx(pub(crate) usize);
impl Index<CanonicalTypeIdx> for Vec<CanonicalType> {
    type Output = CanonicalType;

    fn index(&self, index: CanonicalTypeIdx) -> &Self::Output {
        return &self[index.0];
    }
}
impl IndexMut<CanonicalTypeIdx> for Vec<CanonicalType> {
    fn index_mut(&mut self, index: CanonicalTypeIdx) -> &mut Self::Output {
        return &mut self[index.0];
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct TypeIdx(pub(crate) usize);

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
    tags: Vec<HashMap<String, CanonicalTypeIdx>>,

    types: Vec<QualifiedType>,
    canonical_types: Vec<CanonicalType>,
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

    // look in current scope and all parent scopes
    pub(crate) fn lookup_tag_type(&self, scope: Scope, tag: &str) -> Option<&CanonicalType> {
        self.lookup_tag_type_idx(scope, tag)
            .map(|idx| &self.canonical_types[idx])
    }

    pub(crate) fn lookup_tag_type_mut(
        &mut self,
        scope: Scope,
        tag: &str,
    ) -> Option<&mut CanonicalType> {
        self.lookup_tag_type_idx(scope, tag)
            .map(|idx| &mut self.canonical_types[idx])
    }

    pub(crate) fn lookup_tag_type_idx(&self, scope: Scope, tag: &str) -> Option<CanonicalTypeIdx> {
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
    pub(crate) fn direct_lookup_tag_type(&self, scope: Scope, tag: &str) -> Option<&CanonicalType> {
        self.direct_lookup_tag_type_idx(scope, tag)
            .map(|idx| &self.canonical_types[idx])
    }

    pub(crate) fn direct_lookup_tag_type_mut(
        &mut self,
        scope: Scope,
        tag: &str,
    ) -> Option<&mut CanonicalType> {
        self.direct_lookup_tag_type_idx(scope, tag)
            .map(|idx| &mut self.canonical_types[idx])
    }

    pub(crate) fn direct_lookup_tag_type_idx(
        &self,
        scope: Scope,
        tag: &str,
    ) -> Option<CanonicalTypeIdx> {
        let direct_lookup = self.tags[scope.index].get(tag);
        direct_lookup.copied()
    }

    pub(crate) fn lookup_symbol(&self, scope: Scope, name: &str) -> Option<&Symbol> {
        let mut current_scope = scope;
        loop {
            match self.direct_lookup_symbol(scope, name) {
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

    pub(crate) fn all_symbols_at_scope(&self, scope: Scope) -> impl Iterator<Item = &Symbol> {
        self.symbols[scope.index].values()
    }

    pub(crate) fn add_qualified_type(&mut self, qualified_type: QualifiedType) -> TypeIdx {
        let type_idx = TypeIdx(self.types.len());
        self.types.push(qualified_type);
        type_idx
    }

    pub(crate) fn add_canonical_type(&mut self, canonical_type: CanonicalType) -> CanonicalTypeIdx {
        let type_idx = CanonicalTypeIdx(self.canonical_types.len());
        self.canonical_types.push(canonical_type);
        type_idx
    }

    pub(crate) fn add_tag(
        &mut self,
        scope: Scope,
        name: String,
        tag: CanonicalTypeIdx,
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

    pub(crate) fn get_qualified_type(&self, idx: TypeIdx) -> &QualifiedType {
        &self.types[idx.0 as usize]
    }

    pub(crate) fn get_canonical_type(&self, idx: CanonicalTypeIdx) -> &CanonicalType {
        &self.canonical_types[idx]
    }

    pub(crate) fn get_canonical_type_mut(&mut self, idx: CanonicalTypeIdx) -> &mut CanonicalType {
        &mut self.canonical_types[idx]
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
