use bitflags::bitflags;

use super::symtab::{CanonicalTypeIdx, Scope};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum BasicType {
    Char,
    UChar,

    Short,
    UShort,

    Int,
    UInt,

    Long,
    ULong,

    Float,
    Double,
}

// for now, all enums will be 4 bytes
pub(crate) type EnumVariant = (String, i32);

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub(crate) struct TypeQualifier: u8 {
        const Const = 1;
        const Restrict = 1 << 1;
        const Volatile = 1 << 2;
    }
}

// For now, not used
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum FunctionSpecifier {
    Inline,
    None,
}

pub(crate) type AggregateMember = (String, QualifiedType);
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct QualifiedType {
    pub(crate) base_type: CType,
    pub(crate) qualifier: TypeQualifier,
}

pub(crate) type FunctionArgument = (Option<String>, QualifiedType);

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct FunctionTypeInner {
    pub(crate) parameter_types: Vec<FunctionArgument>,
    pub(crate) return_type: Box<QualifiedType>,
    pub(crate) function_specifier: FunctionSpecifier,
    pub(crate) varargs: bool,
    pub(crate) prototype_scope: Scope,
}

impl FunctionTypeInner {
    // are two function types compatible?
    fn compatible(a: &Self, b: &Self) -> bool {
        // 1. return types are compatible
        todo!();

        // 2. parameter types are compatible
        todo!();

        // 3. whether they have varargs must be same
        if a.varargs != b.varargs {
            return false;
        }

        todo!()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum CanonicalType {
    // "Canonical" types go into type table, should try to only keep 1 around
    // if possible. in addition, they do not specify type qualifiers (though their members might)
    IncompleteUnionType {
        tag: String,
    },
    UnionType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
    },
    IncompleteStructureType {
        tag: String, // having an anonymous incomplete struct/union type seems meaningless...
    },
    StructureType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
    },
    IncompleteEnumType {
        tag: String,
    },
    EnumerationType {
        tag: Option<String>,
        members: Vec<EnumVariant>,
    },
    FunctionType(FunctionTypeInner),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CType {
    // Derived / basic types can be passed around more freely
    BasicType {
        basic_type: BasicType,
    },
    IncompleteArrayType {
        element_type: Box<QualifiedType>,
    },
    ArrayType {
        size: usize,
        element_type: Box<QualifiedType>,
    },

    PointerType {
        pointee_type: Box<QualifiedType>,
    },
    Void,

    // Pointers into symbol table to avoid having deep copies of highly-nested structs / unions
    // need to be careful to avoid circular / recursive structs (infinite size!)
    StructureTypeRef {
        symtab_idx: CanonicalTypeIdx,
    },
    UnionTypeRef {
        symtab_idx: CanonicalTypeIdx,
    },
    EnumTypeRef {
        symtab_idx: CanonicalTypeIdx,
    },
    FunctionTypeRef {
        symtab_idx: CanonicalTypeIdx,
    },
}

impl CType {
    pub(crate) fn scalar_type(&self) -> bool {
        match self {
            CType::BasicType { .. } => true,
            CType::PointerType { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn is_void(&self) -> bool {
        matches!(self, CType::Void)
    }
}
