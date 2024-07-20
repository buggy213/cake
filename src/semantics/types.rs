use bitflags::bitflags;

use super::symtab::TypeIdx;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum BasicType {
    UChar,
    Char,
    UShort,
    Short,
    UInt,
    Int,
    ULong,
    Long,

    Float,
    Double,
}

pub(crate) type AggregateMember = (String, Box<CType>);
// for now, all enums will be 4 bytes
type EnumVariant = (String, i32);

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
    None
}


#[derive(Debug)]
pub(crate) struct QualifiedType {
    base_type: CType,
    qualifier: TypeQualifier
}

#[derive(Debug)]
pub(crate) enum CType {
    // "Canonical" types go into symbol table, should try to only keep 1 around
    // if possible. Incomplete types can be std::mem::replace'd once they are completed
    IncompleteUnionType {
        tag: String
    },
    UnionType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
    },
    IncompleteStructureType {
        tag: String // having an anonymous incomplete struct/union type seems meaningless...
    },
    StructureType {
        tag: Option<String>,
        members: Vec<AggregateMember>,
    },
    EnumerationType {
        tag: Option<String>,
        members: Vec<EnumVariant>,
    },

    // Derived / basic types can be passed around more freely
    BasicType {
        basic_type: BasicType,
    },
    ArrayType {
        size: usize,
        element_type: Box<CType>,
    },
    FunctionType {
        parameter_types: Vec<CType>,
        return_type: Box<CType>,
        function_specifier: FunctionSpecifier,
    },
    PointerType {
        pointee_type: Box<CType>,
    },
    Void,

    // Pointers into symbol table to avoid having deep copies of highly-nested structs / unions
    // need to be careful to avoid circular / recursive structs (infinite size!)
    StructureTypeRef {
        symtab_idx: TypeIdx
    },
    UnionTypeRef {
        symtab_idx: TypeIdx
    }
}
