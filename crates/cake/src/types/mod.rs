use std::ops::{Index, IndexMut};

use bitflags::bitflags;

use crate::semantics::symtab::Scope;

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

impl BasicType {
    pub(crate) fn is_integral(self) -> bool {
        match self {
            BasicType::Char
            | BasicType::UChar
            | BasicType::Short
            | BasicType::UShort
            | BasicType::Int
            | BasicType::UInt
            | BasicType::Long
            | BasicType::ULong => true,
            BasicType::Float | BasicType::Double => false,
        }
    }

    pub(crate) fn is_signed(self) -> bool {
        match self {
            BasicType::Char
            | BasicType::Short
            | BasicType::Int
            | BasicType::Long
            | BasicType::Float
            | BasicType::Double => true,
            BasicType::UChar | BasicType::UShort | BasicType::UInt | BasicType::ULong => false,
        }
    }

    pub(crate) fn is_fp(self) -> bool {
        match self {
            BasicType::Float | BasicType::Double => true,
            _ => false,
        }
    }

    // we will only compile in LP64 model for now
    pub(crate) fn bytes(&self) -> u32 {
        match self {
            BasicType::Char => 1,
            BasicType::UChar => 1,
            BasicType::Short => 2,
            BasicType::UShort => 2,
            BasicType::Int => 4,
            BasicType::UInt => 4,
            BasicType::Long => 8,
            BasicType::ULong => 8,
            BasicType::Float => 4,
            BasicType::Double => 8,
        }
    }

    pub(crate) fn align(&self) -> u32 {
        self.bytes()
    }
}

macro_rules! make_type_idx {
    ($type_idx_name:tt, $type_name:tt) => {
        // TODO: consider newtyping Vec and adding push which returns type_idx
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub(crate) struct $type_idx_name(u32);

        impl $type_idx_name {
            pub(crate) fn from_push(vec: &mut Vec<$type_name>, val: $type_name) -> $type_idx_name {
                let idx = $type_idx_name(vec.len() as u32);
                vec.push(val);
                idx
            }
        }

        impl Index<$type_idx_name> for [$type_name] {
            type Output = $type_name;

            fn index(&self, index: $type_idx_name) -> &Self::Output {
                &self[index.0 as usize]
            }
        }

        impl IndexMut<$type_idx_name> for [$type_name] {
            fn index_mut(&mut self, index: $type_idx_name) -> &mut Self::Output {
                &mut self[index.0 as usize]
            }
        }

        impl Index<$type_idx_name> for Vec<$type_name> {
            type Output = $type_name;

            fn index(&self, index: $type_idx_name) -> &Self::Output {
                self.as_slice().index(index)
            }
        }

        impl IndexMut<$type_idx_name> for Vec<$type_name> {
            fn index_mut(&mut self, index: $type_idx_name) -> &mut Self::Output {
                self.as_mut_slice().index_mut(index)
            }
        }
    };
}

// for now, all enums will be 4 bytes
pub(crate) type EnumVariant = (String, i32);
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct EnumType {
    complete: bool,
    tag: Option<String>,
    members: Vec<EnumVariant>,
}

make_type_idx!(EnumTypeIdx, EnumType);

impl EnumType {
    pub(crate) fn new_complete_enum_type(
        tag: Option<String>,
        members: Vec<EnumVariant>,
    ) -> EnumType {
        EnumType {
            complete: true,
            tag,
            members,
        }
    }

    pub(crate) fn new_incomplete_enum_type(tag: String) -> EnumType {
        EnumType {
            complete: false,
            tag: Some(tag),
            members: Vec::new(),
        }
    }

    pub(crate) fn is_complete(&self) -> bool {
        self.complete
    }

    pub(crate) fn tag(&self) -> Option<&str> {
        (&self.tag).as_deref()
    }

    pub(crate) fn members(&self) -> &[EnumVariant] {
        &self.members
    }
}

pub(crate) type AggregateMember = (String, QualifiedType);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StructureType {
    complete: bool,
    tag: Option<String>,
    members: Vec<AggregateMember>,
}

make_type_idx!(StructureTypeIdx, StructureType);

impl StructureType {
    pub(crate) fn new_complete_structure_type(
        tag: Option<String>,
        members: Vec<AggregateMember>,
    ) -> StructureType {
        StructureType {
            complete: true,
            tag,
            members,
        }
    }

    pub(crate) fn new_incomplete_structure_type(tag: String) -> StructureType {
        StructureType {
            complete: false,
            tag: Some(tag),
            members: Vec::new(),
        }
    }

    pub(crate) fn is_complete(&self) -> bool {
        self.complete
    }

    pub(crate) fn tag(&self) -> Option<&str> {
        (&self.tag).as_deref()
    }

    pub(crate) fn members(&self) -> &[AggregateMember] {
        &self.members
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UnionType {
    complete: bool,
    tag: Option<String>,
    members: Vec<AggregateMember>,
}

make_type_idx!(UnionTypeIdx, UnionType);

impl UnionType {
    pub(crate) fn new_complete_union_type(
        tag: Option<String>,
        members: Vec<AggregateMember>,
    ) -> UnionType {
        UnionType {
            complete: true,
            tag,
            members,
        }
    }

    pub(crate) fn new_incomplete_union_type(tag: String) -> UnionType {
        UnionType {
            complete: false,
            tag: Some(tag),
            members: Vec::new(),
        }
    }

    pub(crate) fn is_complete(&self) -> bool {
        self.complete
    }

    pub(crate) fn tag(&self) -> Option<&str> {
        (&self.tag).as_deref()
    }

    pub(crate) fn members(&self) -> &[AggregateMember] {
        &self.members
    }
}

pub(crate) type FunctionArgument = (Option<String>, QualifiedType);

// For now, not actually used to inform codegen (this is perfectly compliant with standard)
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum FunctionSpecifier {
    Inline,
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct FunctionType {
    // public visibility of fields is reasonable, no real internal invariants to uphold
    pub(crate) parameter_types: Vec<FunctionArgument>,
    pub(crate) return_type: Box<QualifiedType>,
    pub(crate) function_specifier: FunctionSpecifier,
    pub(crate) varargs: bool,

    // TODO: remove this from FunctionType, it should essentially be a parsing detail
    pub(crate) prototype_scope: Scope,
}

make_type_idx!(FunctionTypeIdx, FunctionType);

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub(crate) struct TypeQualifier: u8 {
        const Const = 1;
        const Restrict = 1 << 1;
        const Volatile = 1 << 2;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct QualifiedType {
    pub(crate) base_type: CType,
    pub(crate) qualifier: TypeQualifier,
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
        symtab_idx: StructureTypeIdx,
    },
    UnionTypeRef {
        symtab_idx: UnionTypeIdx,
    },
    EnumTypeRef {
        symtab_idx: EnumTypeIdx,
    },
    FunctionTypeRef {
        symtab_idx: FunctionTypeIdx,
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
