use cake_util::make_type_idx;

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

pub(crate) type AggregateMember = (String, CType);

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

pub(crate) type FunctionArgument = (Option<String>, CType);

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
    pub(crate) return_type: CType,
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
pub(crate) enum CType {
    // Derived / basic types can be passed around more freely
    BasicType {
        basic_type: BasicType,
        qualifier: TypeQualifier,
    },
    IncompleteArrayType {
        element_type: Box<CType>,
        qualifier: TypeQualifier,
    },
    ArrayType {
        size: u32,
        element_type: Box<CType>,
        qualifier: TypeQualifier,
    },

    PointerType {
        pointee_type: Box<CType>,
        qualifier: TypeQualifier,
    },
    Void {
        qualifier: TypeQualifier,
    },

    // Pointers into symbol table to avoid having deep copies of highly-nested structs / unions
    // need to be careful to avoid circular / recursive structs (infinite size!)
    StructureTypeRef {
        symtab_idx: StructureTypeIdx,
        qualifier: TypeQualifier,
    },
    UnionTypeRef {
        symtab_idx: UnionTypeIdx,
        qualifier: TypeQualifier,
    },
    EnumTypeRef {
        symtab_idx: EnumTypeIdx,
        qualifier: TypeQualifier,
    },
    FunctionTypeRef {
        symtab_idx: FunctionTypeIdx,
    },
}

impl CType {
    pub(crate) fn qualifier(&self) -> TypeQualifier {
        match self {
            CType::BasicType { qualifier, .. } => *qualifier,
            CType::IncompleteArrayType { qualifier, .. } => *qualifier,
            CType::ArrayType { qualifier, .. } => *qualifier,
            CType::PointerType { qualifier, .. } => *qualifier,
            CType::Void { qualifier } => *qualifier,
            CType::StructureTypeRef { qualifier, .. } => *qualifier,
            CType::UnionTypeRef { qualifier, .. } => *qualifier,
            CType::EnumTypeRef { qualifier, .. } => *qualifier,
            CType::FunctionTypeRef { symtab_idx } => TypeQualifier::empty(),
        }
    }

    pub(crate) fn qualifier_mut(&mut self) -> &mut TypeQualifier {
        match self {
            CType::BasicType { qualifier, .. } => qualifier,
            CType::IncompleteArrayType { qualifier, .. } => qualifier,
            CType::ArrayType { qualifier, .. } => qualifier,
            CType::PointerType { qualifier, .. } => qualifier,
            CType::Void { qualifier } => qualifier,
            CType::StructureTypeRef { qualifier, .. } => qualifier,
            CType::UnionTypeRef { qualifier, .. } => qualifier,
            CType::EnumTypeRef { qualifier, .. } => qualifier,
            CType::FunctionTypeRef { symtab_idx } => {
                panic!("function types are always unqualified")
            }
        }
    }

    pub(crate) fn unqualified_equal(lhs: &CType, rhs: &CType) -> bool {
        let mut lhs = lhs.clone();
        let mut rhs = rhs.clone();
        *lhs.qualifier_mut() = TypeQualifier::empty();
        *rhs.qualifier_mut() = TypeQualifier::empty();
        lhs == rhs
    }

    pub(crate) fn scalar_type(&self) -> bool {
        match self {
            CType::BasicType { .. } => true,
            CType::PointerType { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn is_void(&self) -> bool {
        matches!(self, CType::Void { .. })
    }

    pub(crate) fn is_function_pointer(&self) -> bool {
        match self {
            CType::PointerType { pointee_type, .. } => match pointee_type.as_ref() {
                CType::FunctionTypeRef { .. } => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub(crate) fn is_void_pointer(&self) -> bool {
        match self {
            CType::PointerType { pointee_type, .. } => match pointee_type.as_ref() {
                CType::Void { .. } => true,
                _ => false,
            },
            _ => false,
        }
    }
}
