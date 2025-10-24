use std::ops::Index;

use bumpalo::Bump;
use bumpalo::collections::Vec as BumpVec;

use crate::semantics::resolved_ast::MemberRef;
use crate::types::{BasicType, CType, StructureType, StructureTypeIdx, UnionType, UnionTypeIdx};

pub(crate) struct FieldOffsets<'arena>(&'arena [u32]);

impl Index<MemberRef> for FieldOffsets<'_> {
    type Output = u32;

    fn index(&self, index: MemberRef) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

pub(crate) struct StructLayout<'arena> {
    pub(crate) size: u32,
    pub(crate) align: u32,
    pub(crate) field_offsets: FieldOffsets<'arena>,
}

pub(crate) struct UnionLayout {
    pub(crate) size: u32,
    pub(crate) align: u32,
}

pub(crate) struct Layouts<'arena> {
    pub(crate) struct_layouts: BumpVec<'arena, StructLayout<'arena>>,
    pub(crate) union_layouts: BumpVec<'arena, UnionLayout>,
}

impl<'arena> Index<StructureTypeIdx> for Layouts<'arena> {
    type Output = StructLayout<'arena>;

    fn index(&self, index: StructureTypeIdx) -> &Self::Output {
        &self.struct_layouts[index.get_inner()]
    }
}

impl<'arena> Index<UnionTypeIdx> for Layouts<'arena> {
    type Output = UnionLayout;

    fn index(&self, index: UnionTypeIdx) -> &Self::Output {
        &self.union_layouts[index.get_inner()]
    }
}

impl Layouts<'_> {
    pub(crate) fn get_member_offset(&self, aggregate_type: &CType, member: MemberRef) -> u32 {
        match aggregate_type {
            CType::StructureTypeRef {
                symtab_idx,
                qualifier: _,
            } => self[*symtab_idx].field_offsets[member],
            CType::UnionTypeRef {
                symtab_idx,
                qualifier: _,
            } => 0,
            _ => unreachable!("TODO: more specificity in cake's type system to avoid this"),
        }
    }
}

// compute_layouts
// takes in all StructType and computes their layouts
pub(crate) fn compute_layouts<'arena>(
    arena: &'arena Bump,
    struct_types: &[StructureType],
    union_types: &[UnionType],
) -> Layouts<'arena> {
    let mut struct_layouts: BumpVec<'arena, StructLayout> =
        BumpVec::with_capacity_in(struct_types.len(), arena);
    let mut union_layouts: BumpVec<'arena, UnionLayout> =
        BumpVec::with_capacity_in(union_types.len(), arena);

    let mut struct_layouts_idx = 0;
    let mut union_layouts_idx = 0;
    loop {
        // struct / union types must be complete by the time they are declared
        // (this is enforced by the resolver) so any struct / union they have internally
        // must already be possible to layout
        // however, since they are separated, we have to "ping-pong" between laying out
        // structs and laying out unions in order to resolve "dependencies"
        'struct_loop: while struct_layouts_idx < struct_types.len() {
            let struct_type = &struct_types[struct_layouts_idx];

            let mut current_alignment = 0u32;
            let mut current_size = 0u32;
            let field_offsets: &'arena mut [u32] =
                arena.alloc_slice_fill_copy(struct_type.members().len(), 0);

            for (i, (_, member_type)) in struct_type.members().iter().enumerate() {
                if let Some((member_align, member_size)) =
                    type_align_size(member_type, &struct_layouts, &union_layouts)
                {
                    let aligned_offset = current_size.next_multiple_of(member_align);
                    field_offsets[i] = aligned_offset;

                    current_size = aligned_offset + member_size;
                    current_alignment = u32::max(current_alignment, member_align);
                } else {
                    // leaks some memory for field offsets, but it's ok
                    break 'struct_loop;
                }
            }

            // struct is fully laid out
            let struct_layout = StructLayout {
                size: current_size,
                align: current_alignment,
                field_offsets: FieldOffsets(field_offsets),
            };

            struct_layouts.push(struct_layout);
            struct_layouts_idx += 1;
        }

        'union_loop: while union_layouts_idx < union_types.len() {
            let union_type = &union_types[union_layouts_idx];

            let mut current_alignment = 0u32;
            let mut current_size = 0u32;

            for (i, (_, member_type)) in union_type.members().iter().enumerate() {
                if let Some((member_align, member_size)) =
                    type_align_size(member_type, &struct_layouts, &union_layouts)
                {
                    current_size = u32::max(current_size, member_size);
                    current_alignment = u32::max(current_alignment, member_align);
                } else {
                    break 'union_loop;
                }
            }

            // union is fully laid out
            let union_layout = UnionLayout {
                size: current_size,
                align: current_alignment,
            };

            union_layouts.push(union_layout);
            union_layouts_idx += 1;
        }

        if struct_layouts_idx == struct_types.len() && union_layouts_idx == union_types.len() {
            break;
        }
    }

    Layouts {
        struct_layouts,
        union_layouts,
    }
}

fn type_align_size(
    ty: &CType,
    struct_layouts: &[StructLayout],
    union_layouts: &[UnionLayout],
) -> Option<(u32, u32)> {
    match ty {
        CType::BasicType { basic_type, .. } => Some((basic_type.align(), basic_type.bytes())),

        CType::ArrayType {
            size, element_type, ..
        } => {
            if let Some((element_align, element_size)) =
                type_align_size(&element_type, struct_layouts, union_layouts)
            {
                Some((element_align, element_size * size))
            } else {
                None
            }
        }
        CType::PointerType { .. } => {
            let ptr_type = BasicType::ULong;
            Some((ptr_type.align(), ptr_type.bytes()))
        }

        CType::StructureTypeRef { symtab_idx, .. } => {
            if symtab_idx.get_inner() < struct_layouts.len() {
                let inner_struct_layout = &struct_layouts[*symtab_idx];
                Some((inner_struct_layout.align, inner_struct_layout.size))
            } else {
                None
            }
        }
        CType::UnionTypeRef { symtab_idx, .. } => {
            if symtab_idx.get_inner() < union_layouts.len() {
                let inner_union_layout = &union_layouts[*symtab_idx];
                Some((inner_union_layout.align, inner_union_layout.size))
            } else {
                None
            }
        }
        CType::EnumTypeRef { .. } => {
            // TODO: all enums are just ints for now
            let enum_type = BasicType::Int;
            Some((enum_type.align(), enum_type.bytes()))
        }
        CType::IncompleteArrayType { .. } => {
            panic!("incomplete array types are unsized")
        }
        CType::Void { .. } => {
            panic!("incomplete void type is unsized")
        }
        CType::FunctionTypeRef { .. } => panic!("function type is unsized"),
    }
}
