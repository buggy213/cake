use std::ops::Index;

use crate::semantics::resolved_ast::MemberRef;
use crate::types::{StructureType, StructureTypeIdx, UnionType, UnionTypeIdx};

#[derive(Clone, Debug)]
pub(crate) struct StructLayout {
    pub(crate) size: u32,
    pub(crate) align: u32,
    pub(crate) field_offsets: Vec<u32>,
}

#[derive(Clone, Debug)]
pub(crate) struct UnionLayout {
    pub(crate) size: u32,
    pub(crate) align: u32,
}

#[derive(Debug)]
pub(crate) struct Layouts {
    pub(crate) struct_layouts: Vec<StructLayout>,
    pub(crate) union_layouts: Vec<UnionLayout>,

    struct_layout_valids: Vec<bool>,
    union_layout_valids: Vec<bool>
}

impl Index<StructureTypeIdx> for Layouts {
    type Output = StructLayout;

    fn index(&self, index: StructureTypeIdx) -> &Self::Output {
        assert!(self.struct_layout_valids[index.get_inner()]);
        &self.struct_layouts[index.get_inner()]
    }
}

impl Index<UnionTypeIdx> for Layouts {
    type Output = UnionLayout;

    fn index(&self, index: UnionTypeIdx) -> &Self::Output {
        assert!(self.union_layout_valids[index.get_inner()]);
        &self.union_layouts[index.get_inner()]
    }
}

enum LayoutError {

}

impl Layouts {
    pub(crate) fn new() -> Layouts {
        Layouts { 
            struct_layouts: Vec::new(), 
            union_layouts: Vec::new(), 
            struct_layout_valids: Vec::new(), 
            union_layout_valids: Vec::new() 
        }
    }

    pub(crate) fn get_struct_member_offset(&self, struct_ref: StructureTypeIdx, member: MemberRef) -> u32 {
        self[struct_ref].field_offsets[member.0 as usize]
    }

    pub(crate) fn compute_struct_layout(&mut self, struct_ref: StructureTypeIdx, struct_type: &StructureType) {
        let mut current_size = 0u32;
        let mut current_alignment = 1u32;
        let mut field_offsets = Vec::with_capacity(struct_type.members.len());
        for (_, member_type) in struct_type.members() {
            let member_size = member_type.size(self);
            let member_align = member_type.align(self);

            let aligned_offset = current_size.next_multiple_of(member_align);
            field_offsets.push(aligned_offset);

            current_size = aligned_offset + member_size;
            current_alignment = u32::max(current_alignment, member_align);
        }

        // insert padding at end of struct
        current_size = current_size.next_multiple_of(current_alignment);
        
        if struct_ref.get_inner() >= self.struct_layouts.len() {
            self.struct_layouts.resize(
                struct_ref.get_inner() + 1, 
                StructLayout { size: 0, align: 0, field_offsets: Vec::new() }
            );

            self.struct_layout_valids.resize(struct_ref.get_inner() + 1, false);
        }

        self.struct_layouts[struct_ref.get_inner()] = StructLayout { size: current_size, align: current_alignment, field_offsets };
        self.struct_layout_valids[struct_ref.get_inner()] = true;
    }

    pub(crate) fn compute_union_layout(&mut self, union_ref: UnionTypeIdx, union_type: &UnionType) {
        let mut current_size = 0u32;
        let mut current_alignment = 1u32;
        for (_, member_type) in union_type.members() {
            let member_size = member_type.size(self);
            let member_align = member_type.align(self);

            current_size = u32::max(current_size, member_size);
            current_alignment = u32::max(current_alignment, member_align);
        }

        current_size = current_size.next_multiple_of(current_alignment);

        if union_ref.get_inner() >= self.union_layouts.len() {
            self.union_layouts.resize(
                union_ref.get_inner() + 1, 
                UnionLayout { size: 0, align: 0 }
            );
            self.union_layout_valids.resize(union_ref.get_inner() + 1, false);
        }

        self.union_layouts[union_ref.get_inner()] = UnionLayout { size: current_size, align: current_alignment };
        self.union_layout_valids[union_ref.get_inner()] = true;
    }
}
