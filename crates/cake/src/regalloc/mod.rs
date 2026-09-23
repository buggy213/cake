//! Register allocator interface. Uses a pull-based approach to query the IR through a set of traits,
//! and a push-based approach to apply edits to the IR at the end

use cake_util::{IndexVec, make_type_idx};

use crate::cir::Type;

pub(crate) struct VReg(u32);
pub(crate) struct PReg(u32);

pub(crate) struct ProgramPoint(u32);

pub(crate) enum Edit {

}

pub(crate) struct SpillSlot(Type);
make_type_idx!(SpillSlotRef, SpillSlot);


pub(crate) struct RegisterAllocatorOutput {
    spill_slots: IndexVec<SpillSlotRef, SpillSlot>,
    
}

pub(crate) trait RegisterAllocator {
    fn allocate(&mut self);
}

// Implementations of the register allocator
mod basic_alloc;