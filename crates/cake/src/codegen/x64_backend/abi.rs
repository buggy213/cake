//! ABI Handling for x86_64
//! 
//! This module has a few important jobs:
//! 1. Define the MachineEnv for register allocation.
//!    This specifies which registers are caller-saved, callee-saved (union should be all registers) for
//!    all register classes. It specifies preferred registers, non-preferred registers, and reserved registers 
//!    (union should) be all registers. 
//! 
//! 2. Before register allocation, we need to analyze all function calls and apply ABI parameter 
//!    passing sequence rules to figure out which parameters go through stack and which go through
//!    registers.
//!    
//!    The parameters which go onto the stack require stack manipulation instructions, which we use
//!    `mov`s for (push and pop don't work for SSE registers). We need to save off the maximal stack usage 
//!    for parameter passing over all function calls in the function.
//!    
//!    The parameters which will be passed through registers are given as constraints to the register
//!    allocator.
//! 
//! 3. After register allocation, inserts prologue and epilogue code.
//!    Once we know which registers are allocated, we can save the callee-save registers at the entry block,
//!    and restore them before every `ret` instruction. 
//!    
//!    We can also finalize the layout of the stack frame which contains
//!    1. variables with automatic lifetime
//!    2. stack spills requested by the register allocator
//!    3. callee-saved registers
//!    4. maximally-sized parameter passing area 
//!    
//!    We can choose the layout of 1, 2, and 3 to either be unoptimized (no attempt to reduce stack usage)
//!    or optimized (sort by descending alignment to reduce stack usage). This layout is passed off
//!    to codegen for final assembly emission

use cake_util::{IndexVec, index_vec};
use regalloc2::{PRegSet, PRegSetIter};
use rustc_hash::FxHashMap;
use smallvec::{SmallVec, smallvec};

use crate::{cir::{self, StackSlot, StackSlotRef}, mir::{MachineFunctionDefinition, MachineInstRef, PhysReg}, regalloc::{self}};

enum ParameterLocation {
    Register(PhysReg),
    Stack(usize)
}

struct CallABIInfo {
    call_sig: cir::SigRef,
    parameter_locations: Vec<ParameterLocation>,
}

struct PreRegAllocABIInfo {
    call_by_iref: FxHashMap<MachineInstRef, CallABIInfo>,
    maximal_parameter_area: Option<StackSlot>,
}

/// Perform analysis of function calls and insert parameter passing sequences
fn prepare_abi(mfunc: &mut MachineFunctionDefinition) -> PreRegAllocABIInfo {
    let mut call_by_iref = FxHashMap::default();
    
    for block in &mfunc.blocks {
        for &iref in &block.inst_refs {

        }
    }

    PreRegAllocABIInfo { 
        call_by_iref, 
        maximal_parameter_area: todo!(),
    }
}

struct StackLayout {
    // negative offsets from the top of the stack frame
    stack: IndexVec<StackSlotRef, u32>,

    // total stack frame size, guaranteed to be a multiple of 16
    stack_usage: u32,
}

/// Finalize the stack layout, then insert prologue and epilogue code
fn finalize_abi(
    mfunc: &mut MachineFunctionDefinition,
    used_int_regs: PRegSet,
    maximal_parameter_area: Option<StackSlot>,
    optimize_stack: bool,
) -> StackLayout {
    let abi = abi_machine_env();
    let int_save = used_int_regs & abi.int_callee_saved_regset;

    let mut int_save_slots: SmallVec<[StackSlotRef; 16]> = smallvec![];
    let mut iref_buffer: SmallVec<[MachineInstRef; 17]> = smallvec![];
    for _ in int_save {
        let slot_def = StackSlot {
            size: 8,
            align: 8,
        };

        let int_save_slot = mfunc.stack_slots.push(slot_def);
        int_save_slots.push(int_save_slot);
    }

    let mut computed_stack: IndexVec<StackSlotRef, u32> = index_vec![0u32; mfunc.stack_slots.len()];
    let mut alignment = 16u32;
    let mut current_stack = 8u32;
    
    if optimize_stack {
        let mut stack_slot_by_align: Vec<StackSlotRef> = StackSlotRef::iter(&mfunc.stack_slots).collect();
        stack_slot_by_align.sort_by(|&a, &b| {
            Ord::cmp(&mfunc.stack_slots[a].align, &mfunc.stack_slots[b].align)
        });

        
        for stack_slot_ref in stack_slot_by_align {
            let stack_slot = mfunc.stack_slots[stack_slot_ref];
            current_stack += stack_slot.size;
            current_stack = current_stack.next_multiple_of(stack_slot.align);
            computed_stack[stack_slot_ref] = current_stack;
            alignment = u32::max(alignment, stack_slot.align);
        }
    }
    else {
        let mut current_stack = 0u32;
        for (stack_slot_ref, &stack_slot) in StackSlotRef::enumerate2(&mfunc.stack_slots) {
            current_stack += stack_slot.size;
            current_stack = current_stack.next_multiple_of(stack_slot.align);
            computed_stack[stack_slot_ref] = current_stack;
            alignment = u32::max(alignment, stack_slot.align);
        }
    }
    
    let empty_parameter_area = StackSlot { size: 0, align: 1 };
    let maximal_parameter_area = maximal_parameter_area.unwrap_or(empty_parameter_area);    
    current_stack += maximal_parameter_area.size;
    current_stack = current_stack.next_multiple_of(maximal_parameter_area.align);
    alignment = u32::max(alignment, maximal_parameter_area.align);
    
    assert!(alignment <= 16, "overaligned function frames not supported for now");
    current_stack = current_stack.next_multiple_of(16);
    

    for &ss in &int_save_slots {
        
    }

    let entry_bref = mfunc.entry_block();
    let entry_block = &mut mfunc.blocks[entry_bref];
    entry_block.inst_refs.splice(0..0, iref_buffer.iter().copied());
    iref_buffer.clear();

    todo!()
}

struct ABIMachineEnv {
    int_preferred_regset: PRegSet,
    fp_preferred_regset: PRegSet,

    int_nonpreferred_regset: PRegSet,
    fp_nonpreferred_regset: PRegSet,

    int_callee_saved_regset: PRegSet,
    fp_callee_saved_regset: PRegSet,

    int_caller_saved_regset: PRegSet,
    fp_caller_saved_regset: PRegSet,

    int_reserved_regset: PRegSet,
    fp_reserved_regset: PRegSet,
}


const fn abi_machine_env() -> ABIMachineEnv {
    let int_callee_saved = todo!();
    let int_caller_saved = todo!();
    let fp_callee_saved = todo!();
    let fp_caller_saved = todo!();

    

    todo!()
}

const fn regalloc_machine_env() -> regalloc::MachineEnv {
    todo!()
}