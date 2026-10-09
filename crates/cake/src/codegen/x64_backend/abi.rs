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

use std::iter::zip;

use cake_util::{IndexVec, index_vec};
use regalloc2::PRegSet;
use rustc_hash::FxHashMap;
use smallvec::{SmallVec, smallvec};

use crate::{cir::{self, StackSlot, StackSlotRef}, mir::{GprOperandWidth, MachineFunctionDefinition, MachineInst, MachineInstRef, MemLocation, MemOperand, MemOperandDisplacement, PhysReg, Reg, RegClass}, regalloc::{self}};

enum ParameterLocation {
    Register(PhysReg),
    Stack {
        // relative to the bottom of the parameter passing area
        offset: u32
    }
}

pub(crate) struct CallABIInfo {
    pub(crate) call_sig: cir::SigRef,
    pub(crate) parameter_locations: Vec<ParameterLocation>,
    pub(crate) parameter_area: StackSlot,
}

pub(crate) struct PreRegAllocABIInfo {
    pub(crate) callee_info: CallABIInfo,
    pub(crate) call_by_iref: FxHashMap<cir::SigRef, CallABIInfo>,
    pub(crate) maximal_parameter_area: Option<StackSlot>,
}

/// Perform analysis of function calls and insert parameter passing sequences
fn prepare_abi(mfunc: &mut MachineFunctionDefinition) -> PreRegAllocABIInfo {
    let mut call_by_iref = FxHashMap::default();
    
    fn process_sig(sig: &cir::Signature) -> CallABIInfo {
        let mut parameter_locations = vec![];

        use crate::mir::PhysReg::*;
        let mut int_regs: SmallVec<[PhysReg; 8]> = smallvec![r9, r8, rdx, rcx, rsi, rdi];
        let mut fp_regs: SmallVec<[PhysReg; 8]> = smallvec![xmm7, xmm6, xmm5, xmm4, xmm3, xmm2, xmm1, xmm0];

        let mut current_stack = 0u32;
        for &arg_ty in &sig.argument_types {
            if (arg_ty.is_integral() || arg_ty.is_ptr()) && let Some(ireg) = int_regs.pop() {
                parameter_locations.push(ParameterLocation::Register(ireg));
            }
            else if arg_ty.is_fp() && let Some(freg) = fp_regs.pop() {
                parameter_locations.push(ParameterLocation::Register(freg));
            }
            else {
                parameter_locations.push(ParameterLocation::Stack { offset: current_stack });
                current_stack += 8;
            }
        }
        
        CallABIInfo {
            call_sig: todo!(), 
            parameter_locations, 
            parameter_area: todo!() 
        }
    }

    for block in &mfunc.blocks {
        for &iref in &block.inst_refs {
            let inst = mfunc.insts[iref];
            let callee_sig: cir::Signature = match inst {
                MachineInst::CallWithParams { target, params } => {
                    todo!("we need to thread signatures in somehow")
                },
                MachineInst::CallIndirectWithParams { target, params } => todo!("indirect calls"),
                _ => continue
            };

            
        }
    }

    PreRegAllocABIInfo {
        callee_info: todo!(),
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
    

    for (preg, &ss) in zip(int_save, &int_save_slots) {
        let phys_reg: PhysReg = preg.into();
        let save_inst = MachineInst::StoreReg {
            op1: MemOperand::stack(ss),
            op2: Reg::PReg(phys_reg),
            width: GprOperandWidth::Qword,
        };

        let save_inst_ref = mfunc.insts.push(save_inst);
        iref_buffer.push(save_inst_ref);
    }

    let entry_bref = mfunc.entry_block();
    let entry_block = &mut mfunc.blocks[entry_bref];
    entry_block.inst_refs.splice(0..0, iref_buffer.iter().copied());
    iref_buffer.clear();

    for block in &mut mfunc.blocks {
        let terminator_ref = block.terminator_ref();
        let terminator = mfunc.insts[terminator_ref];

        if !matches!(terminator, MachineInst::Ret) {
            continue;
        }

        for (preg, &ss) in zip(int_save, &int_save_slots) {
            let phys_reg: PhysReg = preg.into();
            let restore_inst = MachineInst::Load { 
                dst: phys_reg.into(), 
                op2: MemOperand::stack(ss), 
                width: GprOperandWidth::Qword, 
            };

            let restore_inst_ref = mfunc.insts.push(restore_inst);
            iref_buffer.push(restore_inst_ref);
        }
        iref_buffer.push(terminator_ref);

        block.inst_refs.pop();
        block.inst_refs.extend_from_slice(&iref_buffer);
    }

    StackLayout { 
        stack: computed_stack, 
        stack_usage: current_stack 
    }
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