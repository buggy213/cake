//! Mem2Reg
//! 
//! This pass performs escape analysis on the stack slots in use by a function and promotes them 
//! to SSA values if they meet the following criteria:
//! 1. It is used only as the address operand of load/store instructions
//! 2. All loads and stores through the stack slot have the same type (i.e. no type punning is occurring)
//! 3. It is a scalar type (this matches criteria 1, since aggregates would usually require pointer arithmetic
//!    to access different fields)
//! 
//! Slots that meet these criteria then have a small liveness analysis performed to identify which basic blocks
//! the value must be live in.
//! 
//! Finally, iterated dominance frontier is used to calculate where to place phis (block params)
//! using the liveness information from before to filter out useless phis (effectively building a pruned-SSA)
//! 

use crate::cir::{FunctionDefinition, Inst, Module, StackSlotRef, Type};

pub(crate) fn mem2reg(func: &mut FunctionDefinition) {
    for stack_slot in StackSlotRef::iter(&func.stack_slots) {
        mem2reg_slot(func, stack_slot);
    }
}

/// Attempts mem2reg promotion on a single stack slot. If a slot is dead, then it will be ignored
/// and cleaned up by a later DCE pass (hopefully)
fn mem2reg_slot(func: &mut FunctionDefinition, ss_ref: StackSlotRef) {
    let analysis = is_slot_promotable(func, ss_ref);
    let slot_ty;
    match analysis {
        SlotAnalysis::Promotable(ty) => {
            slot_ty = ty;
        },
        SlotAnalysis::Dead => return,
        SlotAnalysis::NotPromotable => return,
    }

    
}

enum SlotAnalysis {
    Promotable(Type),
    Dead,
    NotPromotable,
}

/// Performs the analysis to determine if a slot is promotable; also checks if the slot is trivially dead
/// (e.g. the StackAddr instructions using it are not used at all)
fn is_slot_promotable(func: &FunctionDefinition, ss_ref: StackSlotRef) -> SlotAnalysis {
    let mut ss_type: Option<Type> = None;
    let mut stored = false;
    let mut loaded = false;

    // note that the only users of stack slot are StackAddr instructions
    for &ss_use in &func.stack_slot_uses[ss_ref] {
        let ss_use_vec = &func.inst_uses[ss_use];
        for &use_ in ss_use_vec {
            let ss_user = func.insts[use_.user];
            let inferred_type = match ss_user {
                Inst::Load { .. } => {
                    loaded = true;
                    func.inst_types[use_.user][0]
                }
                Inst::Store { addr, val } if use_.operand_coord.idx == 0 => {
                    stored = true;
                    func.type_of_value(val)
                }
                _ => return SlotAnalysis::NotPromotable
            };
            
            match ss_type {
                Some(ty) if ty == inferred_type => (),
                Some(ty) if ty != inferred_type => return SlotAnalysis::NotPromotable,
                None => {
                    ss_type = Some(inferred_type);
                },
                _ => unreachable!()
            }
        }
    }

    if !stored && !loaded {
        return SlotAnalysis::Dead;
    }

    SlotAnalysis::Promotable(ss_type.unwrap())
}



