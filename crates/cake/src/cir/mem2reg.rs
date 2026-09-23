//! Mem2Reg
//! 
//! This pass performs escape analysis on the stack slots in use by a function and promotes them 
//! to SSA values if they meet the following criteria:
//! 1. It is used only as the address operand of load/store instructions, and the use-def relation between 
//!    StackAddr and Load/Store instructions is local within a single basic block
//! 2. All loads and stores through the stack slot have the same type (i.e. no type punning is occurring)
//! 3. It is a scalar type (this matches criteria 1, since aggregates would usually require pointer arithmetic
//!    to access different fields)
//! 
//! Slots that meet these criteria have a small liveness analysis performed to identify which basic blocks
//! the value must be live in.
//! 
//! Finally, iterated dominance frontier is used to calculate where to place phis (block params)
//! using the liveness information from before to filter out redundant phis (effectively building pruned SSA)

use std::ops::Index;

use cake_util::{IndexVec, index_vec};
use rustc_hash::FxHashSet;
use smallvec::{SmallVec, smallvec};

use crate::cir::{BlockRef, FunctionDefinition, Inst, InstRef, Module, OperandCoord, StackSlotRef, Type, Value, dom_info::{self, DominanceFrontiers, DominanceTree}};

pub(crate) fn mem2reg(func: &mut FunctionDefinition) {
    let dom_tree = dom_info::dom_tree(func);
    let dom_frontiers = dom_info::dom_frontiers(func, &dom_tree);

    for stack_slot in StackSlotRef::iter(&func.stack_slots) {
        mem2reg_slot(func, stack_slot, &dom_tree, &dom_frontiers);
    }
}

/// Attempts mem2reg promotion on a single stack slot. If a slot is dead, then it will be ignored
/// and cleaned up by a later DCE pass (hopefully)
fn mem2reg_slot(
    func: &mut FunctionDefinition, 
    ss_ref: StackSlotRef,
    dom_tree: &DominanceTree,
    dom_frontiers: &DominanceFrontiers,
) {
    let analysis = is_slot_promotable(func, ss_ref);
    let slot_ty;
    let slot_live_in;
    let slot_stores;
    match analysis {
        SlotAnalysis::Promotable { ty, live_in, stores } => {
            slot_ty = ty;
            slot_live_in = live_in;
            slot_stores = stores;
        },
        SlotAnalysis::Dead => return,
        SlotAnalysis::NotPromotable => return,
    }

    let mut needs_phi: FxHashSet<BlockRef> = FxHashSet::default();
    let mut idf_worklist = slot_stores.clone();
    while !idf_worklist.is_empty() {
        let top = idf_worklist.pop().unwrap();
        for df_element in dom_frontiers.frontier(top) {
            if !slot_live_in[df_element] {
                continue
            }

            let inserted = needs_phi.insert(df_element);
            if inserted && !slot_stores.contains(&df_element) {
                idf_worklist.push(df_element);
            }
        }
    }

    let mut value_stack: SmallVec<[Value; 16]> = SmallVec::new();

    // each entry contains the number of children, which child is currently being traversed,
    // and whether this node along the current path pushed something onto the value stack.
    // this allows us to simulate a recursive traversal without actually using recursion
    let mut fingers: SmallVec<[(u32, u32, bool); 16]> = SmallVec::new();
    for bref in dom_tree.preorder_traversal(func) {
        let mut did_push = false;

        if needs_phi.contains(&bref) {
            let block_arg_ref = func.blocks[bref].push_block_arg(slot_ty);
            value_stack.push(Value::BlockArgument(bref, block_arg_ref));
            did_push = true;
        }

        let mut iref_idx = 0;
        while iref_idx < func.blocks[bref].inst_refs.borrow().len() {
            let &iref = func.blocks[bref].inst_refs.borrow().index(iref_idx);

            let accesses_stack = accesses_stack(func, iref, ss_ref);
            match accesses_stack {
                StackAccess::Load => {
                    // TODO: think about UB semantics
                    let top = *value_stack.last().expect("uninitialized variable used");
                    func.replace_all_uses_with(Value::Inst(iref), top);
                    func.remove_inst(iref);
                },
                StackAccess::Store => {
                    let val = func.insts[iref].get_operand(
                        &func.value_vecs, 
                        OperandCoord::direct(1)
                    );

                    if did_push {
                        *value_stack.last_mut().unwrap() = val;
                    }
                    else {
                        value_stack.push(val);
                        did_push = true;
                    }

                    func.remove_inst(iref);
                },
                StackAccess::Neither => iref_idx += 1,
            }
        };

        let terminator_ref = func.blocks[bref].terminator_ref();
        let terminator = func.insts[terminator_ref];
        for edge in terminator.edges() {
            if needs_phi.contains(&edge.target) {
                let top = *value_stack.last().expect("uninitialized variable used");
                let arg_vec = edge.args;
                func.value_vecs[arg_vec].push(top);
            }
        }

        let num_children = dom_tree.num_children(bref);
        if num_children > 0 {
            fingers.push((num_children as u32, 0, did_push));
            continue
        }
        
        if did_push {
            value_stack.pop();
        }
        
        loop {
            let Some(top) = fingers.last_mut() else {
                break
            };

            top.1 += 1;
            if top.0 != top.1 {
                break
            }

            if top.2 {
                value_stack.pop();
            }
            fingers.pop();
        }
    }

}

enum SlotAnalysis {
    Promotable {
        ty: Type,
        live_in: IndexVec<BlockRef, bool>,
        stores: Vec<BlockRef>,
    },
    Dead,
    NotPromotable,
}

/// Performs the analysis to determine if a slot is promotable; also checks if the slot is trivially dead
/// (e.g. all StackAddr instructions referencing it are dead), as well as the live-in information
fn is_slot_promotable(func: &FunctionDefinition, ss_ref: StackSlotRef) -> SlotAnalysis {
    let mut ss_type: Option<Type> = None;
    let mut stored = false;
    let mut loaded = false;
    
    let mut live_in_blocks: IndexVec<BlockRef, bool> = index_vec![false; func.blocks.len()];
    let mut store_blocks: FxHashSet<BlockRef> = FxHashSet::default();
    let mut liveness_worklist: SmallVec<[BlockRef; 32]> = smallvec![];

    // note that the only users of stack slot are StackAddr instructions
    for &ss_use in &func.stack_slot_uses[ss_ref] {
        let ss_use_vec = &func.inst_uses[ss_use];
        for &use_ in ss_use_vec {
            let ss_user = func.insts[use_.user];
            let inferred_type = match ss_user {
                Inst::Load { .. } => {
                    loaded = true;
                    let load_block = func.inst_block[use_.user].unwrap();
                    liveness_worklist.push(load_block);
                    func.inst_types[use_.user][0]
                }
                Inst::Store { addr, val } if use_.operand_coord.idx == 0 => {
                    stored = true;
                    let store_block = func.inst_block[use_.user].unwrap();
                    store_blocks.insert(store_block);
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

    liveness_worklist.retain(|bb| {
        let bb_irefs = func.blocks[*bb].inst_refs.borrow();
        for &iref in bb_irefs.iter() {
            let stack_access = accesses_stack(func, iref, ss_ref);
            match stack_access {
                StackAccess::Load => return true,
                StackAccess::Store => return false,
                StackAccess::Neither => (),
            }
        }
        unreachable!("there should be at least one load for a basic block to be on the worklist")
    });

    while !liveness_worklist.is_empty() {
        let bb = liveness_worklist.pop().unwrap();
        if live_in_blocks[bb] {
            continue;
        }

        live_in_blocks[bb] = true;
        let block = &func.blocks[bb];
        for &pred in &block.preds {
            if store_blocks.contains(&pred.pred_ref) {
                continue;
            }

            liveness_worklist.push(pred.pred_ref);
        }
    }

    SlotAnalysis::Promotable { 
        ty: ss_type.unwrap(), 
        live_in: live_in_blocks,
        stores: store_blocks.into_iter().collect(),
    }
}

enum StackAccess {
    Load,
    Store,
    Neither
}

/// Checks if an instruction is a load or a store from a specified stack slot
fn accesses_stack(func: &FunctionDefinition, inst_ref: InstRef, ss_ref: StackSlotRef) -> StackAccess {
    let inst = func.insts[inst_ref];
    let is_load;
    let addr = match inst {
        Inst::Load { addr } => {
            is_load = true;
            addr
        },
        Inst::Store { addr, val } => {
            is_load = false;
            addr
        }
        _ => return StackAccess::Neither,
    };

    let addr_inst = match addr {
        Value::Inst(inst_ref) => func.insts[inst_ref],
        _ => return StackAccess::Neither,
    };

    match addr_inst {
        Inst::StackAddr { slot } => {
            if slot != ss_ref {
                return StackAccess::Neither
            }

            if is_load {
                StackAccess::Load
            }
            else {
                StackAccess::Store
            }
        },
        _ => StackAccess::Neither
    }
}

#[cfg(test)]
mod test {
    use crate::cir::{ast2cir, dce, mem2reg::mem2reg};

    #[test]
    fn test_conditional() {
        let mut module = ast2cir::test::conditional_module();

        println!("======== before mem2reg ========");
        print!("{module}");

        for func in module.functions.iter_mut() {
            let Some(defn) = func.definition.as_mut() else {
                continue;
            };

            mem2reg(defn);
        }

        println!("======== after mem2reg ========");
        print!("{module}");
    }
}

