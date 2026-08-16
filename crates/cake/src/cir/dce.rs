//! Dead Code Elimination
//! 
//! A simple optimization which removes trivially dead code, through a basic worklist algorithm.

use crate::cir::{FunctionDefinition, InstRef, Use, Value};

fn eliminate_dead_code(func: &mut FunctionDefinition) {
    // 1. mark all obviously dead instructions, putting them onto a worklist
    //    and removing the link to their parent block
    let mut worklist: Vec<InstRef> = Vec::with_capacity(64);
    for block in &func.blocks {
        for &iref in block.inst_refs.borrow().iter() {
            let inst = func.insts[iref];
            if func.inst_uses[iref].len() == 0 && !inst.has_side_effects() {
                func.inst_block[iref] = None;
                worklist.push(iref);
            }
        }
    }

    // 2. pop items off the worklist, checking if their operands are also dead after removing the use from the popped inst
    while !worklist.is_empty() {
        let dead_inst_ref = worklist.pop().unwrap();
        let dead_inst = func.insts[dead_inst_ref];
        let operand_count = dead_inst.num_operands(&func.value_vecs);

        for operand_idx in 0..operand_count {
            let use_ = Use {
                user: dead_inst_ref,
                operand_idx: operand_idx as u32,
            };
            
            let def = dead_inst.operand(&func.value_vecs, operand_idx);
            let remaining_uses = func.remove_use(def, use_);

            if remaining_uses == 0 {
                match def {
                    Value::Inst(inst_ref)
                    | Value::TupleElement(inst_ref, _) => {
                        func.inst_block[inst_ref] = None;
                        worklist.push(inst_ref);
                    }
                    Value::BlockArgument(block_ref, _) => {
                        // TODO: remove block arguments if they are no longer used
                        // (might lead to more dead code)
                    },
                }
            }
        }
    }

    // 3. do a single sweep to fixup links from block -> inst, fully erasing the dead insts
    for block in &mut func.blocks {
        let irefs = block.inst_refs.get_mut();
        irefs.retain(|&iref| {
            func.inst_block[iref].is_some()
        });
    }
}