//! Dead Code Elimination
//! 
//! A simple optimization which removes trivially dead code, through a basic worklist algorithm.
//! This pass preserves CFG (i.e. it doesn't remove blocks with no side effects), and it won't catch 
//! referential cycles in the IR (e.g. a counter incrementing in a loop)

use crate::cir::{BlockRef, FunctionDefinition, InstRef, OperandCoord, Use, Value};

fn eliminate_dead_code(func: &mut FunctionDefinition) {
    // 1. mark all obviously dead instructions, putting them onto a worklist
    //    do the same for block arguments as well
    let mut inst_worklist: Vec<InstRef> = Vec::with_capacity(64);
    let mut block_arg_worklist: Vec<(BlockRef, u32)> = Vec::with_capacity(64);
    for (block_ref, block) in BlockRef::enumerate2(&func.blocks) {
        for &iref in block.inst_refs.borrow().iter() {
            let inst = func.insts[iref];
            if func.inst_uses[iref].len() == 0 && !inst.has_side_effects() {
                inst_worklist.push(iref);
            }
        }

        for (block_arg_idx, block_arg_use_list) in block.block_arg_uses.iter().enumerate() {
            if block_arg_use_list.len() == 0 {
                block_arg_worklist.push((block_ref, block_arg_idx as u32));
            }
        }
    }

    // 2. pop items off the worklists and remove them from Use list of operands
    // - for block args, they are not physically removed from the block here. instead, we just 
    //   sever the def-use edge the param in each predecessor's terminator, which lets the incoming 
    //   value cascade through the worklist if it becomes dead too. the block argument slot itself
    //   is left in place, still dead (empty use list) and is cleaned up at the end
    //   NOTE: block args of the entry block are immune from dead-code elimination, since they are
    //         in one-to-one correspondence with the function signature / ABI parameters. the cleanup
    //         at the end will leave them intact
    // - for insts, unlink them from their parent block, then check if their operands are also dead
    //   and add to worklist(s) if so
    // - iterate to fixed point
    loop {
        if inst_worklist.is_empty() && block_arg_worklist.is_empty() {
            break;
        }

        while !inst_worklist.is_empty() {
            let dead_inst_ref = inst_worklist.pop().unwrap();
            let dead_inst = func.insts[dead_inst_ref];

            func.inst_block[dead_inst_ref] = None;

            for operand_coord in dead_inst.operand_coord_iter(&func.value_vecs) {
                let use_ = Use {
                    user: dead_inst_ref,
                    operand_coord,
                };

                let def = dead_inst.get_operand(&func.value_vecs, operand_coord);
                let remaining_uses = func.remove_use(def, use_);

                if remaining_uses == 0 {
                    match def {
                        Value::Inst(inst_ref)
                        | Value::TupleElement(inst_ref, _) => {
                            inst_worklist.push(inst_ref);
                        }
                        Value::BlockArgument(block_ref, idx) => {
                            block_arg_worklist.push((block_ref, idx));
                        },
                    }
                }
            }
        }

        while !block_arg_worklist.is_empty() {
            let (dead_block_arg_block, dead_block_arg_idx) = block_arg_worklist.pop().unwrap();

            // foreach predecessor, sever the def-use edge for the value fed into this (now dead)
            // parameter slot
            let num_preds = func.blocks[dead_block_arg_block].preds.len();
            for pred_idx in 0..num_preds {
                let pred = func.blocks[dead_block_arg_block].preds[pred_idx];

                let pred_terminator_ref = {
                    let pred_insts = func.blocks[pred.pred_ref].inst_refs.borrow();
                    *pred_insts.last().unwrap()
                };
                let pred_terminator = func.insts[pred_terminator_ref];

                let edge = pred_terminator.edge(pred.edge_idx);
                let incoming_def = func.value_vecs[edge.args][dead_block_arg_idx as usize];

                let use_ = Use {
                    user: pred_terminator_ref,
                    operand_coord: OperandCoord {
                        kind: edge.args_kind,
                        idx: dead_block_arg_idx,
                    },
                };

                let remaining_uses = func.remove_use(incoming_def, use_);
                if remaining_uses == 0 {
                    match incoming_def {
                        Value::Inst(inst_ref)
                        | Value::TupleElement(inst_ref, _) => {
                            inst_worklist.push(inst_ref);
                        }
                        Value::BlockArgument(block_ref, idx) => {
                            block_arg_worklist.push((block_ref, idx));
                        },
                    }
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

    // 4. cleanup block args too
    for block in &mut func.blocks {
        if block.is_entry {
            continue
        }

        let num_block_args = block.block_args.len();
        
    }
}