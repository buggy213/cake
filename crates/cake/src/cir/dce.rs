//! Dead Code Elimination
//! 
//! A simple optimization which removes trivially dead code, through a basic worklist algorithm.
//! This pass preserves CFG (i.e. it doesn't remove blocks with no side effects), and it won't catch 
//! referential cycles in the IR (e.g. a counter incrementing in a loop)

use crate::cir::{BlockArgRef, BlockRef, FunctionDefinition, InstRef, OperandCoord, Use, Value};

fn eliminate_dead_code(func: &mut FunctionDefinition) {
    // 1. mark all obviously dead instructions, putting them onto a worklist
    //    do the same for block arguments as well
    let mut inst_worklist: Vec<InstRef> = Vec::with_capacity(64);
    let mut block_arg_worklist: Vec<(BlockRef, BlockArgRef)> = Vec::with_capacity(64);
    for (block_ref, block) in BlockRef::enumerate2(&func.blocks) {
        for &iref in block.inst_refs.borrow().iter() {
            let inst = func.insts[iref];
            if func.inst_uses[iref].len() == 0 && !inst.has_side_effects() {
                inst_worklist.push(iref);
            }
        }

        // NOTE: block args of the entry block are immune from dead-code elimination, since they are
        //       in one-to-one correspondence with the function signature / ABI parameters. the final
        //       cleanup skips over them
        if !block.is_entry {
            for &arg_ref in block.block_arg_order.iter() {
                if block.block_arg_uses[arg_ref].len() == 0 {
                    block_arg_worklist.push((block_ref, arg_ref));
                }
            }
        }
    }

    // 2. pop items off the worklists and remove them from Use list of operands
    // - for block args, unlink the def-use chain of their terminators only, modifying worklist accordingly; 
    //   defer the actual deletion of the block arg for the end
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
            let (dead_block_arg_block, dead_arg_ref) = block_arg_worklist.pop().unwrap();
            let dead_block_arg_idx = func.blocks[dead_block_arg_block].block_arg_order
                .iter()
                .position(|&r| r == dead_arg_ref)
                .expect("dead block arg missing from its own block_arg_order");

            let num_preds = func.blocks[dead_block_arg_block].preds.len();
            for pred_idx in 0..num_preds {
                let pred = func.blocks[dead_block_arg_block].preds[pred_idx];

                let pred_terminator_ref = {
                    let pred_insts = func.blocks[pred.pred_ref].inst_refs.borrow();
                    *pred_insts.last().unwrap()
                };
                let pred_terminator = func.insts[pred_terminator_ref];

                let edge = pred_terminator.edge(pred.edge_idx);
                let incoming_def = func.value_vecs[edge.args][dead_block_arg_idx];
                let use_ = Use {
                    user: pred_terminator_ref,
                    operand_coord: OperandCoord {
                        kind: edge.args_kind,
                        idx: dead_block_arg_idx as u32,
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

    // 3. do a single sweep to fixup links from block -> inst, fully erasing the dead insts, and elimination
    //    of the dead block args
    
    for block_ref in BlockRef::iter(&func.blocks) {
        let irefs = func.blocks[block_ref].inst_refs.get_mut();
        irefs.retain(|&iref| {
            func.inst_block[iref].is_some()
        });

        // eliminate dead block args
        if func.blocks[block_ref].is_entry {
            continue;
        }

        let mut new_idx = 0;
        let num_block_args = func.blocks[block_ref].block_arg_order.len();
        for old_idx in 0..num_block_args {
            let arg_ref = func.blocks[block_ref].block_arg_order[old_idx];
            if func.blocks[block_ref].block_arg_uses[arg_ref].len() > 0 {
                if new_idx != old_idx {
                    let num_preds = func.blocks[block_ref].preds.len();
                    for pred_idx in 0..num_preds {
                        let pred = func.blocks[block_ref].preds[pred_idx];
                        let (pred_term, edge) = {
                            let pred_insts = &func.blocks[pred.pred_ref].inst_refs.borrow();
                            let &terminator_inst_ref = pred_insts.last().unwrap();
                            (terminator_inst_ref, func.insts[terminator_inst_ref].edge(pred.edge_idx))
                        };

                        let old_val = func.value_vecs[edge.args][old_idx];
                        func.value_vecs[edge.args][new_idx] = old_val;
                        func.remove_use(old_val, Use { user: pred_term, operand_coord: OperandCoord { kind: edge.args_kind, idx: old_idx as u32 } });
                        func.add_use(old_val, Use { user: pred_term, operand_coord: OperandCoord { kind: edge.args_kind, idx: new_idx as u32 } });
                    }

                    func.blocks[block_ref].block_arg_order[new_idx] = arg_ref;
                }
                new_idx += 1;
            }
        }

        for &pred in &func.blocks[block_ref].preds {
            let edge = {
                let pred_insts = &func.blocks[pred.pred_ref].inst_refs.borrow();
                let &terminator_inst_ref = pred_insts.last().unwrap();
                func.insts[terminator_inst_ref].edge(pred.edge_idx)
            };

            func.value_vecs[edge.args].truncate(new_idx);
        }

        func.blocks[block_ref].block_arg_order.truncate(new_idx);
    }
}

// print-based inspection tests: build a small function, run DCE, print the result so it can be
// eyeballed. no assertions yet — meant as a starting point for lit-style golden-output tests later
#[cfg(test)]
mod test {
    use super::*;
    use crate::cir::{Module, Signature, Type};

    #[test]
    fn dead_arithmetic() {
        let mut module = Module::new();
        let func = module.add_function("dead_arithmetic".into(), Signature::new(vec![], vec![]));
        let mut builder = module.define_function(func);

        let a = builder.insert().const_i32(1);
        let b = builder.insert().const_i32(2);
        let _dead = builder.insert().add(a, b); // never used
        builder.insert().ret(&[]);

        eliminate_dead_code(builder.func);
        drop(builder);

        println!("{}", &module.functions()[func]);
    }

    #[test]
    fn cascading_dead_chain() {
        let mut module = Module::new();
        let func = module.add_function("cascading_dead_chain".into(), Signature::new(vec![], vec![]));
        let mut builder = module.define_function(func);

        let a = builder.insert().const_i32(1);
        let b = builder.insert().add(a, a);
        let _c = builder.insert().add(b, b); // whole chain is unused
        builder.insert().ret(&[]);

        eliminate_dead_code(builder.func);
        drop(builder);

        println!("{}", &module.functions()[func]);
    }

    #[test]
    fn used_arithmetic_survives() {
        let mut module = Module::new();
        let func = module.add_function("used_arithmetic_survives".into(), Signature::new(vec![], vec![Type::i32]));
        let mut builder = module.define_function(func);

        let a = builder.insert().const_i32(1);
        let b = builder.insert().const_i32(2);
        let sum = builder.insert().add(a, b);
        builder.insert().ret(&[sum]);

        eliminate_dead_code(builder.func);
        drop(builder);

        println!("{}", &module.functions()[func]);
    }

    #[test]
    fn side_effecting_inst_survives_unused() {
        let mut module = Module::new();
        let func = module.add_function("side_effecting_inst_survives_unused".into(), Signature::new(vec![], vec![]));
        let mut builder = module.define_function(func);

        let slot = builder.add_stack_slot(4, 4);
        let addr = builder.insert().stack_addr(slot);
        let val = builder.insert().const_i32(7);
        builder.insert().store(addr, val); // side-effecting, produces no value to be "used"
        builder.insert().ret(&[]);

        eliminate_dead_code(builder.func);
        drop(builder);

        println!("{}", &module.functions()[func]);
    }

    #[test]
    fn dead_block_arg_and_its_incoming_value() {
        let mut module = Module::new();
        let func = module.add_function("dead_block_arg_and_its_incoming_value".into(), Signature::new(vec![], vec![]));
        let mut builder = module.define_function(func);

        let entry = BlockRef(0);
        let b1 = builder.add_block();

        builder.set_block(b1);
        let _unused_arg = builder.add_block_arg(Type::i32);

        builder.set_block(entry);
        let c = builder.insert().const_i32(42); // only ever fed into the dead arg above
        builder.insert().jmp(b1, &[c]);

        builder.set_block(b1);
        builder.insert().ret(&[]);

        eliminate_dead_code(builder.func);
        drop(builder);

        println!("{}", &module.functions()[func]);
    }

    #[test]
    fn compacts_mixed_live_and_dead_block_args() {
        let mut module = Module::new();
        let func = module.add_function(
            "compacts_mixed_live_and_dead_block_args".into(),
            Signature::new(vec![], vec![Type::i32]),
        );
        let mut builder = module.define_function(func);

        let entry = BlockRef(0);
        let b1 = builder.add_block();

        builder.set_block(b1);
        let _dead_arg = builder.add_block_arg(Type::i32);
        let live_arg = builder.add_block_arg(Type::i32);

        builder.set_block(entry);
        let c0 = builder.insert().const_i32(1); // only feeds the dead arg
        let c1 = builder.insert().const_i32(2); // feeds the live arg
        builder.insert().jmp(b1, &[c0, c1]);

        builder.set_block(b1);
        builder.insert().ret(&[live_arg]);

        eliminate_dead_code(builder.func);
        drop(builder);

        println!("{}", &module.functions()[func]);
    }

    #[test]
    fn entry_block_args_untouched() {
        let mut module = Module::new();
        let func = module.add_function(
            "entry_block_args_untouched".into(),
            Signature::new(vec![Type::i32, Type::i32], vec![]),
        );
        let mut builder = module.define_function(func);

        // neither parameter is used anywhere
        builder.insert().ret(&[]);

        eliminate_dead_code(builder.func);
        drop(builder);

        println!("{}", &module.functions()[func]);
    }
}