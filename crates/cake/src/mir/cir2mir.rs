//!

use crate::{
    cir,
    mir::{MachineFunction, MachineInst},
};

// lowering occurs at function granularity, MIR has no concept of module
/*
fn lower_cir_function(func: cir::Function) -> MachineFunction {

    let cir::Function {
        signature,
        blocks,
        stack_slots,
    } = func;

    let mut mfunc = MachineFunction {
        signature,
        blocks: Vec::with_capacity(blocks.len()),
        vregs: Vec::new(),
        stack_slots,
    };

    for block in blocks {
        lower_cir_block(block);
    }

    mfunc
}
*/

fn lower_cir_block(cir_block: cir::Block) {
    // let value_to_vreg = Vec::with_capacity(cir_block.insts.len());

    for inst in cir_block.insts {
        match inst {
            cir::Inst::BlockArgument => todo!(),
            cir::Inst::Constant { val } => match val {
                cir::Constant::i8(v) => todo!(),
                cir::Constant::u8(v) => todo!(),
                cir::Constant::i16(v) => todo!(),
                cir::Constant::u16(v) => todo!(),
                cir::Constant::i32(v) => todo!(),
                cir::Constant::u32(v) => todo!(),
                cir::Constant::i64(v) => todo!(),
                cir::Constant::u64(v) => todo!(),
                cir::Constant::f32(_) => todo!("fp insts"),
                cir::Constant::f64(_) => todo!("fp insts"),
            },
            cir::Inst::Add { a, b } => {
                todo!()
            }
            cir::Inst::Sub { a, b } => todo!(),
            cir::Inst::Mul { a, b } => todo!(),
            cir::Inst::Div { a, b } => todo!(),
            cir::Inst::Load { addr } => todo!(),
            cir::Inst::Store { addr, val } => todo!(),
            cir::Inst::StackAddr { slot } => todo!(),
            cir::Inst::CastInt { val, target_type } => todo!(),
            cir::Inst::CompareInt { a, b, mode } => todo!(),
            cir::Inst::CompareFloat { a, b, mode } => todo!(),
            cir::Inst::BranchIf { cond, con, alt } => todo!(),
            cir::Inst::Return { value } => {
                todo!()
            }
            cir::Inst::Jump { target } => todo!(),
        }
    }
}
