//! Instruction selection to lower CIR -> MIR
//! 
//! Operates directly on linear IR the whole way through. Instruction selection is 
//! done by matching "tiles" against def-use trees in CIR in a greedy manner (maximal munch).
//! We use the number of uses of an SSA value to determine whether it can be tiled over by a pattern.
//! Concretely, in a use-def "tree" of CIR of the form
//!          %4 = add %1 %2
//!         /        \
//!        %1      %2 = load %3
//!                   |
//!                  %3
//! Assuming that:
//! - we are matching %4
//! - %2 does not have any other uses
//! Then, it would be safe to lower this to AddMemToReg, since nothing else needs %2, 
//! so it's ok to fold the memory load in.
//!
//! Instruction selection is performed globally by iterating over basic blocks in post-order
//! and selecting uses before defs in a dataflow-like fashion. This is similar to
//! LLVM's GlobalISel.

use cake_util::IndexVec;
use rustc_hash::FxHashMap;

use crate::{
    cir::{BlockRef, Constant, Function, FunctionDefinition, InstRef, Value, post_order}, mir::{ImmediateOperand, MachineInst, MachineInstRef, Reg, VirtualReg}
};

// Whether a CIR instruction has already been selected
enum SelectionStatus {
    // It hasn't been selected yet (it might be the leaf node of some existing tiles)
    NotSelectedYet,
    
    // It has already been selected as an inner node or root of some tile
    Selected
}


struct InstructionSelector {
    // TODO: it might be more efficient to mirror the organization of values in CIR 
    // rather than using a hashmap
    vreg_by_value: FxHashMap<Value, VirtualReg>

}

fn select_add() {
    let x: Value = todo!();
    let y: Value = todo!();

    
}

// When selecting an instruction, we check multiple patterns (in decreasing order of complexity, hence maximal munch)
// until one matches. Once it is matched, we need to allocate MIR virtual registers, record the mapping of input CIR operands
// of the rule to the appropriate MIR virtual regs, look up the mapping CIR outputs of the rule, and finally emit
// the MIR instructions that make up the rule. 
fn select_inst(function: &FunctionDefinition, inst_ref: InstRef) {
    let inst = &function.insts[inst_ref];

    use crate::cir::Inst;

    match inst {
        Inst::Constant { val } => {
            // MovImm

            // allocate vreg

            // no inputs

            let output_value = Value::Inst(inst_ref);
            let output_vreg = vreg_by_value[output_value];
            
            let minst = MachineInst::MovImm { 
                dst: Reg::VReg(output_vreg), 
                op2: val.try_into().expect()
            }
        },
        Inst::Add { a, b } => todo!(),
        Inst::Sub { a, b } => todo!(),
        Inst::Mul { a, b } => todo!(),
        Inst::Div { a, b } => todo!(),
        Inst::Modulo { a, b } => todo!(),
        Inst::And { a, b } => todo!(),
        Inst::Or { a, b } => todo!(),
        Inst::Xor { a, b } => todo!(),
        Inst::Shl { a, b } => todo!(),
        Inst::Ashr { a, b } => todo!(),
        Inst::Lshr { a, b } => todo!(),
        Inst::Icmp { mode, a, b, signed } => todo!(),
        Inst::Fadd { a, b } => todo!(),
        Inst::Fsub { a, b } => todo!(),
        Inst::Fmul { a, b } => todo!(),
        Inst::Fdiv { a, b } => todo!(),
        Inst::Fcmp { mode, a, b } => todo!(),
        Inst::IntToFp { v } => todo!(),
        Inst::FpToInt { v } => todo!(),
        Inst::Load { addr } => todo!(),
        Inst::Store { addr, val } => todo!(),
        Inst::StackAddr { slot } => todo!(),
        Inst::Zext { v } => todo!(),
        Inst::Sext { v } => todo!(),
        Inst::Truncate { v } => todo!(),
        Inst::FpCast { v } => todo!(),
        Inst::PtrAdd { ptr, offset } => todo!(),
        Inst::PtrToInt { v } => todo!(),
        Inst::IntToPtr { v } => todo!(),
        Inst::Select { cond, x, y } => todo!(),
        Inst::BranchIf { cond, con, con_args, alt, alt_args } => todo!(),
        Inst::Return { values } => todo!(),
        Inst::Jump { target, arguments } => todo!(),
        Inst::Call { func, arguments } => todo!(),
        Inst::CallIndirect { callee_sig, func_ptr, arguments } => todo!(),
        Inst::FuncAddr { func } => todo!(),
        Inst::DataAddr { data } => todo!(),
        Inst::Intrinsic { intrinsic, arguments } => todo!(),
    }
}


fn select_block(function: &FunctionDefinition, block_ref: BlockRef) {
    let block = &function.blocks[block_ref];

    for iref in block.inst_refs.borrow().iter().rev() {

    }
}

fn select_function(function: &FunctionDefinition) {
    let post_order_traversal = post_order::post_order(function);
    for bref in post_order_traversal {
        
    }
}

