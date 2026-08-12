//! MIR is x86-64 specific machine IR
//! - Opcodes are meant to be 1-1 with a small subset of x86-64 instructions 
//!   (ideally, those which are three-address like `lea`)
//! - Prior to register allocation, MIR remains in three-address form with block params (i.e. φ-nodes).
//!   It is the responsibility of the register allocator to eliminate φ's and tie one the `srcA` + `dst` together
//!   to be compliant with x86's two-address encoding
//! - Lowering CIR to MIR requires lowering 
//!   1. calling convention, which is done by reading /writing SSA values from physical registers in prologue / epilogue
//!      as well as around function calls (in addition to `push`ing and `pop`ing values as well for functions with many args)
//!   2. stack usage + ABI requirements
//! 
//! 
//! In addition, we make a few assumptions in the backend:
//! - mcmodel=small, i.e. all data and code fits within 2 GiB, so that rel32 addressing always works 
//! 

use cake_util::make_type_idx;

use crate::cir::Type;

#[allow(non_camel_case_types, reason = "x86 convention")]
enum PhysReg {
    rax,
    rbx,
    rcx,
    rdx,
    rsi,
    rdi,
    rbp,
    rsp,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
}

struct VirtualReg {
    
}

enum OperandWidth {
    Byte,
    Word,
    Dword,
    Qword
}

enum Reg {
    VReg(VirtualReg, OperandWidth),
    PReg(PhysReg, OperandWidth)
}

enum MemOperand {
    PcRelative {
        disp: u32,
        width: OperandWidth
    },
    Normal {
        base: Reg,
        index: Reg,
        scale: u32,
        disp: u32,
        width: OperandWidth
    },
}

struct ImmediateOperand {
    value: u64,
    width: OperandWidth,
}

/// MachineInst are the opcodes of MIR, and correspond directly to a single x86 opcode + addressing mode selection
/// The width of the operation (usually) comes from the OperandWidth field of its registers / memory operands, 
/// except for sign-extend / zero-extend. `ImmediateOperand`s are sign-extended if not full-width.
enum MachineInst {
    // lea %dst [%op2]
    Lea {
        dst: Reg,
        op2: MemOperand
    },

    // add %dst/op1, %op2
    AddRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg,       
    },
    // add %dst/op1, [%op2]
    AddMemToReg {
        dst: Reg,
        op1: Reg,
        op2: MemOperand,
    },
    // add [%op1], %op2
    AddRegToMem {
        op1: MemOperand,
        op2: Reg,
    },
    // add %dst/op1, %op2
    AddImmToReg {
        dst: Reg,
        op1: Reg,
        op2: ImmediateOperand
    },
    // add [%op1], %op2
    AddImmToMem {
        op1: MemOperand,
        op2: ImmediateOperand,
    },

    

    // mov %dst, [%op2]
    Load {
        dst: Reg,
        op2: MemOperand
    },
    // mov %dst, %op2
    LoadImm {
        dst: Reg,
        op2: ImmediateOperand,
    },
    // mov [%op1], %op2
    StoreReg {
        op1: MemOperand,
        op2: Reg,
    },
    // mov [%op1], %op2
    StoreImm {
        op1: MemOperand,
        op2: ImmediateOperand,
    },

    // in x86, writing to 32-bit register clears the upper 32 bits
    // so, zero-extend (`movzx`) is only needed when widening from a unsigned byte or unsigned short. 
    // on the other hand, sign-extend (`movsx`) is also needed when widening from a signed int, since the default
    // behavior is zero extension

    // movzx %dst, %op2
    ZeroExtend {
        dst: Reg,
        op2: Reg,
    },
    // movsx %dst, %op2
    SignExtend {
        dst: Reg,
        op2: Reg
    },

    // push %op1
    Push {
        op1: Reg
    },
    // pop %dst
    Pop {
        dst: Reg
    },

    // this will always be encoded as `jmp rel32` (RIP-relative addressing) due to mcmodel assumption
    Jump {
        target: MachineBlockRef
    },

    // this will always be encoded as `call rel32` (RIP-relative addressing) due to mcmodel assumption
    Call {
        // TODO: relocation
    },
    Ret
}

make_type_idx!(MachineInstRef, MachineInst);

struct MachineBlock {
    irefs: Vec<MachineInstRef>,
}

make_type_idx!(MachineBlockRef, MachineBlock);

struct MachineFunction {
    insts: Vec<MachineInst>,

}

