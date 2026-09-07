//! MIR is x86-64 specific machine IR
//! - Opcodes are meant to be 1-1 with a small subset of x86-64 instructions 
//!   (ideally, those which are three-address like `lea`)
//! - Prior to register allocation, MIR remains in three-address form with block params (i.e. φ-nodes).
//!   It is the responsibility of the register allocator to eliminate φ's and tie one the `srcA` + `dst` together
//!   into the same physical register to be compliant with x86's two-address encoding
//! - Lowering CIR to MIR requires lowering 
//!   1. calling convention, which is done by reading / writing SSA values from physical registers in prologue / epilogue
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
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum PhysReg {
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

impl PhysReg {
    pub(crate) fn is_extended_reg(self) -> bool {
        match self {
            PhysReg::r8 | PhysReg::r9 | PhysReg::r10 | PhysReg::r11 | 
            PhysReg::r12 | PhysReg::r13 | PhysReg::r14 | PhysReg::r15 => true,
            _ => false
        }
    }
    
    /// Encoding of the physical register in ModR/M or SIB bytes. Needs to be combined with
    /// REX prefix for r8-r15
    pub(crate) fn encoding(self) -> u8 {
        match self {
            PhysReg::rax => 0b000,
            PhysReg::rbx => 0b011,
            PhysReg::rcx => 0b001,
            PhysReg::rdx => 0b010,
            PhysReg::rsi => 0b110,
            PhysReg::rdi => 0b111,
            PhysReg::rbp => 0b101,
            PhysReg::rsp => 0b100,
            PhysReg::r8  => 0b000,
            PhysReg::r9  => 0b001,
            PhysReg::r10 => 0b010,
            PhysReg::r11 => 0b011,
            PhysReg::r12 => 0b100,
            PhysReg::r13 => 0b101,
            PhysReg::r14 => 0b110,
            PhysReg::r15 => 0b111,
        }
    }
}

#[derive(Clone, Copy)]
struct VirtualReg {
    
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum OperandWidth {
    Byte,
    Word,
    Dword,
    Qword
}


#[derive(Clone, Copy)]
pub(crate) enum Reg {
    VReg(VirtualReg, OperandWidth),
    PReg(PhysReg, OperandWidth)
}

pub(crate) mod phys_regs {
    use crate::mir::{OperandWidth, PhysReg, Reg};

    pub(crate) const rax: Reg = Reg::PReg(PhysReg::rax, OperandWidth::Qword);
    pub(crate) const rbx: Reg = Reg::PReg(PhysReg::rbx, OperandWidth::Qword);
    pub(crate) const rcx: Reg = Reg::PReg(PhysReg::rcx, OperandWidth::Qword);
    pub(crate) const rdx: Reg = Reg::PReg(PhysReg::rdx, OperandWidth::Qword);
    pub(crate) const rsi: Reg = Reg::PReg(PhysReg::rsi, OperandWidth::Qword);
    pub(crate) const rdi: Reg = Reg::PReg(PhysReg::rdi, OperandWidth::Qword);
    pub(crate) const rsp: Reg = Reg::PReg(PhysReg::rsp, OperandWidth::Qword);
    pub(crate) const rbp: Reg = Reg::PReg(PhysReg::rbp, OperandWidth::Qword);
    pub(crate) const r8: Reg = Reg::PReg(PhysReg::r8, OperandWidth::Qword);
    pub(crate) const r9: Reg = Reg::PReg(PhysReg::r9, OperandWidth::Qword);
    pub(crate) const r10: Reg = Reg::PReg(PhysReg::r10, OperandWidth::Qword);
    pub(crate) const r11: Reg = Reg::PReg(PhysReg::r11, OperandWidth::Qword);
    pub(crate) const r12: Reg = Reg::PReg(PhysReg::r12, OperandWidth::Qword);
    pub(crate) const r13: Reg = Reg::PReg(PhysReg::r13, OperandWidth::Qword);
    pub(crate) const r14: Reg = Reg::PReg(PhysReg::r14, OperandWidth::Qword);
    pub(crate) const r15: Reg = Reg::PReg(PhysReg::r15, OperandWidth::Qword);

    pub(crate) const eax: Reg = Reg::PReg(PhysReg::rax, OperandWidth::Dword);
    pub(crate) const ebx: Reg = Reg::PReg(PhysReg::rbx, OperandWidth::Dword);
    pub(crate) const ecx: Reg = Reg::PReg(PhysReg::rcx, OperandWidth::Dword);
    pub(crate) const edx: Reg = Reg::PReg(PhysReg::rdx, OperandWidth::Dword);
    pub(crate) const esi: Reg = Reg::PReg(PhysReg::rsi, OperandWidth::Dword);
    pub(crate) const edi: Reg = Reg::PReg(PhysReg::rdi, OperandWidth::Dword);
    pub(crate) const esp: Reg = Reg::PReg(PhysReg::rsp, OperandWidth::Dword);
    pub(crate) const ebp: Reg = Reg::PReg(PhysReg::rbp, OperandWidth::Dword);
    pub(crate) const r8d: Reg = Reg::PReg(PhysReg::r8, OperandWidth::Dword);
    pub(crate) const r9d: Reg = Reg::PReg(PhysReg::r9, OperandWidth::Dword);
    pub(crate) const r10d: Reg = Reg::PReg(PhysReg::r10, OperandWidth::Dword);
    pub(crate) const r11d: Reg = Reg::PReg(PhysReg::r11, OperandWidth::Dword);
    pub(crate) const r12d: Reg = Reg::PReg(PhysReg::r12, OperandWidth::Dword);
    pub(crate) const r13d: Reg = Reg::PReg(PhysReg::r13, OperandWidth::Dword);
    pub(crate) const r14d: Reg = Reg::PReg(PhysReg::r14, OperandWidth::Dword);
    pub(crate) const r15d: Reg = Reg::PReg(PhysReg::r15, OperandWidth::Dword);

    pub(crate) const ax: Reg = Reg::PReg(PhysReg::rax, OperandWidth::Word);
    pub(crate) const bx: Reg = Reg::PReg(PhysReg::rbx, OperandWidth::Word);
    pub(crate) const cx: Reg = Reg::PReg(PhysReg::rcx, OperandWidth::Word);
    pub(crate) const dx: Reg = Reg::PReg(PhysReg::rdx, OperandWidth::Word);
    pub(crate) const si: Reg = Reg::PReg(PhysReg::rsi, OperandWidth::Word);
    pub(crate) const di: Reg = Reg::PReg(PhysReg::rdi, OperandWidth::Word);
    pub(crate) const sp: Reg = Reg::PReg(PhysReg::rsp, OperandWidth::Word);
    pub(crate) const bp: Reg = Reg::PReg(PhysReg::rbp, OperandWidth::Word);
    pub(crate) const r8w: Reg = Reg::PReg(PhysReg::r8, OperandWidth::Word);
    pub(crate) const r9w: Reg = Reg::PReg(PhysReg::r9, OperandWidth::Word);
    pub(crate) const r10w: Reg = Reg::PReg(PhysReg::r10, OperandWidth::Word);
    pub(crate) const r11w: Reg = Reg::PReg(PhysReg::r11, OperandWidth::Word);
    pub(crate) const r12w: Reg = Reg::PReg(PhysReg::r12, OperandWidth::Word);
    pub(crate) const r13w: Reg = Reg::PReg(PhysReg::r13, OperandWidth::Word);
    pub(crate) const r14w: Reg = Reg::PReg(PhysReg::r14, OperandWidth::Word);
    pub(crate) const r15w: Reg = Reg::PReg(PhysReg::r15, OperandWidth::Word);

    pub(crate) const al: Reg = Reg::PReg(PhysReg::rax, OperandWidth::Byte);
    pub(crate) const bl: Reg = Reg::PReg(PhysReg::rbx, OperandWidth::Byte);
    pub(crate) const cl: Reg = Reg::PReg(PhysReg::rcx, OperandWidth::Byte);
    pub(crate) const dl: Reg = Reg::PReg(PhysReg::rdx, OperandWidth::Byte);
    pub(crate) const sil: Reg = Reg::PReg(PhysReg::rsi, OperandWidth::Byte);
    pub(crate) const dil: Reg = Reg::PReg(PhysReg::rdi, OperandWidth::Byte);
    pub(crate) const spl: Reg = Reg::PReg(PhysReg::rsp, OperandWidth::Byte);
    pub(crate) const bpl: Reg = Reg::PReg(PhysReg::rbp, OperandWidth::Byte);
    pub(crate) const r8b: Reg = Reg::PReg(PhysReg::r8, OperandWidth::Byte);
    pub(crate) const r9b: Reg = Reg::PReg(PhysReg::r9, OperandWidth::Byte);
    pub(crate) const r10b: Reg = Reg::PReg(PhysReg::r10, OperandWidth::Byte);
    pub(crate) const r11b: Reg = Reg::PReg(PhysReg::r11, OperandWidth::Byte);
    pub(crate) const r12b: Reg = Reg::PReg(PhysReg::r12, OperandWidth::Byte);
    pub(crate) const r13b: Reg = Reg::PReg(PhysReg::r13, OperandWidth::Byte);
    pub(crate) const r14b: Reg = Reg::PReg(PhysReg::r14, OperandWidth::Byte);
    pub(crate) const r15b: Reg = Reg::PReg(PhysReg::r15, OperandWidth::Byte);
}

#[derive(Clone, Copy)]
pub(crate) enum MemOperandDisplacement {
    Disp32(u32),
    Disp8(u8),
    Zero
}

impl MemOperandDisplacement {
    pub(crate) fn as_u32(self) -> u32 {
        match self {
            MemOperandDisplacement::Disp32(v) => v,
            MemOperandDisplacement::Disp8(v) => v as u32,
            MemOperandDisplacement::Zero => 0u32,
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum MemOperandScale {
    One,
    Two,
    Four,
    Eight
}

impl MemOperandScale {
    pub(crate) fn as_u32(self) -> u32 {
        match self {
            MemOperandScale::One => 1,
            MemOperandScale::Two => 2,
            MemOperandScale::Four => 4,
            MemOperandScale::Eight => 8,
        }
    }
}

// the width field roughly corresponds to "BYTE PTR" / "DWORD PTR" / "QWORD PTR" in assembler
// syntax; it is not used unless the memory operand is the destination of some instruction 
#[derive(Clone, Copy)]
pub(crate) enum MemOperand {
    PcRelative {
        disp: MemOperandDisplacement,
        width: OperandWidth
    },
    // index is not allowed to be rsp or r12, since that r/m is used for SIB
    Full {
        base: Reg,
        index: Reg,
        scale: MemOperandScale,
        disp: MemOperandDisplacement,
        width: OperandWidth
    },
    BasePlusDisp {
        base: Reg,
        disp: MemOperandDisplacement,
        width: OperandWidth
    },
    AbsoluteDisp {
        disp: MemOperandDisplacement,
        width: OperandWidth
    }
}

impl MemOperand {
    pub(crate) fn width(&self) -> OperandWidth {
        match self {
            MemOperand::PcRelative { width, .. }
            | MemOperand::Full { width, .. }
            | MemOperand::BasePlusDisp { width, .. }
            | MemOperand::AbsoluteDisp { width, .. } => *width,
        }
    }
}

pub(crate) struct ImmediateOperand {
    pub(crate) value: u64,
    pub(crate) width: OperandWidth,
}

pub(crate) enum Condition {
    // overflow
    O,
    No,

    // zero / not zero (equal / not equal) 
    Z,
    Nz,

    // "below" and "above" are used for unsigned comparisons
    B,
    Ae,
    Be,
    A,

    // "less" and "greater" are used for signed comparisons
    L,
    Ge,
    Le,
    G
}

/// MachineInst are the opcodes of MIR, and correspond directly to a single x86 opcode + addressing mode selection
/// The width of the operation (usually) comes from the OperandWidth field of its registers / memory operands, 
/// except for sign-extend / zero-extend. `ImmediateOperand`s are sign-extended if not full-width.
pub(crate) enum MachineInst {
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

    // mov %op1, %op2
    Mov {
        dst: Reg,
        op2: Reg
    },
    Xchg {
        op1: Reg,
        op2: Reg
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
    PushImm {
        op1: ImmediateOperand,
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
        target: MachineFunctionRef,
    },
    CallIndirect {
        target: Reg
    },
    // ret
    Ret,

    // x86 offers a one-operand form `imul`, but it always writes to rdx:rax, which is pretty inflexible
    // this is mostly useful if you want a true 128-bit multiplication, which is not really required for us
    // thus, we only encode two-operand and two-operand one-imm form
    MulRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg,
    },
    MulMemToReg {
        dst: Reg,
        op1: Reg,
        op2: MemOperand
    },
    MulRegWithImm {
        dst: Reg,
        op1: Reg,
        op2: ImmediateOperand
    },
    MulMemWithImm {
        dst: Reg,
        op1: MemOperand,
        op2: ImmediateOperand,
    },

    // x86 division operates on rdx:rax (or subregisters thereof) as the dividend, 
    // outputting quotient in rax and remainder in rdx
    UDivByReg {
        dst_quo: Reg,
        dst_rem: Reg,
        op1: Reg,
    },
    UDivByMem {
        dst_quo: Reg,
        dst_rem: Reg,
        op1: MemOperand
    },
    SDivByReg {
        dst_quo: Reg,
        dst_rem: Reg,
        op1: Reg,
    },
    SDivByMem {
        dst_quo: Reg,
        dst_rem: Reg,
        op1: MemOperand
    },

    // cwd / cdq / cqo, depending on width
    PrepareDiv {
        width: OperandWidth       
    },

    // and %op1, %op2
    AndRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg,
    },

    // or %op1, %op2
    OrRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg,
    },

    // xor %op1, %op2
    XorRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg
    },
    
    // not %op1
    NotReg {
        dst: Reg,
        op1: Reg,
    },

    // test %op1, %op2
    TestRegWithImm {
        op1: Reg,
        op2: ImmediateOperand
    },
    // cmp %op1, %op2
    CmpRegWithImm {
        op1: Reg,
        op2: ImmediateOperand
    },

    JmpWithCond {
        cond: Condition,
        target: MachineBlockRef,
    },


}

make_type_idx!(MachineInstRef, MachineInst);

struct MachineBlock {
    irefs: Vec<MachineInstRef>,
}

make_type_idx!(MachineBlockRef, MachineBlock);

struct MachineFunction {
    insts: Vec<MachineInst>,
}

make_type_idx!(MachineFunctionRef, MachineFunction);

mod cir2mir;