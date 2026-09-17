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
//! - AVX is available (pretty much every x86-64 CPU made in the last 15 years supports it), so
//!   that the three-address instruction encodings are available

use cake_util::{IndexVec, make_type_idx};
use smallvec::SmallVec;

use crate::cir;

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

    xmm0,
    xmm1,
    xmm2,
    xmm3,
    xmm4,
    xmm5,
    xmm6,
    xmm7,
    xmm8,
    xmm9,
    xmm10,
    xmm11,
    xmm12,
    xmm13,
    xmm14,
    xmm15,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum GprOperandWidth {
    Byte,
    Word,
    Dword,
    Qword,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SseOperandWidth {
    Single,
    Double
}


#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum RegClass {
    // all gprs
    Gpr,

    // all gprs except rsp / r12, as these cannot be used as index for memory addressing
    GprNoSp,

    // xmm/ymm/zmm
    Sse
}

impl PhysReg {
    pub(crate) fn reg_class(self) -> RegClass {
        use PhysReg::*;

        if matches!(
            self,
            xmm0 | xmm1 | xmm2  | xmm3  | xmm4  | xmm5  | xmm6  | xmm7  |
            xmm8 | xmm9 | xmm10 | xmm11 | xmm12 | xmm13 | xmm14 | xmm15
        ) {
            return RegClass::Sse;
        }

        if matches!(self, rsp | r12) {
            return RegClass::Gpr;
        }

        RegClass::GprNoSp
    }
}

/// Identifies a single operand within a MachineInst; only SSA values for now.
/// References to StackSlot, Function, Data are not considered.
#[derive(Clone, Copy)]
pub(crate) struct MachineInstOperandCoord {
    kind: u32,
    idx: u32,
}

/// For BlockParam's, this is the index of the block param
/// For Inst, this is always zero for instructions that only have a single output,
/// and the index of the output for instructions with >1 output.
#[derive(Clone, Copy)]
pub(crate) struct VRegDefCoord(u32);

#[derive(Clone, Copy)]
pub(crate) enum VRegDef {
    BlockParam(MachineBlockRef, VRegDefCoord),
    Inst(MachineInstRef, VRegDefCoord),
}

#[derive(Clone, Copy)]
pub(crate) struct VRegUse {
    inst: MachineInstRef,
    coord: MachineInstOperandCoord,
}

impl MachineInstOperandCoord {
    fn direct(idx: u32) -> Self {
        Self { kind: 0, idx }
    }

    fn indirect(kind: u32, idx: u32) -> Self {
        Self { kind, idx }
    }
}

#[derive(Clone)]
pub(crate) struct VirtualReg {
    class: RegClass,
    def: VRegDef,
    uses: SmallVec<[VRegUse; 3]>
}

make_type_idx!(VRegRef, VirtualReg);

#[derive(Clone, Copy)]
pub(crate) enum Reg {
    VReg(VRegRef),
    PReg(PhysReg)
}

/// Shorthands for constructing Reg::PReg in tests
pub(crate) mod phys_regs {
    use crate::mir::{PhysReg, Reg};

    macro_rules! decl_regs {
        ($name:ident) => {
            #[allow(non_camel_case_types, reason = "x86 convention")]
            pub(crate) const $name: Reg = Reg::PReg(PhysReg::$name);
        };
        ($name:ident, $($rest:ident),*) => {
            decl_regs!($name);
            decl_regs!($($rest),*);
        };
    }

    decl_regs!(
        rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, r8, r9, r10, r11, r12, r13, r14, r15,
        xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15
    );
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

#[derive(Clone, Copy)]
pub(crate) enum MemOperand {
    PcRelativeFn {
        target: MachineFunctionRef
    },
    PcRelativeData {
        target: cir::DataRef,
    },
    // index is not allowed to be physical register rsp or r12, 
    // this is enforced by register allocator
    Full {
        base: Reg,
        index: Reg,
        scale: MemOperandScale,
        disp: MemOperandDisplacement,
    },
    BasePlusDisp {
        base: Reg,
        disp: MemOperandDisplacement,
    },
    AbsoluteDisp {
        disp: MemOperandDisplacement,
    }
}

/// The width of the operand is tracked once, on the `MachineInst`, and thus we throw away the width info
/// on CIR constants.
#[derive(Debug, Clone, Copy)]
pub(crate) struct ImmediateOperand(pub(crate) i64);

/// x86-64 has no support for floating-point immediates, so fail the conversion if we see one
#[derive(Debug)]
pub(crate) struct ImmediateOperandError;

impl TryFrom<cir::Constant> for ImmediateOperand {
    type Error = ImmediateOperandError;

    fn try_from(value: cir::Constant) -> Result<Self, Self::Error> {
        let imm = match value {
            cir::Constant::i8(v) => ImmediateOperand(v as i64),
            cir::Constant::i16(v) => ImmediateOperand(v as i64),
            cir::Constant::i32(v) => ImmediateOperand(v as i64),
            cir::Constant::i64(v) => ImmediateOperand(v as i64),
            cir::Constant::f32(_) => return Err(ImmediateOperandError),
            cir::Constant::f64(_) => return Err(ImmediateOperandError),
        };

        Ok(imm)
    }
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

/// MachineInst are the opcodes of MIR, and correspond directly to a 
/// single x86_64 opcode and addressing mode selection to simplify final assembly
/// emission. MachineInst's remain in three-address SSA form until register allocation, 
/// using virtual registers and block parameters; it is the register allocator's job
/// to perform out-of-SSA and two-address legalization for x86_64. 
pub(crate) enum MachineInst {
    // lea %dst, [%op2]
    Lea {
        dst: Reg,
        op2: MemOperand,
        width: GprOperandWidth
    },

    // add %dst/op1, %op2
    AddRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg,
        width: GprOperandWidth  
    },
    // add %dst/op1, [%op2]
    AddMemToReg {
        dst: Reg,
        op1: Reg,
        op2: MemOperand,
        width: GprOperandWidth,
    },
    // add [%op1], %op2
    AddRegToMem {
        op1: MemOperand,
        op2: Reg,
        width: GprOperandWidth
    },
    // add %dst/op1, %op2
    AddImmToReg {
        dst: Reg,
        op1: Reg,
        op2: ImmediateOperand,
        width: GprOperandWidth
    },
    // add [%op1], %op2
    AddImmToMem {
        op1: MemOperand,
        op2: ImmediateOperand,
        width: GprOperandWidth
    },
    // vaddss %dst, %op1, %op2
    FAddRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg,
        width: SseOperandWidth
    },
    // vaddss %dst, %op1, [%op2]
    FAddMemToReg {
        dst: Reg,
        op1: Reg,
        op2: MemOperand,
        width: SseOperandWidth,
    },

    // mov %dst, [%op2]
    Load {
        dst: Reg,
        op2: MemOperand,
        width: GprOperandWidth
    },
    // mov %dst, %op2
    LoadImm {
        dst: Reg,
        op2: ImmediateOperand,
        width: GprOperandWidth
    },
    // vmovss %dst, [%op2]
    LoadFloat {
        dst: Reg,
        op2: MemOperand,
        width: SseOperandWidth
    },
    // mov [%op1], %op2
    StoreReg {
        op1: MemOperand,
        op2: Reg,
        width: GprOperandWidth
    },
    // mov [%op1], %op2
    StoreImm {
        op1: MemOperand,
        op2: ImmediateOperand,
        width: GprOperandWidth
    },
    // vmovss [%op1], %op2
    StoreFloat {
        op1: MemOperand,
        op2: Reg,
        width: SseOperandWidth
    },

    // mov %dst, %op2
    Mov {
        dst: Reg,
        op2: Reg,
        width: GprOperandWidth
    },
    // cmov(cc) %dst, %op2
    Cmov {
        dst: Reg,
        op2: Reg,
        cond: Condition,
        width: GprOperandWidth
    },
    // xchg %op1, %op2
    Xchg {
        op1: Reg,
        op2: Reg,
        width: GprOperandWidth,
    }, 

    // in x86, writing to 32-bit register clears the upper 32 bits
    // so, zero-extend (`movzx`) is only needed when widening from a unsigned byte or unsigned short. 
    // on the other hand, sign-extend (`movsx`) is also needed when widening from a signed int, since the default
    // behavior is zero extension

    // movzx %dst, %op2
    ZeroExtend {
        dst: Reg,
        op2: Reg,
        dst_width: GprOperandWidth,
        op2_width: GprOperandWidth,
    },
    // movsx %dst, %op2
    SignExtend {
        dst: Reg,
        op2: Reg,
        dst_width: GprOperandWidth,
        op2_width: GprOperandWidth,
    },

    // push %op1
    Push {
        op1: Reg,
        width: GprOperandWidth
    },
    PushImm {
        op1: ImmediateOperand,
        width: GprOperandWidth
    },
    // pop %dst
    Pop {
        dst: Reg,
        width: GprOperandWidth
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
        width: GprOperandWidth
    },
    MulMemToReg {
        dst: Reg,
        op1: Reg,
        op2: MemOperand,
        width: GprOperandWidth
    },
    MulRegWithImm {
        dst: Reg,
        op1: Reg,
        op2: ImmediateOperand,
        width: GprOperandWidth,
    },
    MulMemWithImm {
        dst: Reg,
        op1: MemOperand,
        op2: ImmediateOperand,
        width: GprOperandWidth,
    },

    // x86 division operates on rdx:rax (or subregisters thereof) as the dividend, 
    // outputting quotient in rax and remainder in rdx
    UDivByReg {
        dst_quo: Reg,
        dst_rem: Reg,
        op1: Reg,
        width: GprOperandWidth,
    },
    UDivByMem {
        dst_quo: Reg,
        dst_rem: Reg,
        op1: MemOperand,
        width: GprOperandWidth,
    },
    SDivByReg {
        dst_quo: Reg,
        dst_rem: Reg,
        op1: Reg,
        width: GprOperandWidth,
    },
    SDivByMem {
        dst_quo: Reg,
        dst_rem: Reg,
        op1: MemOperand,
        width: GprOperandWidth,
    },

    // cwd / cdq / cqo, depending on width
    PrepareDiv {
        width: GprOperandWidth
    },

    // and %op1, %op2
    AndRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg,
        width: GprOperandWidth,
    },

    // or %op1, %op2
    OrRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg,
        width: GprOperandWidth,
    },

    // xor %op1, %op2
    XorRegToReg {
        dst: Reg,
        op1: Reg,
        op2: Reg,
        width: GprOperandWidth,
    },

    // not %op1
    NotReg {
        dst: Reg,
        op1: Reg,
        width: GprOperandWidth,
    },

    // test %op1, %op2
    TestRegWithImm {
        op1: Reg,
        op2: ImmediateOperand,
        width: GprOperandWidth,
    },
    // cmp %op1, %op2
    CmpRegWithImm {
        op1: Reg,
        op2: ImmediateOperand,
        width: GprOperandWidth,
    },

    JmpWithCond {
        cond: Condition,
        target: MachineBlockRef,
    },
}

make_type_idx!(MachineInstRef, MachineInst);

#[derive(Clone)]
struct MachineBlock {
    irefs: Vec<MachineInstRef>,
}

impl MachineBlock {
    fn new() -> Self {
        MachineBlock { irefs: vec![] }
    }
}

make_type_idx!(MachineBlockRef, MachineBlock);

struct MachineFunctionDefinition {
    insts: IndexVec<MachineInstRef, MachineInst>,
    vregs: IndexVec<VRegRef, VirtualReg>,

    blocks: IndexVec<MachineBlockRef, MachineBlock>,
}

struct MachineFunction {
    name: String,
    definition: Option<MachineFunctionDefinition>,
}

make_type_idx!(MachineFunctionRef, MachineFunction);

struct MachineModule {
    functions: IndexVec<MachineFunctionRef, MachineFunction>,

    // signatures and data can be imported directly from the CIR module
    signatures: IndexVec<cir::FuncRef, cir::Signature>,
    data: IndexVec<cir::DataRef, cir::Data>,
}

impl MachineModule {
    pub(crate) fn add_data(&mut self, data: cir::Data) -> cir::DataRef {
        cir::DataRef::from_push2(&mut self.data, data)
    }
}

mod cir2mir;