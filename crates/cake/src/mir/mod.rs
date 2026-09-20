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


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

    pub(crate) fn mnemonic(self) -> &'static str {
        match self {
            PhysReg::rax => "rax",
            PhysReg::rbx => "rbx",
            PhysReg::rcx => "rcx",
            PhysReg::rdx => "rdx",
            PhysReg::rsi => "rsi",
            PhysReg::rdi => "rdi",
            PhysReg::rbp => "rbp",
            PhysReg::rsp => "rsp",
            PhysReg::r8 => "r8",
            PhysReg::r9 => "r9",
            PhysReg::r10 => "r10",
            PhysReg::r11 => "r11",
            PhysReg::r12 => "r12",
            PhysReg::r13 => "r13",
            PhysReg::r14 => "r14",
            PhysReg::r15 => "r15",
            PhysReg::xmm0 => "xmm0",
            PhysReg::xmm1 => "xmm1",
            PhysReg::xmm2 => "xmm2",
            PhysReg::xmm3 => "xmm3",
            PhysReg::xmm4 => "xmm4",
            PhysReg::xmm5 => "xmm5",
            PhysReg::xmm6 => "xmm6",
            PhysReg::xmm7 => "xmm7",
            PhysReg::xmm8 => "xmm8",
            PhysReg::xmm9 => "xmm9",
            PhysReg::xmm10 => "xmm10",
            PhysReg::xmm11 => "xmm11",
            PhysReg::xmm12 => "xmm12",
            PhysReg::xmm13 => "xmm13",
            PhysReg::xmm14 => "xmm14",
            PhysReg::xmm15 => "xmm15",
        }
    }
}

/// Identifies a single operand within a MachineInst; only SSA values for now.
/// References to StackSlot, Function, Data are not considered.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MachineInstOperandCoord {
    kind: u32,
    idx: u32,
}

/// For BlockParam's, this is the index of the block param
/// For Inst, this is always zero for instructions that only have a single output,
/// and the index of the output for instructions with >1 output.
#[derive(Debug, Clone, Copy)]
pub(crate) struct VRegDefCoord(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) enum VRegDef {
    BlockParam(MachineBlockRef, VRegDefCoord),
    Inst(MachineInstRef, VRegDefCoord),
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub(crate) struct VirtualReg {
    class: RegClass,
    def: VRegDef,
    uses: SmallVec<[VRegUse; 3]>
}

make_type_idx!(VRegRef, VirtualReg);

#[derive(Debug, Clone, Copy)]
pub(crate) enum Reg {
    VReg(VRegRef),
    PReg(PhysReg)
}

impl From<VRegRef> for Reg {
    fn from(value: VRegRef) -> Self {
        Self::VReg(value)
    }
}

impl From<PhysReg> for Reg {
    fn from(value: PhysReg) -> Self {
        Self::PReg(value)
    }
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub(crate) enum StackOrReg {
    Stack(cir::StackSlotRef),
    Reg(Reg),
}

#[derive(Debug, Clone)]
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
        base: StackOrReg,
        index: Reg,
        scale: MemOperandScale,
        disp: MemOperandDisplacement,
    },
    BasePlusDisp {
        base: StackOrReg,
        disp: MemOperandDisplacement,
    },
    AbsoluteDisp {
        disp: MemOperandDisplacement,
    },
}

// helpers to construct MemOperand
impl MemOperand {
    pub(crate) fn func(func_ref: MachineFunctionRef) -> Self {
        MemOperand::PcRelativeFn { target: func_ref }
    }

    pub(crate) fn data(data_ref: cir::DataRef) -> Self {
        MemOperand::PcRelativeData { target: data_ref }
    }

    pub(crate) fn base(reg: impl Into<Reg>) -> Self {
        MemOperand::BasePlusDisp { base: StackOrReg::Reg(reg.into()), disp: MemOperandDisplacement::Zero }
    }

    pub(crate) fn base_disp(reg: impl Into<Reg>, disp: MemOperandDisplacement) -> Self {
        MemOperand::BasePlusDisp { base: StackOrReg::Reg(reg.into()), disp }
    }
}

/// The width of the operand is tracked in one place, on the `MachineInst`,
/// and thus we throw away the width info on CIR constants.
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

#[derive(Debug, Clone, Copy)]
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
#[derive(Debug)]
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

#[derive(Debug, Clone)]
struct MachineBlock {
    irefs: Vec<MachineInstRef>,
}

impl MachineBlock {
    fn new() -> Self {
        MachineBlock { irefs: vec![] }
    }
}

make_type_idx!(MachineBlockRef, MachineBlock);

#[derive(Debug)]
pub(crate) struct MachineFunctionDefinition {
    pub(crate) insts: IndexVec<MachineInstRef, MachineInst>,
    pub(crate) vregs: IndexVec<VRegRef, VirtualReg>,

    pub(crate) blocks: IndexVec<MachineBlockRef, MachineBlock>,

    pub(crate) stack_slots: IndexVec<cir::StackSlotRef, cir::StackSlot>
}

#[derive(Debug)]
struct MachineFunction {
    name: String,
    definition: Option<MachineFunctionDefinition>,
}

make_type_idx!(MachineFunctionRef, MachineFunction);

#[derive(Debug)]
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

// `impl Display` for MIR
impl std::fmt::Display for PhysReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.mnemonic())
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::VReg(vreg_ref) => write!(f, "v{}", vreg_ref.0),
            Reg::PReg(phys_reg) => write!(f, "{phys_reg}"),
        }
    }
}

impl std::fmt::Display for ImmediateOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Display for GprOperandWidth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GprOperandWidth::Byte => write!(f, "i8"),
            GprOperandWidth::Word => write!(f, "i16"),
            GprOperandWidth::Dword => write!(f, "i32"),
            GprOperandWidth::Qword => write!(f, "i64"),
        }
    }
}

impl std::fmt::Display for SseOperandWidth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SseOperandWidth::Single => write!(f, "f32"),
            SseOperandWidth::Double => write!(f, "f64"),
        }
    }
}

impl std::fmt::Display for MemOperandScale {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemOperandScale::One => write!(f, "1"),
            MemOperandScale::Two => write!(f, "2"),
            MemOperandScale::Four => write!(f, "4"),
            MemOperandScale::Eight => write!(f, "8"),
        }
    }
}

impl std::fmt::Display for MemOperandDisplacement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemOperandDisplacement::Disp32(v) => write!(f, "{v}"),
            MemOperandDisplacement::Disp8(v) => write!(f, "{v}"),
            MemOperandDisplacement::Zero => write!(f, "zero")
        }
    }
}

impl std::fmt::Display for StackOrReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackOrReg::Stack(stack_slot_ref) => 
                write!(f, "&ss{}", stack_slot_ref.get_inner()),
            StackOrReg::Reg(reg) => 
                write!(f, "{reg}"),
        }
    }
}

impl std::fmt::Display for MemOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemOperand::PcRelativeFn { target } => 
                write!(f, "[&fn{}]", target.get_inner()),
            MemOperand::PcRelativeData { target } => 
                write!(f, "[&data{}]", target.get_inner()),
            MemOperand::Full { base, index, scale, disp } => 
                write!(f, "[{base}+{scale}*{index}+{disp}]"),
            MemOperand::BasePlusDisp { base, disp } => 
                write!(f, "[{base}+{disp}]"),
            MemOperand::AbsoluteDisp { disp } => 
                write!(f, "[{disp}]"),
        }
    }
}

impl MachineInst {
    fn mnemonic(&self) -> &'static str {
        match self {
            MachineInst::Lea { .. } => "Lea",
            MachineInst::AddRegToReg { .. } => "AddRegToReg",
            MachineInst::AddMemToReg { .. } => "AddMemToReg",
            MachineInst::AddRegToMem { .. } => "AddRegToMem",
            MachineInst::AddImmToReg { .. } => "AddImmToReg",
            MachineInst::AddImmToMem { .. } => "AddImmToMem",
            MachineInst::FAddRegToReg { .. } => "FAddRegToReg",
            MachineInst::FAddMemToReg { .. } => "FAddMemToReg",
            MachineInst::Load { .. } => "Load",
            MachineInst::LoadImm { .. } => "LoadImm",
            MachineInst::LoadFloat { .. } => "LoadFloat",
            MachineInst::StoreReg { .. } => "StoreReg",
            MachineInst::StoreImm { .. } => "StoreImm",
            MachineInst::StoreFloat { .. } => "StoreFloat",
            MachineInst::Mov { .. } => "Mov",
            MachineInst::Cmov { .. } => "Cmov",
            MachineInst::Xchg { .. } => "Xchg",
            MachineInst::ZeroExtend { .. } => "ZeroExtend",
            MachineInst::SignExtend { .. } => "SignExtend",
            MachineInst::Push { .. } => "Push",
            MachineInst::PushImm { .. } => "PushImm",
            MachineInst::Pop { .. } => "Pop",
            MachineInst::Jump { .. } => "Jump",
            MachineInst::Call { .. } => "Call",
            MachineInst::CallIndirect { .. } => "CallIndirect",
            MachineInst::Ret => "Ret",
            MachineInst::MulRegToReg { .. } => "MulRegToReg",
            MachineInst::MulMemToReg { .. } => "MulMemToReg",
            MachineInst::MulRegWithImm { .. } => "MulRegWithImm",
            MachineInst::MulMemWithImm { .. } => "MulMemWithImm",
            MachineInst::UDivByReg { .. } => "UDivByReg",
            MachineInst::UDivByMem { .. } => "UDivByMem",
            MachineInst::SDivByReg { .. } => "SDivByReg",
            MachineInst::SDivByMem { .. } => "SDivByMem",
            MachineInst::PrepareDiv { .. } => "PrepareDiv",
            MachineInst::AndRegToReg { .. } => "AndRegToReg",
            MachineInst::OrRegToReg { .. } => "OrRegToReg",
            MachineInst::XorRegToReg { .. } => "XorRegToReg",
            MachineInst::NotReg { .. } => "NotReg",
            MachineInst::TestRegWithImm { .. } => "TestRegWithImm",
            MachineInst::CmpRegWithImm { .. } => "CmpRegWithImm",
            MachineInst::JmpWithCond { .. } => "JmpWithCond",
        }
    }
}

impl std::fmt::Display for MachineInst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let m = self.mnemonic();
        match self {
            MachineInst::Lea { dst, op2, width } => 
                write!(f, "{m} {width} {dst}, {op2}"),
            MachineInst::AddRegToReg { dst, op1, op2, width } => 
                write!(f, "{m} {width} {dst}, {op1}, {op2}"),
            MachineInst::AddMemToReg { dst, op1, op2, width } => todo!(),
            MachineInst::AddRegToMem { op1, op2, width } => todo!(),
            MachineInst::AddImmToReg { dst, op1, op2, width } => todo!(),
            MachineInst::AddImmToMem { op1, op2, width } => todo!(),
            MachineInst::FAddRegToReg { dst, op1, op2, width } => todo!(),
            MachineInst::FAddMemToReg { dst, op1, op2, width } => todo!(),
            MachineInst::Load { dst, op2, width } => todo!(),
            MachineInst::LoadImm { dst, op2, width } => 
                write!(f, "{m} {width} {dst}, {op2}"),
            MachineInst::LoadFloat { dst, op2, width } => todo!(),
            MachineInst::StoreReg { op1, op2, width } => todo!(),
            MachineInst::StoreImm { op1, op2, width } => todo!(),
            MachineInst::StoreFloat { op1, op2, width } => todo!(),
            MachineInst::Mov { dst, op2, width } => 
                write!(f, "{m} {width} {dst}, {op2}"),
            MachineInst::Cmov { dst, op2, cond, width } => todo!(),
            MachineInst::Xchg { op1, op2, width } => todo!(),
            MachineInst::ZeroExtend { dst, op2, dst_width, op2_width } => todo!(),
            MachineInst::SignExtend { dst, op2, dst_width, op2_width } => todo!(),
            MachineInst::Push { op1, width } => todo!(),
            MachineInst::PushImm { op1, width } => todo!(),
            MachineInst::Pop { dst, width } => todo!(),
            MachineInst::Jump { target } => todo!(),
            MachineInst::Call { target } => todo!(),
            MachineInst::CallIndirect { target } => todo!(),
            MachineInst::Ret => write!(f, "{m}"),
            MachineInst::MulRegToReg { dst, op1, op2, width } => todo!(),
            MachineInst::MulMemToReg { dst, op1, op2, width } => todo!(),
            MachineInst::MulRegWithImm { dst, op1, op2, width } => todo!(),
            MachineInst::MulMemWithImm { dst, op1, op2, width } => todo!(),
            MachineInst::UDivByReg { dst_quo, dst_rem, op1, width } => todo!(),
            MachineInst::UDivByMem { dst_quo, dst_rem, op1, width } => todo!(),
            MachineInst::SDivByReg { dst_quo, dst_rem, op1, width } => todo!(),
            MachineInst::SDivByMem { dst_quo, dst_rem, op1, width } => todo!(),
            MachineInst::PrepareDiv { width } => todo!(),
            MachineInst::AndRegToReg { dst, op1, op2, width } => todo!(),
            MachineInst::OrRegToReg { dst, op1, op2, width } => todo!(),
            MachineInst::XorRegToReg { dst, op1, op2, width } => todo!(),
            MachineInst::NotReg { dst, op1, width } => todo!(),
            MachineInst::TestRegWithImm { op1, op2, width } => todo!(),
            MachineInst::CmpRegWithImm { op1, op2, width } => todo!(),
            MachineInst::JmpWithCond { cond, target } => todo!(),
        }
    }
}

impl std::fmt::Display for MachineFunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (bref, block) in MachineBlockRef::enumerate2(&self.blocks) {
            writeln!(f, "b{}:", bref.get_inner())?;
            
            for &iref in &block.irefs {
                let minst = &self.insts[iref];
                write!(f, "    ")?;
                write!(f, "{minst}")?;
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for MachineFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", &self.name)?;

        let Some(defn) = &self.definition else {
            return Ok(())
        };

        defn.fmt(f)
    }
}

impl std::fmt::Display for MachineModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "functions:")?;
        let mut idx = 0;
        for (func, sig) in std::iter::zip(
            self.functions.as_ref(), 
            self.signatures.as_ref()
        ) {
            write!(f, "f{idx}: {sig}, ")?;
            writeln!(f, "{func}")?;
            idx += 1;
        }

        Ok(())
    }
}

mod cir2mir;