//! MIR: a low-level, machine-level IR sitting between [`crate::cir`] and final
//! x86-64 encoding.
//!
//! Each [`MachineInst`] variant corresponds ~1-1 with a real x86 instruction
//! (a curated subset, not iced-x86's full opcode zoo). Instructions operate over
//! [`Reg`]s, which are either *virtual* ([`Vreg`]) or *physical* ([`Preg`]); the
//! enum shape is identical in both phases so a register allocator can rewrite in
//! place.
//!
//! Model (LLVM-style): instruction selection produces loosely-SSA MIR (block
//! params act as phis, like CIR's `BlockArgument`). Later passes take it out of
//! SSA (block-params -> copies), legalize the two-address ALU constraint
//! (force a tied `dst == lhs` by inserting a `mov`), then run register
//! allocation.

use cake_util::make_type_idx;

use crate::cir::{Data, SigRef, Signature, StackSlot, Type};

// ---------------------------------------------------------------------------
// Registers
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum RegClass {
    /// General-purpose integer registers. (Xmm/float later)
    Gpr,
}

/// A physical x86-64 register. Width-agnostic: the operating width (8/16/32/64)
/// comes from the instruction / the defining vreg's [`Type`], and the right
/// iced `Register` is picked at encode time.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Preg {
    rax,
    rcx,
    rdx,
    rbx,
    rsp,
    rbp,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
}

/// Registers clobbered by a `call` under the System V AMD64 ABI (caller-saved).
/// Surfaced as implicit defs of [`MachineInst::Call`] so the allocator knows
/// these values do not survive a call.
pub(crate) const CALLER_SAVED: [Preg; 9] = [
    Preg::rax,
    Preg::rcx,
    Preg::rdx,
    Preg::rsi,
    Preg::rdi,
    Preg::r8,
    Preg::r9,
    Preg::r10,
    Preg::r11,
];

// `Vreg` is both an index and a value: it indexes a side-table of [`VregData`]
// held on the [`MachineFunction`].
make_type_idx!(Vreg, VregData);

#[derive(Debug, Clone, Copy)]
pub(crate) struct VregData {
    pub(crate) class: RegClass,
    pub(crate) ty: Type,
}

/// A register operand, virtual before allocation and physical after. The enum
/// shape of [`MachineInst`] never changes; only the [`Reg`]s inside it do.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Reg {
    Virtual(Vreg),
    Physical(Preg),
}

// ---------------------------------------------------------------------------
// Operands
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Scale {
    S1,
    S2,
    S4,
    S8,
}

/// An x86 memory operand: `[base + index*scale + disp]`.
///
/// (Future: a RIP-relative + symbol form for globals/externs that lowers to an
/// ELF relocation.)
#[derive(Debug, Clone, Copy)]
pub(crate) struct Mem {
    pub(crate) base: Option<Reg>,
    pub(crate) index: Option<(Reg, Scale)>,
    pub(crate) disp: i32,
}

/// A general source/destination operand.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Operand {
    Reg(Reg),
    Imm(i64),
    Mem(Mem),
}

/// Operating width for the width-carrying instructions (e.g. `movzx`/`movsx`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Width {
    W8,
    W16,
    W32,
    W64,
}

/// Condition codes for `jcc` / `setcc`. CIR's `CompareMode` plus operand
/// signedness lowers into these (signed `L/Le/G/Ge` vs unsigned `B/Be/A/Ae`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Cond {
    E,
    Ne,
    L,
    Le,
    G,
    Ge,
    B,
    Be,
    A,
    Ae,
    S,
    Ns,
}

/// Target of a `call`: a named symbol (resolved to a relocation later) or an
/// indirect register/memory target.
#[derive(Debug, Clone)]
pub(crate) enum CallTarget {
    Symbol(String),
    Indirect(Operand),
}

// ---------------------------------------------------------------------------
// Instructions
// ---------------------------------------------------------------------------

/// One variant ~= one x86 mnemonic.
///
/// The ALU ops are written in three-field form (`dst`, `lhs`, `rhs`) so they are
/// usable in loose-SSA with a fresh `dst` distinct from `lhs`. The real x86
/// instruction is two-address: the two-address legalization pass forces
/// `dst == lhs` (inserting a `mov dst, lhs` when they differ) before encoding.
#[derive(Debug, Clone)]
pub(crate) enum MachineInst {
    /// `mov dst, src` — at most one operand may be memory.
    Mov {
        dst: Operand,
        src: Operand,
    },
    /// `lea dst, [addr]`.
    Lea {
        dst: Reg,
        addr: Mem,
    },

    // two-address ALU (dst tied to lhs at legalization)
    Add {
        dst: Reg,
        lhs: Reg,
        rhs: Operand,
    },
    Sub {
        dst: Reg,
        lhs: Reg,
        rhs: Operand,
    },
    And {
        dst: Reg,
        lhs: Reg,
        rhs: Operand,
    },
    Or {
        dst: Reg,
        lhs: Reg,
        rhs: Operand,
    },
    Xor {
        dst: Reg,
        lhs: Reg,
        rhs: Operand,
    },
    Imul {
        dst: Reg,
        lhs: Reg,
        rhs: Operand,
    },
    /// Shift; `amount` is an immediate or (implicitly) `cl`.
    Shl {
        dst: Reg,
        lhs: Reg,
        amount: Operand,
    },
    Shr {
        dst: Reg,
        lhs: Reg,
        amount: Operand,
    },
    Sar {
        dst: Reg,
        lhs: Reg,
        amount: Operand,
    },
    Neg {
        dst: Reg,
        src: Reg,
    },
    Not {
        dst: Reg,
        src: Reg,
    },

    /// `cqo`/`cdq`: sign-extend `rax` into `rdx:rax` ahead of `idiv`.
    Cqo,
    /// `idiv divisor`: dividend in `rdx:rax`, quotient -> `rax`, remainder -> `rdx`.
    Idiv {
        divisor: Operand,
    },

    Cmp {
        lhs: Reg,
        rhs: Operand,
    },
    Test {
        lhs: Reg,
        rhs: Operand,
    },
    /// `setcc dst` — writes the low byte of `dst`.
    Setcc {
        cond: Cond,
        dst: Reg,
    },
    /// Zero/sign-extend `src` (of width `src_width`) into `dst`.
    Movzx {
        dst: Reg,
        src: Operand,
        src_width: Width,
    },
    Movsx {
        dst: Reg,
        src: Operand,
        src_width: Width,
    },

    Push {
        src: Reg,
    },
    Pop {
        dst: Reg,
    },

    // terminators: always the last inst in a block. Branch args line up
    // positionally with the target block's `params` (loose-SSA).
    Jmp {
        target: MblockRef,
        args: Vec<Vreg>,
    },
    /// Conditional branch; the fall-through edge is the textually next block.
    Jcc {
        cond: Cond,
        target: MblockRef,
        args: Vec<Vreg>,
    },
    Call {
        target: CallTarget,
    },
    Ret,
}

/// Whether a register operand is read, written, or both.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OperandRole {
    Def,
    Use,
    /// Read-modify-write of a single physical register (e.g. `idiv`'s `rdx:rax`).
    DefUse,
}

impl MachineInst {
    /// Visit every register touched by this instruction, with its role —
    /// including *implicit* fixed-physical-register operands (`idiv`'s
    /// `rdx:rax`, `call`'s caller-saved clobbers, `ret`'s return reg). Used by
    /// liveness, clobber analysis, and the future register allocator.
    ///
    /// For mutation/rewriting use [`MachineInst::for_each_reg_mut`], which only
    /// visits the *explicit* (rewritable) operands.
    pub(crate) fn for_each_reg(&self, mut f: impl FnMut(Reg, OperandRole)) {
        // helper: a read operand contributes its register(s) as Use
        let use_operand = |op: &Operand, f: &mut dyn FnMut(Reg, OperandRole)| match op {
            Operand::Reg(r) => f(*r, OperandRole::Use),
            Operand::Mem(m) => mem_regs(m, f),
            Operand::Imm(_) => {}
        };

        match self {
            MachineInst::Mov { dst, src } => {
                use_operand(src, &mut f);
                match dst {
                    // a register destination is a pure def
                    Operand::Reg(r) => f(*r, OperandRole::Def),
                    // a memory destination only *reads* its address registers
                    Operand::Mem(m) => mem_regs(m, &mut f),
                    Operand::Imm(_) => {}
                }
            }
            MachineInst::Lea { dst, addr } => {
                mem_regs(addr, &mut f);
                f(*dst, OperandRole::Def);
            }
            MachineInst::Add { dst, lhs, rhs }
            | MachineInst::Sub { dst, lhs, rhs }
            | MachineInst::And { dst, lhs, rhs }
            | MachineInst::Or { dst, lhs, rhs }
            | MachineInst::Xor { dst, lhs, rhs }
            | MachineInst::Imul { dst, lhs, rhs }
            | MachineInst::Shl {
                dst,
                lhs,
                amount: rhs,
            }
            | MachineInst::Shr {
                dst,
                lhs,
                amount: rhs,
            }
            | MachineInst::Sar {
                dst,
                lhs,
                amount: rhs,
            } => {
                f(*lhs, OperandRole::Use);
                use_operand(rhs, &mut f);
                f(*dst, OperandRole::Def);
            }
            MachineInst::Neg { dst, src } | MachineInst::Not { dst, src } => {
                f(*src, OperandRole::Use);
                f(*dst, OperandRole::Def);
            }
            MachineInst::Cqo => {
                f(Reg::Physical(Preg::rax), OperandRole::Use);
                f(Reg::Physical(Preg::rdx), OperandRole::Def);
            }
            MachineInst::Idiv { divisor } => {
                use_operand(divisor, &mut f);
                f(Reg::Physical(Preg::rax), OperandRole::DefUse);
                f(Reg::Physical(Preg::rdx), OperandRole::DefUse);
            }
            MachineInst::Cmp { lhs, rhs } | MachineInst::Test { lhs, rhs } => {
                f(*lhs, OperandRole::Use);
                use_operand(rhs, &mut f);
            }
            MachineInst::Setcc { dst, .. } => f(*dst, OperandRole::Def),
            MachineInst::Movzx { dst, src, .. } | MachineInst::Movsx { dst, src, .. } => {
                use_operand(src, &mut f);
                f(*dst, OperandRole::Def);
            }
            MachineInst::Push { src } => f(*src, OperandRole::Use),
            MachineInst::Pop { dst } => f(*dst, OperandRole::Def),
            MachineInst::Jmp { args, .. } | MachineInst::Jcc { args, .. } => {
                for v in args {
                    f(Reg::Virtual(*v), OperandRole::Use);
                }
            }
            MachineInst::Call { target } => {
                if let CallTarget::Indirect(op) = target {
                    use_operand(op, &mut f);
                }
                // caller-saved registers are clobbered across the call
                for preg in CALLER_SAVED {
                    f(Reg::Physical(preg), OperandRole::Def);
                }
            }
            // The return value reg (rax / xmm0) is set up by the preceding mov;
            // ret itself reads it. Modeled as a use of rax for now.
            MachineInst::Ret => f(Reg::Physical(Preg::rax), OperandRole::Use),
        }
    }

    /// Visit every *explicit* register operand mutably — the operands a register
    /// allocator rewrites from virtual to physical. Implicit fixed-preg
    /// constraints are already physical and are intentionally NOT yielded here.
    pub(crate) fn for_each_reg_mut(&mut self, mut f: impl FnMut(&mut Reg)) {
        fn operand_regs(op: &mut Operand, f: &mut dyn FnMut(&mut Reg)) {
            match op {
                Operand::Reg(r) => f(r),
                Operand::Mem(m) => mem_regs_mut(m, f),
                Operand::Imm(_) => {}
            }
        }

        match self {
            MachineInst::Mov { dst, src } => {
                operand_regs(dst, &mut f);
                operand_regs(src, &mut f);
            }
            MachineInst::Lea { dst, addr } => {
                f(dst);
                mem_regs_mut(addr, &mut f);
            }
            MachineInst::Add { dst, lhs, rhs }
            | MachineInst::Sub { dst, lhs, rhs }
            | MachineInst::And { dst, lhs, rhs }
            | MachineInst::Or { dst, lhs, rhs }
            | MachineInst::Xor { dst, lhs, rhs }
            | MachineInst::Imul { dst, lhs, rhs }
            | MachineInst::Shl {
                dst,
                lhs,
                amount: rhs,
            }
            | MachineInst::Shr {
                dst,
                lhs,
                amount: rhs,
            }
            | MachineInst::Sar {
                dst,
                lhs,
                amount: rhs,
            } => {
                f(dst);
                f(lhs);
                operand_regs(rhs, &mut f);
            }
            MachineInst::Neg { dst, src } | MachineInst::Not { dst, src } => {
                f(dst);
                f(src);
            }
            MachineInst::Cqo => {}
            MachineInst::Idiv { divisor } => operand_regs(divisor, &mut f),
            MachineInst::Cmp { lhs, rhs } | MachineInst::Test { lhs, rhs } => {
                f(lhs);
                operand_regs(rhs, &mut f);
            }
            MachineInst::Setcc { dst, .. } => f(dst),
            MachineInst::Movzx { dst, src, .. } | MachineInst::Movsx { dst, src, .. } => {
                f(dst);
                operand_regs(src, &mut f);
            }
            MachineInst::Push { src } => f(src),
            MachineInst::Pop { dst } => f(dst),
            MachineInst::Jmp { args, .. } | MachineInst::Jcc { args, .. } => {
                for v in args {
                    // args are vregs; wrap/unwrap through Reg for uniform rewrite
                    let mut r = Reg::Virtual(*v);
                    f(&mut r);
                    if let Reg::Virtual(nv) = r {
                        *v = nv;
                    }
                }
            }
            MachineInst::Call { target } => {
                if let CallTarget::Indirect(op) = target {
                    operand_regs(op, &mut f);
                }
            }
            MachineInst::Ret => {}
        }
    }

    /// Successor blocks of this terminator (empty for non-terminators / `ret`).
    pub(crate) fn successors(&self) -> Option<MblockRef> {
        match self {
            MachineInst::Jmp { target, .. } => Some(*target),
            MachineInst::Jcc { target, .. } => Some(*target),
            _ => None,
        }
    }
}

fn mem_regs(m: &Mem, f: &mut dyn FnMut(Reg, OperandRole)) {
    if let Some(base) = m.base {
        f(base, OperandRole::Use);
    }
    if let Some((index, _)) = m.index {
        f(index, OperandRole::Use);
    }
}

fn mem_regs_mut(m: &mut Mem, f: &mut dyn FnMut(&mut Reg)) {
    if let Some(base) = m.base.as_mut() {
        f(base);
    }
    if let Some((index, _)) = m.index.as_mut() {
        f(index);
    }
}

// ---------------------------------------------------------------------------
// CFG containers
// ---------------------------------------------------------------------------

make_type_idx!(MblockRef, Mblock);

#[derive(Debug)]
pub(crate) struct Mblock {
    /// The terminator is the last instruction.
    pub(crate) insts: Vec<MachineInst>,
}

impl Mblock {
    fn new() -> Mblock {
        Mblock { insts: Vec::new() }
    }
}

make_type_idx!(MFuncRef, MachineFunction);

#[derive(Debug)]
pub(crate) struct MachineFunction {
    pub(crate) signature: SigRef,
    pub(crate) blocks: Vec<Mblock>,
    pub(crate) vregs: Vec<VregData>,
    pub(crate) stack_slots: Vec<StackSlot>,
}

impl MachineFunction {
    pub(crate) fn new_vreg(&mut self, class: RegClass, ty: Type) -> Vreg {
        Vreg::from_push(&mut self.vregs, VregData { class, ty })
    }
}

mod cir2mir;

#[cfg(test)]
mod test {
    use super::*;
}
