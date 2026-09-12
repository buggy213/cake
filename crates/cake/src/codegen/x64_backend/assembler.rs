use std::assert_matches;
use cake_util::{IndexSlice, IndexVec};
use iced_x86::{
    IcedError, Instruction, MemoryOperand, Register
};

use crate::{
    cir,
    mir::{
        Condition, GprOperandWidth, MachineBlockRef, MachineFunctionRef, MachineInst, MemOperand,
        MemOperandDisplacement, PhysReg, Reg, SseOperandWidth
    }
};

use valid::ValidForCodegenToken;

/// This ZST asserts that that certain invariants which are required to actually emit correct code
/// are upheld. It can be produced by a verifier in debug builds, or `unsafe`-ly created in release builds
/// - All registers have been replaced with physical registers
/// - No block arguments (i.e. phis) persist in the final set of instructions
/// - Registers specified as operands are of the correct register class and widths for an instruction
mod valid {
    use crate::mir::{PhysReg, Reg};

    #[derive(Clone, Copy)]
    pub(crate) struct ValidForCodegenToken {
        _private: ()
    }

    impl ValidForCodegenToken {
        /// # Safety
        /// The caller must ensure the invariants documented on `ValidForCodegenToken` actually
        /// hold for whatever `MachineInst`s will be assembled using this token.
        pub(crate) unsafe fn assume_valid() -> Self {
            Self { _private: () }
        }
    }

    // helper functions to extract physical registers from the MIR function, relying on the invariants above
    impl Reg {
        pub(super) fn as_preg(self, valid: ValidForCodegenToken) -> PhysReg {
            match self {
                Reg::VReg(virtual_reg) => unreachable!(),
                Reg::PReg(phys_reg) => phys_reg,
            }
        }

        pub(super) fn tied_pregs(a: Reg, b: Reg, valid: ValidForCodegenToken) -> PhysReg {
            match (a, b) {
                (Reg::PReg(a), Reg::PReg(b)) if a == b => a,
                _ => unreachable!()
            }
        }
    }
}

impl PhysReg {
    /// Converts MIR (width-agnostic) physical register to iced_x86 physical register. 
    /// Panics if `self` is not a GPR.
    fn to_register(self, width: GprOperandWidth) -> Register {
        use Register::*;

        match width {
            GprOperandWidth::Byte => match self {
                PhysReg::rax => AL,
                PhysReg::rbx => BL,
                PhysReg::rcx => CL,
                PhysReg::rdx => DL,
                PhysReg::rsi => SIL,
                PhysReg::rdi => DIL,
                PhysReg::rbp => BPL,
                PhysReg::rsp => SPL,
                PhysReg::r8 => R8L,
                PhysReg::r9 => R9L,
                PhysReg::r10 => R10L,
                PhysReg::r11 => R11L,
                PhysReg::r12 => R12L,
                PhysReg::r13 => R13L,
                PhysReg::r14 => R14L,
                PhysReg::r15 => R15L,
                _ => panic!("not a gpr"),
            },
            GprOperandWidth::Word => match self {
                PhysReg::rax => AX,
                PhysReg::rbx => BX,
                PhysReg::rcx => CX,
                PhysReg::rdx => DX,
                PhysReg::rsi => SI,
                PhysReg::rdi => DI,
                PhysReg::rbp => BP,
                PhysReg::rsp => SP,
                PhysReg::r8 => R8W,
                PhysReg::r9 => R9W,
                PhysReg::r10 => R10W,
                PhysReg::r11 => R11W,
                PhysReg::r12 => R12W,
                PhysReg::r13 => R13W,
                PhysReg::r14 => R14W,
                PhysReg::r15 => R15W,
                _ => panic!("not a gpr"),
            },
            GprOperandWidth::Dword => match self {
                PhysReg::rax => EAX,
                PhysReg::rbx => EBX,
                PhysReg::rcx => ECX,
                PhysReg::rdx => EDX,
                PhysReg::rsi => ESI,
                PhysReg::rdi => EDI,
                PhysReg::rbp => EBP,
                PhysReg::rsp => ESP,
                PhysReg::r8 => R8D,
                PhysReg::r9 => R9D,
                PhysReg::r10 => R10D,
                PhysReg::r11 => R11D,
                PhysReg::r12 => R12D,
                PhysReg::r13 => R13D,
                PhysReg::r14 => R14D,
                PhysReg::r15 => R15D,
                _ => panic!("not a gpr"),
            },
            GprOperandWidth::Qword => match self {
                PhysReg::rax => RAX,
                PhysReg::rbx => RBX,
                PhysReg::rcx => RCX,
                PhysReg::rdx => RDX,
                PhysReg::rsi => RSI,
                PhysReg::rdi => RDI,
                PhysReg::rbp => RBP,
                PhysReg::rsp => RSP,
                PhysReg::r8 => R8,
                PhysReg::r9 => R9,
                PhysReg::r10 => R10,
                PhysReg::r11 => R11,
                PhysReg::r12 => R12,
                PhysReg::r13 => R13,
                PhysReg::r14 => R14,
                PhysReg::r15 => R15,
                _ => panic!("not a gpr"),
            },
        }
    }
    
    /// Maps an xmm `PhysReg` to its iced_x86 register; xmm registers have one name
    /// regardless of scalar width.
    /// Panics if `self` is not an xmm register.
    fn to_sse_register(self) -> Register {
        use Register::*;

        match self {
            PhysReg::xmm0 => XMM0,
            PhysReg::xmm1 => XMM1,
            PhysReg::xmm2 => XMM2,
            PhysReg::xmm3 => XMM3,
            PhysReg::xmm4 => XMM4,
            PhysReg::xmm5 => XMM5,
            PhysReg::xmm6 => XMM6,
            PhysReg::xmm7 => XMM7,
            PhysReg::xmm8 => XMM8,
            PhysReg::xmm9 => XMM9,
            PhysReg::xmm10 => XMM10,
            PhysReg::xmm11 => XMM11,
            PhysReg::xmm12 => XMM12,
            PhysReg::xmm13 => XMM13,
            PhysReg::xmm14 => XMM14,
            PhysReg::xmm15 => XMM15,
            _ => panic!("not an xmm register"),
        }
    }
}

impl MemOperand {
    fn as_memory_operand(self, valid: ValidForCodegenToken) -> MemoryOperand {
        let displ_size_from_displacement = |w: MemOperandDisplacement| match w {
            MemOperandDisplacement::Disp32(_) => 4,
            MemOperandDisplacement::Disp8(_) => 1,
            MemOperandDisplacement::Zero => 0
        };

        match self {
            MemOperand::PcRelative { disp } => {
                // have to widen to disp32 for this addressing mode
                let disp = disp.as_u32();

                MemoryOperand::with_base_displ_size(Register::RIP, disp as i64, 4)
            },
            MemOperand::Full { base, index, scale, disp } => {
                let base = base.as_preg(valid);
                let index = index.as_preg(valid);

                // memory operand base/index registers are always full-width
                let base: Register = base.to_register(GprOperandWidth::Qword);
                let index: Register = index.to_register(GprOperandWidth::Qword);
                let scale: u32 = scale.as_u32();
                let displ_size = displ_size_from_displacement(disp);
                let disp: u32 = disp.as_u32();

                MemoryOperand::with_base_index_scale_displ_size(base, index, scale, disp as i64, displ_size)
            },
            MemOperand::BasePlusDisp { base, disp } => {
                let base = base.as_preg(valid);

                let base: Register = base.to_register(GprOperandWidth::Qword);
                let displ_size = displ_size_from_displacement(disp);
                let disp = disp.as_u32();

                MemoryOperand::with_base_displ_size(base, disp as i64, displ_size)
            },
            MemOperand::AbsoluteDisp { disp } => {
                // have to widen to disp32 for this particular addressing mode
                let disp = disp.as_u32();

                MemoryOperand::with_displ(disp as u64, 4)
            },
        }
    }
}

enum MachineRelocation {
    FnReloc {
        code_offset: u64,
        target: MachineFunctionRef,
    },

    DataReloc {
        code_offset: u64,
        target: cir::DataRef,
    }
}

#[derive(Clone, Copy)]
struct MachineLabel(u64);

struct Assembler {
    encoder: iced_x86::Encoder,
    current_offset: usize,
    relocs: Vec<MachineRelocation>,
}

/// Per-function context for assembling
struct AssembleFunctionContext {
    labels: IndexVec<MachineBlockRef, MachineLabel>,
}

impl AssembleFunctionContext {
    fn reset(&mut self) {
        self.labels.clear();
    }
}

impl Assembler {
    /// Encodes `instruction` at the current offset and advances it by the encoded length.
    fn emit(&mut self, instruction: Instruction) -> Result<(), IcedError> {
        self.current_offset += self.encoder.encode(&instruction, self.current_offset as u64)?;
        Ok(())
    }

    /// Assembles a single MIR instruction into `self`.
    ///
    /// `block_labels` must contain the resolved byte offset (within the function) for every
    /// `MachineBlockRef` that may be targeted by a jump; it is the caller's responsibility to
    /// compute those offsets (e.g. via a prior sizing pass) before calling this.
    fn assemble_inst(
        &mut self,
        mir_inst: MachineInst,
        fn_ctx: &AssembleFunctionContext,
        valid: ValidForCodegenToken,
    ) -> Result<(), IcedError> {
        use iced_x86::Code;
        
        let block_labels = fn_ctx.labels.as_ref();

        match mir_inst {
            MachineInst::Lea { dst, op2, width } => {
                let dst = dst.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => panic!("lea with byte-sized dst not encodable"),
                    GprOperandWidth::Word => iced_x86::Code::Lea_r16_m,
                    GprOperandWidth::Dword => iced_x86::Code::Lea_r32_m,
                    GprOperandWidth::Qword => iced_x86::Code::Lea_r64_m,
                };

                let dst: Register = dst.to_register(width);
                let mem: MemoryOperand = op2.as_memory_operand(valid);
                self.emit(Instruction::with2(code, dst, mem)?)
            },
            MachineInst::AddRegToReg { dst, op1, op2, width } => {
                let dst_op1 = Reg::tied_pregs(dst, op1, valid);
                let op2 = op2.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => iced_x86::Code::Add_rm8_r8,
                    GprOperandWidth::Word => iced_x86::Code::Add_rm16_r16,
                    GprOperandWidth::Dword => iced_x86::Code::Add_rm32_r32,
                    GprOperandWidth::Qword => iced_x86::Code::Add_rm64_r64,
                };

                let dst_op1: Register = dst_op1.to_register(width);
                let addend: Register = op2.to_register(width);
                self.emit(Instruction::with2(code, dst_op1, addend)?)
            },
            MachineInst::AddMemToReg { dst, op1, op2, width } => {
                let dst_op1 = Reg::tied_pregs(dst, op1, valid);

                let code = match width {
                    GprOperandWidth::Byte => iced_x86::Code::Add_r8_rm8,
                    GprOperandWidth::Word => iced_x86::Code::Add_r16_rm16,
                    GprOperandWidth::Dword => iced_x86::Code::Add_r32_rm32,
                    GprOperandWidth::Qword => iced_x86::Code::Add_r64_rm64,
                };

                let dst_op1: Register = dst_op1.to_register(width);
                let addend: MemoryOperand = op2.as_memory_operand(valid);
                self.emit(Instruction::with2(code, dst_op1, addend)?)
            },
            MachineInst::AddRegToMem { op1, op2, width } => {
                let op2 = op2.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Add_rm8_r8,
                    GprOperandWidth::Word => Code::Add_rm16_r16,
                    GprOperandWidth::Dword => Code::Add_rm32_r32,
                    GprOperandWidth::Qword => Code::Add_rm64_r64,
                };

                let mem: MemoryOperand = op1.as_memory_operand(valid);
                let addend: Register = op2.to_register(width);
                self.emit(Instruction::with2(code, mem, addend)?)
            },
            MachineInst::AddImmToReg { dst, op1, op2, width } => {
                let dst_op1 = Reg::tied_pregs(dst, op1, valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Add_rm8_imm8,
                    GprOperandWidth::Word => Code::Add_rm16_imm16,
                    GprOperandWidth::Dword => Code::Add_rm32_imm32,
                    GprOperandWidth::Qword => Code::Add_rm64_imm32,
                };

                let dst_op1: Register = dst_op1.to_register(width);
                self.emit(Instruction::with2(code, dst_op1, op2.0 as i32)?)
            },
            MachineInst::AddImmToMem { op1, op2, width } => {
                let code = match width {
                    GprOperandWidth::Byte => Code::Add_rm8_imm8,
                    GprOperandWidth::Word => Code::Add_rm16_imm16,
                    GprOperandWidth::Dword => Code::Add_rm32_imm32,
                    GprOperandWidth::Qword => Code::Add_rm64_imm32,
                };

                let mem: MemoryOperand = op1.as_memory_operand(valid);
                self.emit(Instruction::with2(code, mem, op2.0 as i32)?)
            },
            MachineInst::FAddRegToReg { dst, op1, op2, width } => {
                let dst_op1 = Reg::tied_pregs(dst, op1, valid);
                let op2 = op2.as_preg(valid);
                
                let code = match width {
                    SseOperandWidth::Single => Code::VEX_Vaddss_xmm_xmm_xmmm32,
                    SseOperandWidth::Double => Code::VEX_Vaddsd_xmm_xmm_xmmm64,
                };

                let dst_op1: Register = dst_op1.to_sse_register();
                let op2: Register = op2.to_sse_register();
                self.emit(Instruction::with2(code, dst_op1, op2)?)
            },
            MachineInst::FAddMemToReg { dst, op1, op2, width } => {
                let code = match width {
                    SseOperandWidth::Single => Code::VEX_Vaddss_xmm_xmm_xmmm32,
                    SseOperandWidth::Double => Code::VEX_Vaddsd_xmm_xmm_xmmm64,
                };

                let dst: Register = dst.as_preg(valid).to_sse_register();
                let op1: Register = op1.as_preg(valid).to_sse_register();
                let op2: Register = op2.as_preg(valid).to_sse_register();

                self.emit(Instruction::with3(code, dst, op1, op2)?)
            }
            MachineInst::Load { dst, op2, width } => {
                let dst = dst.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Mov_r8_rm8,
                    GprOperandWidth::Word => Code::Mov_r16_rm16,
                    GprOperandWidth::Dword => Code::Mov_r32_rm32,
                    GprOperandWidth::Qword => Code::Mov_r64_rm64,
                };

                let dst: Register = dst.to_register(width);
                let src: MemoryOperand = op2.as_memory_operand(valid);
                self.emit(Instruction::with2(code, dst, src)?)
            },
            MachineInst::LoadImm { dst, op2, width } => {
                let dst = dst.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Mov_r8_imm8,
                    GprOperandWidth::Word => Code::Mov_r16_imm16,
                    GprOperandWidth::Dword => Code::Mov_r32_imm32,
                    GprOperandWidth::Qword => Code::Mov_r64_imm64,
                };

                let dst: Register = dst.to_register(width);
                self.emit(Instruction::with2(code, dst, op2.0 as u64)?)
            },
            MachineInst::LoadFloat { dst, op2, width } => {
                let dst = dst.as_preg(valid);

                let code = match width {
                    SseOperandWidth::Single => Code::VEX_Vmovss_xmm_m32,
                    SseOperandWidth::Double => Code::VEX_Vmovsd_xmm_m64,
                };

                let dst: Register = dst.to_sse_register();
                let src: MemoryOperand = op2.as_memory_operand(valid);
                self.emit(Instruction::with2(code, dst, src)?)
            },
            MachineInst::StoreReg { op1, op2, width } => {
                let op2 = op2.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Mov_rm8_r8,
                    GprOperandWidth::Word => Code::Mov_rm16_r16,
                    GprOperandWidth::Dword => Code::Mov_rm32_r32,
                    GprOperandWidth::Qword => Code::Mov_rm64_r64,
                };

                let mem: MemoryOperand = op1.as_memory_operand(valid);
                let op2: Register = op2.to_register(width);
                self.emit(Instruction::with2(code, mem, op2)?)
            },
            MachineInst::StoreImm { op1, op2, width } => {
                let code = match width {
                    GprOperandWidth::Byte => Code::Mov_rm8_imm8,
                    GprOperandWidth::Word => Code::Mov_rm16_imm16,
                    GprOperandWidth::Dword => Code::Mov_rm32_imm32,
                    GprOperandWidth::Qword => Code::Mov_rm64_imm32,
                };

                let mem: MemoryOperand = op1.as_memory_operand(valid);
                self.emit(Instruction::with2(code, mem, op2.0 as i32)?)
            },
            MachineInst::StoreFloat { op1, op2, width } => {
                let src = op2.as_preg(valid);

                let code = match width {
                    SseOperandWidth::Single => Code::VEX_Vmovss_m32_xmm,
                    SseOperandWidth::Double => Code::VEX_Vmovsd_m64_xmm,
                };

                let dst: MemoryOperand = op1.as_memory_operand(valid);
                let src: Register = src.to_sse_register();
                self.emit(Instruction::with2(code, dst, src)?)
            }
            MachineInst::Mov { dst, op2, width } => {
                let dst = dst.as_preg(valid);
                let op2 = op2.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Mov_rm8_r8,
                    GprOperandWidth::Word => Code::Mov_rm16_r16,
                    GprOperandWidth::Dword => Code::Mov_rm32_r32,
                    GprOperandWidth::Qword => Code::Mov_rm64_r64,
                };

                let dst: Register = dst.to_register(width);
                let src: Register = op2.to_register(width);
                self.emit(Instruction::with2(code, dst, src)?)
            },
            MachineInst::MovImm { dst, op2, width } => {
                let dst = dst.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Mov_r8_imm8,
                    GprOperandWidth::Word => Code::Mov_r16_imm16,
                    GprOperandWidth::Dword => Code::Mov_r32_imm32,
                    GprOperandWidth::Qword => Code::Mov_r64_imm64,
                };

                let dst: Register = dst.to_register(width);
                let imm: u64 = op2.0 as u64;
                self.emit(Instruction::with2(code, dst, imm)?)
            },
            MachineInst::Xchg { op1, op2, width } => {
                let op1 = op1.as_preg(valid);
                let op2 = op2.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Xchg_rm8_r8,
                    GprOperandWidth::Word => Code::Xchg_rm16_r16,
                    GprOperandWidth::Dword => Code::Xchg_rm32_r32,
                    GprOperandWidth::Qword => Code::Xchg_rm64_r64,
                };

                let op1: Register = op1.to_register(width);
                let op2: Register = op2.to_register(width);
                self.emit(Instruction::with2(code, op1, op2)?)
            }

            MachineInst::ZeroExtend { dst, op2, dst_width, op2_width } => {
                let dst = dst.as_preg(valid);
                let op2 = op2.as_preg(valid);

                let code = match (dst_width, op2_width) {
                    (GprOperandWidth::Word, GprOperandWidth::Byte) => Code::Movzx_r16_rm8,
                    (GprOperandWidth::Dword, GprOperandWidth::Byte) => Code::Movzx_r32_rm8,
                    (GprOperandWidth::Qword, GprOperandWidth::Byte) => Code::Movzx_r64_rm8,
                    (GprOperandWidth::Dword, GprOperandWidth::Word) => Code::Movzx_r32_rm16,
                    (GprOperandWidth::Qword, GprOperandWidth::Word) => Code::Movzx_r64_rm16,
                    _ => panic!("invalid zero-extend widths"),
                };

                let dst: Register = dst.to_register(dst_width);
                let op2: Register = op2.to_register(op2_width);
                self.emit(Instruction::with2(code, dst, op2)?)
            },
            MachineInst::SignExtend { dst, op2, dst_width, op2_width } => {
                let dst = dst.as_preg(valid);
                let op2 = op2.as_preg(valid);

                let code = match (dst_width, op2_width) {
                    (GprOperandWidth::Word, GprOperandWidth::Byte) => Code::Movsx_r16_rm8,
                    (GprOperandWidth::Dword, GprOperandWidth::Byte) => Code::Movsx_r32_rm8,
                    (GprOperandWidth::Qword, GprOperandWidth::Byte) => Code::Movsx_r64_rm8,
                    (GprOperandWidth::Dword, GprOperandWidth::Word) => Code::Movsx_r32_rm16,
                    (GprOperandWidth::Qword, GprOperandWidth::Word) => Code::Movsx_r64_rm16,
                    (GprOperandWidth::Qword, GprOperandWidth::Dword) => Code::Movsxd_r64_rm32,
                    _ => panic!("invalid sign-extend widths"),
                };

                let dst: Register = dst.to_register(dst_width);
                let op2: Register = op2.to_register(op2_width);
                self.emit(Instruction::with2(code, dst, op2)?)
            },
            MachineInst::Push { op1, width } => {
                assert!(width != GprOperandWidth::Byte && width != GprOperandWidth::Dword);
                let op1 = op1.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Word => Code::Push_r16,
                    GprOperandWidth::Qword => Code::Push_r64,
                    _ => unreachable!(),
                };

                let op1: Register = op1.to_register(width);
                self.emit(Instruction::with1(code, op1)?)
            },
            MachineInst::PushImm { op1, width } => {
                let code = match width {
                    GprOperandWidth::Byte => panic!("push imm8 not encodable"),
                    GprOperandWidth::Word => Code::Push_imm16,
                    GprOperandWidth::Dword | GprOperandWidth::Qword => Code::Pushq_imm32,
                };

                self.emit(Instruction::with1(code, op1.0 as i32)?)
            },
            MachineInst::Pop { dst, width } => {
                assert!(width != GprOperandWidth::Byte && width != GprOperandWidth::Dword);
                let dst = dst.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Word => Code::Pop_r16,
                    GprOperandWidth::Qword => Code::Pop_r64,
                    _ => unreachable!(),
                };

                let dst: Register = dst.to_register(width);
                self.emit(Instruction::with1(code, dst)?)
            },
            MachineInst::Jump { target } => {
                let target = block_labels[target];
                self.emit(Instruction::with_branch(Code::Jmp_rel32_64, target.0)?)
            },
            MachineInst::Call { target } => {
                // the target's final address isn't known yet (it may not have been assembled,
                // and even if it has, we don't know its final address), so encode with a
                // placeholder and record where the rel32 needs to be patched later.
                let code_offset = self.current_offset as u64;
                let len = self.encoder.encode(&Instruction::with_branch(Code::Call_rel32_64, 0)?, code_offset)?;
                let offsets = self.encoder.get_constant_offsets();
                self.relocs.push(MachineRelocation::FnReloc {
                    code_offset: code_offset + offsets.immediate_offset() as u64,
                    target,
                });
                self.current_offset += len;

                Ok(())
            },
            MachineInst::CallIndirect { target } => {
                let target = target.as_preg(valid);

                // calls always go through the full-width register, regardless of the caller's declared width
                let target: Register = target.to_register(GprOperandWidth::Qword);
                self.emit(Instruction::with1(Code::Call_rm64, target)?)
            },
            MachineInst::Ret => {
                self.emit(Instruction::with(Code::Retnq))
            },
            MachineInst::MulRegToReg { dst, op1, op2, width } => {
                let op1 = Reg::tied_pregs(dst, op1, valid);
                let op2 = op2.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => panic!("two-operand imul with byte-sized operands not encodable"),
                    GprOperandWidth::Word => Code::Imul_r16_rm16,
                    GprOperandWidth::Dword => Code::Imul_r32_rm32,
                    GprOperandWidth::Qword => Code::Imul_r64_rm64,
                };

                let op1: Register = op1.to_register(width);
                let op2: Register = op2.to_register(width);
                self.emit(Instruction::with2(code, op1, op2)?)
            },
            MachineInst::MulMemToReg { dst, op1, op2, width } => {
                let dst = Reg::tied_pregs(dst, op1, valid);

                let code = match width {
                    GprOperandWidth::Byte => panic!("two-operand imul with byte-sized operands not encodable"),
                    GprOperandWidth::Word => Code::Imul_r16_rm16,
                    GprOperandWidth::Dword => Code::Imul_r32_rm32,
                    GprOperandWidth::Qword => Code::Imul_r64_rm64,
                };

                let dst: Register = dst.to_register(width);
                let op2: MemoryOperand = op2.as_memory_operand(valid);
                self.emit(Instruction::with2(code, dst, op2)?)
            },
            MachineInst::MulRegWithImm { dst, op1, op2, width } => {
                let dst = dst.as_preg(valid);
                let op1 = op1.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => panic!("imul with byte-sized operands not encodable"),
                    GprOperandWidth::Word => Code::Imul_r16_rm16_imm16,
                    GprOperandWidth::Dword => Code::Imul_r32_rm32_imm32,
                    GprOperandWidth::Qword => Code::Imul_r64_rm64_imm32,
                };

                let dst: Register = dst.to_register(width);
                let op1: Register = op1.to_register(width);
                self.emit(Instruction::with3(code, dst, op1, op2.0 as i32)?)
            },
            MachineInst::MulMemWithImm { dst, op1, op2, width } => {
                let dst = dst.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => panic!("imul with byte-sized operands not encodable"),
                    GprOperandWidth::Word => Code::Imul_r16_rm16_imm16,
                    GprOperandWidth::Dword => Code::Imul_r32_rm32_imm32,
                    GprOperandWidth::Qword => Code::Imul_r64_rm64_imm32,
                };

                let dst: Register = dst.to_register(width);
                let op1: MemoryOperand = op1.as_memory_operand(valid);
                self.emit(Instruction::with3(code, dst, op1, op2.0 as i32)?)
            },
            MachineInst::UDivByReg { dst_quo, dst_rem, op1, width } => {
                let dst_quo = dst_quo.as_preg(valid);
                let dst_rem = dst_rem.as_preg(valid);
                let op1 = op1.as_preg(valid);
                assert_matches!(dst_quo, PhysReg::rax);
                assert_matches!(dst_rem, PhysReg::rdx);

                let code = match width {
                    GprOperandWidth::Byte => Code::Div_rm8,
                    GprOperandWidth::Word => Code::Div_rm16,
                    GprOperandWidth::Dword => Code::Div_rm32,
                    GprOperandWidth::Qword => Code::Div_rm64,
                };

                let op1: Register = op1.to_register(width);
                self.emit(Instruction::with1(code, op1)?)
            },
            MachineInst::UDivByMem { dst_quo, dst_rem, op1, width } => {
                let dst_quo = dst_quo.as_preg(valid);
                let dst_rem = dst_rem.as_preg(valid);
                assert_matches!(dst_quo, PhysReg::rax);
                assert_matches!(dst_rem, PhysReg::rdx);

                let code = match width {
                    GprOperandWidth::Byte => Code::Div_rm8,
                    GprOperandWidth::Word => Code::Div_rm16,
                    GprOperandWidth::Dword => Code::Div_rm32,
                    GprOperandWidth::Qword => Code::Div_rm64,
                };

                let op1: MemoryOperand = op1.as_memory_operand(valid);
                self.emit(Instruction::with1(code, op1)?)
            },
            MachineInst::SDivByReg { dst_quo, dst_rem, op1, width } => {
                let dst_quo = dst_quo.as_preg(valid);
                let dst_rem = dst_rem.as_preg(valid);
                let op1 = op1.as_preg(valid);
                assert_matches!(dst_quo, PhysReg::rax);
                assert_matches!(dst_rem, PhysReg::rdx);

                let code = match width {
                    GprOperandWidth::Byte => Code::Idiv_rm8,
                    GprOperandWidth::Word => Code::Idiv_rm16,
                    GprOperandWidth::Dword => Code::Idiv_rm32,
                    GprOperandWidth::Qword => Code::Idiv_rm64,
                };

                let op1: Register = op1.to_register(width);
                self.emit(Instruction::with1(code, op1)?)
            },
            MachineInst::SDivByMem { dst_quo, dst_rem, op1, width } => {
                let dst_quo = dst_quo.as_preg(valid);
                let dst_rem = dst_rem.as_preg(valid);
                assert_matches!(dst_quo, PhysReg::rax);
                assert_matches!(dst_rem, PhysReg::rdx);

                let code = match width {
                    GprOperandWidth::Byte => Code::Idiv_rm8,
                    GprOperandWidth::Word => Code::Idiv_rm16,
                    GprOperandWidth::Dword => Code::Idiv_rm32,
                    GprOperandWidth::Qword => Code::Idiv_rm64,
                };

                let op1: MemoryOperand = op1.as_memory_operand(valid);
                self.emit(Instruction::with1(code, op1)?)
            },
            MachineInst::PrepareDiv { width } => {
                let code = match width {
                    GprOperandWidth::Byte => panic!("no sign/zero-extension instruction needed before byte-sized division"),
                    GprOperandWidth::Word => Code::Cwd,
                    GprOperandWidth::Dword => Code::Cdq,
                    GprOperandWidth::Qword => Code::Cqo,
                };

                self.emit(Instruction::with(code))
            },
            MachineInst::AndRegToReg { dst, op1, op2, width } => {
                let op1 = Reg::tied_pregs(dst, op1, valid);
                let op2 = op2.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::And_rm8_r8,
                    GprOperandWidth::Word => Code::And_rm16_r16,
                    GprOperandWidth::Dword => Code::And_rm32_r32,
                    GprOperandWidth::Qword => Code::And_rm64_r64,
                };

                let op1: Register = op1.to_register(width);
                let op2: Register = op2.to_register(width);
                self.emit(Instruction::with2(code, op1, op2)?)
            },
            MachineInst::OrRegToReg { dst, op1, op2, width } => {
                let op1 = Reg::tied_pregs(dst, op1, valid);
                let op2 = op2.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Or_rm8_r8,
                    GprOperandWidth::Word => Code::Or_rm16_r16,
                    GprOperandWidth::Dword => Code::Or_rm32_r32,
                    GprOperandWidth::Qword => Code::Or_rm64_r64,
                };

                let op1: Register = op1.to_register(width);
                let op2: Register = op2.to_register(width);
                self.emit(Instruction::with2(code, op1, op2)?)
            },
            MachineInst::XorRegToReg { dst, op1, op2, width } => {
                let op1 = Reg::tied_pregs(dst, op1, valid);
                let op2 = op2.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Xor_rm8_r8,
                    GprOperandWidth::Word => Code::Xor_rm16_r16,
                    GprOperandWidth::Dword => Code::Xor_rm32_r32,
                    GprOperandWidth::Qword => Code::Xor_rm64_r64,
                };

                let op1: Register = op1.to_register(width);
                let op2: Register = op2.to_register(width);
                self.emit(Instruction::with2(code, op1, op2)?)
            },
            MachineInst::NotReg { dst, op1, width } => {
                let dst = Reg::tied_pregs(dst, op1, valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Not_rm8,
                    GprOperandWidth::Word => Code::Not_rm16,
                    GprOperandWidth::Dword => Code::Not_rm32,
                    GprOperandWidth::Qword => Code::Not_rm64,
                };

                let dst: Register = dst.to_register(width);
                self.emit(Instruction::with1(code, dst)?)
            },
            MachineInst::TestRegWithImm { op1, op2, width } => {
                let op1 = op1.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Test_rm8_imm8,
                    GprOperandWidth::Word => Code::Test_rm16_imm16,
                    GprOperandWidth::Dword => Code::Test_rm32_imm32,
                    GprOperandWidth::Qword => Code::Test_rm64_imm32,
                };

                let op1: Register = op1.to_register(width);
                self.emit(Instruction::with2(code, op1, op2.0 as i32)?)
            },
            MachineInst::CmpRegWithImm { op1, op2, width } => {
                let op1 = op1.as_preg(valid);

                let code = match width {
                    GprOperandWidth::Byte => Code::Cmp_rm8_imm8,
                    GprOperandWidth::Word => Code::Cmp_rm16_imm16,
                    GprOperandWidth::Dword => Code::Cmp_rm32_imm32,
                    GprOperandWidth::Qword => Code::Cmp_rm64_imm32,
                };

                let op1: Register = op1.to_register(width);
                self.emit(Instruction::with2(code, op1, op2.0 as i32)?)
            },
            MachineInst::JmpWithCond { cond, target } => {
                let target = block_labels[target];

                let code = match cond {
                    Condition::O => Code::Jo_rel32_64,
                    Condition::No => Code::Jno_rel32_64,
                    Condition::Z => Code::Je_rel32_64,
                    Condition::Nz => Code::Jne_rel32_64,
                    Condition::B => Code::Jb_rel32_64,
                    Condition::Ae => Code::Jae_rel32_64,
                    Condition::Be => Code::Jbe_rel32_64,
                    Condition::A => Code::Ja_rel32_64,
                    Condition::L => Code::Jl_rel32_64,
                    Condition::Ge => Code::Jge_rel32_64,
                    Condition::Le => Code::Jle_rel32_64,
                    Condition::G => Code::Jg_rel32_64,
                };

                self.emit(Instruction::with_branch(code, target.0)?)
            },
        }
    }
}


#[cfg(test)]
mod tests {
    use cake_util::IndexVec;

    use crate::{
        codegen::x64_backend::assembler::{AssembleFunctionContext, Assembler, MachineLabel, valid::ValidForCodegenToken}, mir::{
            GprOperandWidth, ImmediateOperand, MachineBlockRef, MachineInst, MemOperand,
            MemOperandDisplacement, phys_regs::*
        }
    };

    // Basic smoke test for function encoding
    #[test]
    fn test_basic() {
        let mut assembler = Assembler {
            encoder: iced_x86::Encoder::try_new(64).unwrap(),
            current_offset: 0,
            relocs: Vec::new(),
        };
        
        let block_labels: IndexVec<MachineBlockRef, MachineLabel> = IndexVec::new();
        let fn_ctx = AssembleFunctionContext {
            labels: block_labels
        };

        // SAFETY: every register below is physical, and every operand is used at the width
        // it's declared with, so `ValidForCodegenToken`'s invariants hold.
        let valid = unsafe { ValidForCodegenToken::assume_valid() };

        let insts = vec![
            MachineInst::Push {
                op1: rbp,
                width: GprOperandWidth::Qword,
            },
            MachineInst::Mov {
                dst: rbp,
                op2: rsp,
                width: GprOperandWidth::Qword,
            },
            MachineInst::StoreImm {
                op1: MemOperand::BasePlusDisp {
                    base: rbp,
                    disp: MemOperandDisplacement::Disp8((-4i8) as u8),
                },
                op2: ImmediateOperand(2),
                width: GprOperandWidth::Dword,
            },
            MachineInst::Load {
                dst: rax,
                op2: MemOperand::BasePlusDisp {
                    base: rbp,
                    disp: MemOperandDisplacement::Disp8((-4i8) as u8),
                },
                width: GprOperandWidth::Dword,
            },
            MachineInst::AddImmToReg {
                dst: rax,
                op1: rax,
                op2: ImmediateOperand(2),
                width: GprOperandWidth::Dword,
            },
            MachineInst::Pop {
                dst: rbp,
                width: GprOperandWidth::Qword,
            },
            MachineInst::Ret
        ];

        for inst in insts {
            assembler.assemble_inst(inst, &fn_ctx, valid)
                .expect("failed to assemble instructions")
        }

        let bytes = assembler.encoder.take_buffer();
        let expected_bytes = vec![
            0x55, 0x48, 0x89, 0xE5, 0xC7, 0x85, 0xFC, 0x00, 0x00, 0x00, 0x02, 0x00,
            0x00, 0x00, 0x8B, 0x85, 0xFC, 0x00, 0x00, 0x00, 0x81, 0xC0, 0x02, 0x00,
            0x00, 0x00, 0x5D, 0xC3,
        ];

        assert_eq!(bytes, expected_bytes);
    }
}
