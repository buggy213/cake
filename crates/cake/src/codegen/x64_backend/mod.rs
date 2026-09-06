use std::assert_matches;

use iced_x86::{IcedError, Instruction, MemoryOperand, Register, code_asm::{CodeAssembler, CodeLabel}};

use crate::mir::{Condition, ImmediateOperand, MachineInst, MemOperand, MemOperandDisplacement, OperandWidth, PhysReg, Reg};

fn validate_tied_pregs(a: Reg, b: Reg, c: Reg) -> (PhysReg, PhysReg, OperandWidth) {
    match (a, b, c) {
        (Reg::PReg(r1, w1),
            Reg::PReg(r2, w2),
            Reg::PReg(r3, w3)) => {
            if r1 != r2 {
                panic!("registers must be tied")
            }

            if w1 != w2 || w1 != w3 {
                panic!("registers must be equal width")
            }

            (r1, r3, w1)
        },
        _ => panic!("cannot assemble instruction with virtual registers")
    }
}

fn validate_preg(a: Reg) -> (PhysReg, OperandWidth) {
    match a {
        Reg::PReg(phys_reg, operand_width) => (phys_reg, operand_width),
        _ => panic!("cannot assemble instruction with virtual registers")
    }
}

// We never use 32-bit address size override, so ensure that virtual registers are always 64-bit wide
// when used in a mem operand
fn validate_mem_operand_preg(a: Reg) -> PhysReg {
    match a {
        Reg::PReg(phys_reg, operand_width) => {
            if operand_width != OperandWidth::Qword {
                panic!("register used in memory operand must be full-width")
            }

            phys_reg
        },
        _ => panic!("cannot assemble instruction with virtual registers")
    }
}

fn validate_pregs(a: Reg, b: Reg) -> (PhysReg, PhysReg, OperandWidth) {
    match (a, b) {
        (Reg::PReg(r1, w1), Reg::PReg(r2, w2)) => {
            if w1 != w2 {
                panic!("registers must be equal width")
            }

            (r1, r2, w1)
        },
        _ => panic!("cannot assemble instruction with virtual registers")
    }
}

fn validate_pregs_and_imm(a: Reg, b: Reg, c: ImmediateOperand) -> (PhysReg, PhysReg, u64, OperandWidth) {
    let (r1, r2, width) = validate_pregs(a, b);
    (r1, r2, c.value, width)
}

impl PhysReg {
    /// Widens/narrows the physical register to the requested operand width
    fn to_register(self, width: OperandWidth) -> Register {
        use Register::*;

        match width {
            OperandWidth::Byte => match self {
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
            },
            OperandWidth::Word => match self {
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
            },
            OperandWidth::Dword => match self {
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
            },
            OperandWidth::Qword => match self {
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
            },
        }
    }
}

impl From<PhysReg> for Register {
    // Memory operands (base/index) are always full-width
    fn from(value: PhysReg) -> Self {
        value.to_register(OperandWidth::Qword)
    }
}

impl From<MemOperand> for MemoryOperand {
    fn from(value: MemOperand) -> Self {
        let displ_size_from_displacement = |w: MemOperandDisplacement| match w {
            MemOperandDisplacement::Disp32(_) => 4,
            MemOperandDisplacement::Disp8(_) => 1,
            MemOperandDisplacement::Zero => 0
        };

        match value {
            MemOperand::PcRelative { disp, width } => {
                // have to widen to disp32 for this addressing mode
                let disp = disp.as_u32();

                MemoryOperand::with_base_displ_size(Register::RIP, disp as i64, 4)
            },
            MemOperand::Full { base, index, scale, disp, width } => {
                let base = validate_mem_operand_preg(base);
                let index = validate_mem_operand_preg(index);

                let base: Register = base.into();
                let index: Register = index.into();
                let scale: u32 = scale.as_u32();
                let displ_size = displ_size_from_displacement(disp);
                let disp: u32 = disp.as_u32();

                MemoryOperand::with_base_index_scale_displ_size(base, index, scale, disp as i64, displ_size)
            },
            MemOperand::BasePlusDisp { base, disp, width } => {
                let base = validate_mem_operand_preg(base);

                let base: Register = base.into();
                let displ_size = displ_size_from_displacement(disp);
                let disp = disp.as_u32();

                MemoryOperand::with_base_displ_size(base, disp as i64, displ_size)
            },
            MemOperand::AbsoluteDisp { disp, width } => {
                // have to widen to disp32 for this particular addressing mode
                let disp = disp.as_u32();

                MemoryOperand::with_displ(disp as u64, 4)
            },
        }
    }
}

/// Assembles a single MIR instruction into `assembler`.
///
/// `block_labels` must contain a `CodeLabel` (created via
/// `assembler.create_label()`) for every `MachineBlockRef` that may be
/// targeted by a jump; it is the caller's responsibility to `set_label`
/// them at the right point while emitting the rest of the function.
fn assemble_inst(
    assembler: &mut CodeAssembler,
    mir_inst: MachineInst,
    block_labels: &[CodeLabel],
) -> Result<(), IcedError> {
    use iced_x86::Code;

    match mir_inst {
        MachineInst::Lea { dst, op2 } => {
            let (dst, width) = validate_preg(dst);

            let code = match width {
                OperandWidth::Byte => panic!("lea with byte-sized dst not encodable"),
                OperandWidth::Word => iced_x86::Code::Lea_r16_m,
                OperandWidth::Dword => iced_x86::Code::Lea_r32_m,
                OperandWidth::Qword => iced_x86::Code::Lea_r64_m,
            };

            let dst: Register = dst.to_register(width);
            let mem: MemoryOperand = op2.into();
            return assembler.add_instruction(Instruction::with2(code, dst, mem)?)
        },
        MachineInst::AddRegToReg { dst, op1, op2 } => {
            let (op1, op2, width) = validate_tied_pregs(dst, op1, op2);

            let code = match width {
                OperandWidth::Byte => iced_x86::Code::Add_rm8_r8,
                OperandWidth::Word => iced_x86::Code::Add_rm16_r16,
                OperandWidth::Dword => iced_x86::Code::Add_rm32_r32,
                OperandWidth::Qword => iced_x86::Code::Add_rm64_r64,
            };

            let dst: Register = op1.to_register(width);
            let addend: Register = op2.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, dst, addend)?)
        },
        MachineInst::AddMemToReg { dst, op1, op2 } => {
            let (dst, op1, width) = validate_tied_pregs(dst, op1, op1);

            let code = match width {
                OperandWidth::Byte => iced_x86::Code::Add_r8_rm8,
                OperandWidth::Word => iced_x86::Code::Add_r16_rm16,
                OperandWidth::Dword => iced_x86::Code::Add_r32_rm32,
                OperandWidth::Qword => iced_x86::Code::Add_r64_rm64,
            };

            let dst: Register = dst.to_register(width);
            let addend: MemoryOperand = op2.into();
            return assembler.add_instruction(Instruction::with2(code, dst, addend)?)
        },
        MachineInst::AddRegToMem { op1, op2 } => {
            let (op2, width) = validate_preg(op2);

            let code = match width {
                OperandWidth::Byte => Code::Add_rm8_r8,
                OperandWidth::Word => Code::Add_rm16_r16,
                OperandWidth::Dword => Code::Add_rm32_r32,
                OperandWidth::Qword => Code::Add_rm64_r64,
            };

            let mem: MemoryOperand = op1.into();
            let op2: Register = op2.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, mem, op2)?)
        },
        MachineInst::AddImmToReg { dst, op1, op2 } => {
            let (dst, _, width) = validate_tied_pregs(dst, op1, op1);

            let code = match width {
                OperandWidth::Byte => Code::Add_rm8_imm8,
                OperandWidth::Word => Code::Add_rm16_imm16,
                OperandWidth::Dword => Code::Add_rm32_imm32,
                OperandWidth::Qword => Code::Add_rm64_imm32,
            };

            let dst: Register = dst.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, dst, op2.value as i32)?)
        },
        MachineInst::AddImmToMem { op1, op2 } => {
            let width = op1.width();

            let code = match width {
                OperandWidth::Byte => Code::Add_rm8_imm8,
                OperandWidth::Word => Code::Add_rm16_imm16,
                OperandWidth::Dword => Code::Add_rm32_imm32,
                OperandWidth::Qword => Code::Add_rm64_imm32,
            };

            let mem: MemoryOperand = op1.into();
            return assembler.add_instruction(Instruction::with2(code, mem, op2.value as i32)?)
        },
        MachineInst::Load { dst, op2 } => {
            let (dst, width) = validate_preg(dst);

            let code = match width {
                OperandWidth::Byte => Code::Mov_r8_rm8,
                OperandWidth::Word => Code::Mov_r16_rm16,
                OperandWidth::Dword => Code::Mov_r32_rm32,
                OperandWidth::Qword => Code::Mov_r64_rm64,
            };

            let dst: Register = dst.to_register(width);
            let src: MemoryOperand = op2.into();
            return assembler.add_instruction(Instruction::with2(code, dst, src)?)
        },
        MachineInst::LoadImm { dst, op2 } => {
            let (dst, width) = validate_preg(dst);
            let dst: Register = dst.to_register(width);

            return match width {
                OperandWidth::Byte => assembler.add_instruction(Instruction::with2(Code::Mov_r8_imm8, dst, op2.value as u32)?),
                OperandWidth::Word => assembler.add_instruction(Instruction::with2(Code::Mov_r16_imm16, dst, op2.value as u32)?),
                OperandWidth::Dword => assembler.add_instruction(Instruction::with2(Code::Mov_r32_imm32, dst, op2.value as u32)?),
                OperandWidth::Qword => assembler.add_instruction(Instruction::with2(Code::Mov_r64_imm64, dst, op2.value)?),
            }
        },
        MachineInst::StoreReg { op1, op2 } => {
            let (op2, width) = validate_preg(op2);

            let code = match width {
                OperandWidth::Byte => Code::Mov_rm8_r8,
                OperandWidth::Word => Code::Mov_rm16_r16,
                OperandWidth::Dword => Code::Mov_rm32_r32,
                OperandWidth::Qword => Code::Mov_rm64_r64,
            };

            let mem: MemoryOperand = op1.into();
            let op2: Register = op2.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, mem, op2)?)
        },
        MachineInst::StoreImm { op1, op2 } => {
            let width = op1.width();

            let code = match width {
                OperandWidth::Byte => Code::Mov_rm8_imm8,
                OperandWidth::Word => Code::Mov_rm16_imm16,
                OperandWidth::Dword => Code::Mov_rm32_imm32,
                OperandWidth::Qword => Code::Mov_rm64_imm32,
            };

            let mem: MemoryOperand = op1.into();
            return assembler.add_instruction(Instruction::with2(code, mem, op2.value as i32)?)
        },
        MachineInst::Mov { dst, op2 } => {
            let (dst, op2, width) = validate_pregs(dst, op2);
            
            let code = match width {
                OperandWidth::Byte => Code::Mov_rm8_r8,
                OperandWidth::Word => Code::Mov_rm16_r16,
                OperandWidth::Dword => Code::Mov_rm32_r32,
                OperandWidth::Qword => Code::Mov_rm64_r64,
            };

            let dst: Register = dst.to_register(width);
            let src: Register = op2.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, dst, src)?)
        }
        MachineInst::Xchg { op1, op2 } => {
            let (op1, op2, width) = validate_pregs(op1, op2);
            
            let code = match width {
                OperandWidth::Byte => Code::Xchg_rm8_r8,
                OperandWidth::Word => Code::Xchg_rm16_r16,
                OperandWidth::Dword => Code::Xchg_rm32_r32,
                OperandWidth::Qword => Code::Xchg_rm64_r64,
            };

            let op1: Register = op1.to_register(width);
            let op2: Register = op2.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, op1, op2)?)
        }

        MachineInst::ZeroExtend { dst, op2 } => {
            let (dst, dst_width) = validate_preg(dst);
            let (op2, src_width) = validate_preg(op2);

            let code = match (dst_width, src_width) {
                (OperandWidth::Word, OperandWidth::Byte) => Code::Movzx_r16_rm8,
                (OperandWidth::Dword, OperandWidth::Byte) => Code::Movzx_r32_rm8,
                (OperandWidth::Qword, OperandWidth::Byte) => Code::Movzx_r64_rm8,
                (OperandWidth::Dword, OperandWidth::Word) => Code::Movzx_r32_rm16,
                (OperandWidth::Qword, OperandWidth::Word) => Code::Movzx_r64_rm16,
                _ => panic!("invalid zero-extend widths"),
            };

            let dst: Register = dst.to_register(dst_width);
            let op2: Register = op2.to_register(src_width);
            return assembler.add_instruction(Instruction::with2(code, dst, op2)?)
        },
        MachineInst::SignExtend { dst, op2 } => {
            let (dst, dst_width) = validate_preg(dst);
            let (op2, src_width) = validate_preg(op2);

            let code = match (dst_width, src_width) {
                (OperandWidth::Word, OperandWidth::Byte) => Code::Movsx_r16_rm8,
                (OperandWidth::Dword, OperandWidth::Byte) => Code::Movsx_r32_rm8,
                (OperandWidth::Qword, OperandWidth::Byte) => Code::Movsx_r64_rm8,
                (OperandWidth::Dword, OperandWidth::Word) => Code::Movsx_r32_rm16,
                (OperandWidth::Qword, OperandWidth::Word) => Code::Movsx_r64_rm16,
                (OperandWidth::Qword, OperandWidth::Dword) => Code::Movsxd_r64_rm32,
                _ => panic!("invalid sign-extend widths"),
            };

            let dst: Register = dst.to_register(dst_width);
            let op2: Register = op2.to_register(src_width);
            return assembler.add_instruction(Instruction::with2(code, dst, op2)?)
        },
        MachineInst::Push { op1 } => {
            let (op1, width) = validate_preg(op1);
            assert!(width != OperandWidth::Byte && width != OperandWidth::Dword);

            let code = match width {
                OperandWidth::Word => Code::Push_r16,
                OperandWidth::Qword => Code::Push_r64,
                _ => unreachable!(),
            };

            let op1: Register = op1.to_register(width);
            return assembler.add_instruction(Instruction::with1(code, op1)?)
        },
        MachineInst::PushImm { op1 } => {
            let code = match op1.width {
                OperandWidth::Byte => panic!("push imm8 not encodable"),
                OperandWidth::Word => Code::Push_imm16,
                OperandWidth::Dword | OperandWidth::Qword => Code::Pushq_imm32,
            };

            return assembler.add_instruction(Instruction::with1(code, op1.value as i32)?)
        },
        MachineInst::Pop { dst } => {
            let (dst, width) = validate_preg(dst);
            assert!(width != OperandWidth::Byte && width != OperandWidth::Dword);

            let code = match width {
                OperandWidth::Word => Code::Pop_r16,
                OperandWidth::Qword => Code::Pop_r64,
                _ => unreachable!(),
            };

            let dst: Register = dst.to_register(width);
            return assembler.add_instruction(Instruction::with1(code, dst)?)
        },
        MachineInst::Jump { target } => {
            let label = block_labels[target.get_inner()];
            return assembler.jmp(label)
        },
        MachineInst::Call { target } => {
            todo!("relocations")
            // let label = fn_labels[target.get_inner()];
            // return assembler.call(label)
        },
        MachineInst::CallIndirect { target } => {
            let (target, width) = validate_preg(target);
            assert!(width == OperandWidth::Qword);

            let target: Register = target.to_register(width);
            return assembler.add_instruction(Instruction::with1(Code::Call_rm64, target)?)
        },
        MachineInst::Ret => {
            return assembler.add_instruction(Instruction::with(Code::Retnq))
        },
        MachineInst::MulRegToReg { dst, op1, op2 } => {
            let (op1, op2, width) = validate_tied_pregs(dst, op1, op2);

            let code = match width {
                OperandWidth::Byte => panic!("two-operand imul with byte-sized operands not encodable"),
                OperandWidth::Word => Code::Imul_r16_rm16,
                OperandWidth::Dword => Code::Imul_r32_rm32,
                OperandWidth::Qword => Code::Imul_r64_rm64,
            };

            let op1: Register = op1.to_register(width);
            let op2: Register = op2.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, op1, op2)?)
        },
        MachineInst::MulMemToReg { dst, op1, op2 } => {
            let (dst, _, width) = validate_tied_pregs(dst, op1, op1);

            let code = match width {
                OperandWidth::Byte => panic!("two-operand imul with byte-sized operands not encodable"),
                OperandWidth::Word => Code::Imul_r16_rm16,
                OperandWidth::Dword => Code::Imul_r32_rm32,
                OperandWidth::Qword => Code::Imul_r64_rm64,
            };

            let dst: Register = dst.to_register(width);
            let op2: MemoryOperand = op2.into();
            return assembler.add_instruction(Instruction::with2(code, dst, op2)?)
        },
        MachineInst::MulRegWithImm { dst, op1, op2 } => {
            let (dst, op1, imm, width) = validate_pregs_and_imm(dst, op1, op2);

            let code = match width {
                OperandWidth::Byte => panic!("imul with byte-sized operands not encodable"),
                OperandWidth::Word => Code::Imul_r16_rm16_imm16,
                OperandWidth::Dword => Code::Imul_r32_rm32_imm32,
                OperandWidth::Qword => Code::Imul_r64_rm64_imm32,
            };

            let dst: Register = dst.to_register(width);
            let op1: Register = op1.to_register(width);
            return assembler.add_instruction(Instruction::with3(code, dst, op1, imm as i32)?)
        },
        MachineInst::MulMemWithImm { dst, op1, op2 } => {
            let (dst, width) = validate_preg(dst);

            let code = match width {
                OperandWidth::Byte => panic!("imul with byte-sized operands not encodable"),
                OperandWidth::Word => Code::Imul_r16_rm16_imm16,
                OperandWidth::Dword => Code::Imul_r32_rm32_imm32,
                OperandWidth::Qword => Code::Imul_r64_rm64_imm32,
            };

            let dst: Register = dst.to_register(width);
            let op1: MemoryOperand = op1.into();
            return assembler.add_instruction(Instruction::with3(code, dst, op1, op2.value as i32)?)
        },
        MachineInst::UDivByReg { dst_quo, dst_rem, op1 } => {
            let (dst_quo, _) = validate_preg(dst_quo);
            let (dst_rem, _) = validate_preg(dst_rem);
            let (op1, width) = validate_preg(op1);
            assert_matches!(dst_quo, PhysReg::rax);
            assert_matches!(dst_rem, PhysReg::rdx);

            let code = match width {
                OperandWidth::Byte => Code::Div_rm8,
                OperandWidth::Word => Code::Div_rm16,
                OperandWidth::Dword => Code::Div_rm32,
                OperandWidth::Qword => Code::Div_rm64,
            };

            let op1: Register = op1.to_register(width);
            return assembler.add_instruction(Instruction::with1(code, op1)?)
        },
        MachineInst::UDivByMem { dst_quo, dst_rem, op1 } => {
            let (dst_quo, _) = validate_preg(dst_quo);
            let (dst_rem, _) = validate_preg(dst_rem);
            let width = op1.width();
            assert_matches!(dst_quo, PhysReg::rax);
            assert_matches!(dst_rem, PhysReg::rdx);

            let code = match width {
                OperandWidth::Byte => Code::Div_rm8,
                OperandWidth::Word => Code::Div_rm16,
                OperandWidth::Dword => Code::Div_rm32,
                OperandWidth::Qword => Code::Div_rm64,
            };

            let op1: MemoryOperand = op1.into();
            return assembler.add_instruction(Instruction::with1(code, op1)?)
        },
        MachineInst::SDivByReg { dst_quo, dst_rem, op1 } => {
            let (dst_quo, _) = validate_preg(dst_quo);
            let (dst_rem, _) = validate_preg(dst_rem);
            let (op1, width) = validate_preg(op1);
            assert_matches!(dst_quo, PhysReg::rax);
            assert_matches!(dst_rem, PhysReg::rdx);
            
            let code = match width {
                OperandWidth::Byte => Code::Idiv_rm8,
                OperandWidth::Word => Code::Idiv_rm16,
                OperandWidth::Dword => Code::Idiv_rm32,
                OperandWidth::Qword => Code::Idiv_rm64,
            };

            let op1: Register = op1.to_register(width);
            return assembler.add_instruction(Instruction::with1(code, op1)?)
        },
        MachineInst::SDivByMem { dst_quo, dst_rem, op1 } => {
            let (dst_quo, _) = validate_preg(dst_quo);
            let (dst_rem, _) = validate_preg(dst_rem);
            let width = op1.width();
            assert_matches!(dst_quo, PhysReg::rax);
            assert_matches!(dst_rem, PhysReg::rdx);
            
            let code = match width {
                OperandWidth::Byte => Code::Idiv_rm8,
                OperandWidth::Word => Code::Idiv_rm16,
                OperandWidth::Dword => Code::Idiv_rm32,
                OperandWidth::Qword => Code::Idiv_rm64,
            };

            let op1: MemoryOperand = op1.into();
            return assembler.add_instruction(Instruction::with1(code, op1)?)
        },
        MachineInst::PrepareDiv { width } => {
            let code = match width {
                OperandWidth::Byte => panic!("no sign/zero-extension instruction needed before byte-sized division"),
                OperandWidth::Word => Code::Cwd,
                OperandWidth::Dword => Code::Cdq,
                OperandWidth::Qword => Code::Cqo,
            };

            return assembler.add_instruction(Instruction::with(code))
        },
        MachineInst::AndRegToReg { dst, op1, op2 } => {
            let (op1, op2, width) = validate_tied_pregs(dst, op1, op2);

            let code = match width {
                OperandWidth::Byte => Code::And_rm8_r8,
                OperandWidth::Word => Code::And_rm16_r16,
                OperandWidth::Dword => Code::And_rm32_r32,
                OperandWidth::Qword => Code::And_rm64_r64,
            };

            let op1: Register = op1.to_register(width);
            let op2: Register = op2.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, op1, op2)?)
        },
        MachineInst::OrRegToReg { dst, op1, op2 } => {
            let (op1, op2, width) = validate_tied_pregs(dst, op1, op2);

            let code = match width {
                OperandWidth::Byte => Code::Or_rm8_r8,
                OperandWidth::Word => Code::Or_rm16_r16,
                OperandWidth::Dword => Code::Or_rm32_r32,
                OperandWidth::Qword => Code::Or_rm64_r64,
            };

            let op1: Register = op1.to_register(width);
            let op2: Register = op2.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, op1, op2)?)
        },
        MachineInst::XorRegToReg { dst, op1, op2 } => {
            let (op1, op2, width) = validate_tied_pregs(dst, op1, op2);

            let code = match width {
                OperandWidth::Byte => Code::Xor_rm8_r8,
                OperandWidth::Word => Code::Xor_rm16_r16,
                OperandWidth::Dword => Code::Xor_rm32_r32,
                OperandWidth::Qword => Code::Xor_rm64_r64,
            };

            let op1: Register = op1.to_register(width);
            let op2: Register = op2.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, op1, op2)?)
        },
        MachineInst::NotReg { dst, op1 } => {
            let (dst, _, width) = validate_tied_pregs(dst, op1, op1);

            let code = match width {
                OperandWidth::Byte => Code::Not_rm8,
                OperandWidth::Word => Code::Not_rm16,
                OperandWidth::Dword => Code::Not_rm32,
                OperandWidth::Qword => Code::Not_rm64,
            };

            let dst: Register = dst.to_register(width);
            return assembler.add_instruction(Instruction::with1(code, dst)?)
        },
        MachineInst::TestRegWithImm { op1, op2 } => {
            let (op1, width) = validate_preg(op1);

            let code = match width {
                OperandWidth::Byte => Code::Test_rm8_imm8,
                OperandWidth::Word => Code::Test_rm16_imm16,
                OperandWidth::Dword => Code::Test_rm32_imm32,
                OperandWidth::Qword => Code::Test_rm64_imm32,
            };

            let op1: Register = op1.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, op1, op2.value as i32)?)
        },
        MachineInst::CmpRegWithImm { op1, op2 } => {
            let (op1, width) = validate_preg(op1);

            let code = match width {
                OperandWidth::Byte => Code::Cmp_rm8_imm8,
                OperandWidth::Word => Code::Cmp_rm16_imm16,
                OperandWidth::Dword => Code::Cmp_rm32_imm32,
                OperandWidth::Qword => Code::Cmp_rm64_imm32,
            };

            let op1: Register = op1.to_register(width);
            return assembler.add_instruction(Instruction::with2(code, op1, op2.value as i32)?)
        },
        MachineInst::JmpWithCond { cond, target } => {
            let label = block_labels[target.get_inner()];

            return match cond {
                Condition::O => assembler.jo(label),
                Condition::No => assembler.jno(label),
                Condition::Z => assembler.je(label),
                Condition::Nz => assembler.jne(label),
                Condition::B => assembler.jb(label),
                Condition::Ae => assembler.jae(label),
                Condition::Be => assembler.jbe(label),
                Condition::A => assembler.ja(label),
                Condition::L => assembler.jl(label),
                Condition::Ge => assembler.jge(label),
                Condition::Le => assembler.jle(label),
                Condition::G => assembler.jg(label),
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use iced_x86::code_asm::CodeAssembler;

    use crate::{codegen::x64_backend::assemble_inst, mir::{ImmediateOperand, MachineInst, MemOperand, MemOperandDisplacement, OperandWidth, PhysReg, Reg, phys_regs::*}};

    #[test]
    fn test_basic() {
        let mut assembler = CodeAssembler::new(64)
            .unwrap();

        let insts = vec![
            MachineInst::Push { op1: Reg::PReg(PhysReg::rbp, OperandWidth::Qword) },
            MachineInst::Mov { 
                dst: Reg::PReg(PhysReg::rbp, OperandWidth::Qword), 
                op2: Reg::PReg(PhysReg::rsp, OperandWidth::Qword)
            },
            MachineInst::StoreImm { 
                op1: MemOperand::BasePlusDisp { 
                    base: rbp, 
                    disp: MemOperandDisplacement::Disp8((-4i8) as u8), 
                    width: OperandWidth::Dword,
                }, 
                op2: ImmediateOperand { value: 2, width: OperandWidth::Dword } 
            },
            MachineInst::Load { 
                dst: eax, 
                op2: MemOperand::BasePlusDisp { 
                    base: rbp, 
                    disp: MemOperandDisplacement::Disp8((-4i8) as u8), 
                    width: OperandWidth::Dword,
                }
            },
            MachineInst::AddImmToReg { 
                dst: eax, 
                op1: eax, 
                op2: ImmediateOperand { value: 2, width: OperandWidth::Dword } 
            },
            MachineInst::Pop { 
                dst: Reg::PReg(PhysReg::rbp, OperandWidth::Qword) 
            },
            MachineInst::Ret
        ];

        for inst in insts {
            assemble_inst(&mut assembler, inst, &[])
                .expect("failed to assemble instructions")
        }

        let bytes = assembler.assemble(0).expect("failed to assemble instructions");
        for b in bytes {
            print!("{:02X} ", b);
        }
    }
}
