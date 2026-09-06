use iced_x86::{Code::Add_rm8_r8, Instruction, MemoryOperand, Register, code_asm::{AsmMemoryOperand, AsmRegister8, AsmRegister16, AsmRegister32, AsmRegister64, CodeAssembler, byte_ptr, dword_ptr, qword_ptr, word_ptr}};

use crate::mir::{ImmediateOperand, MachineInst, MemOperand, MemOperandDisplacement, MemOperandScale, OperandWidth, PhysReg, Reg};

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
    todo!()
}

fn validate_pregs_and_imm(a: Reg, b: Reg, c: ImmediateOperand) -> (PhysReg, PhysReg, u64, OperandWidth) {
    todo!()
}

impl From<PhysReg> for Register {
    fn from(value: PhysReg) -> Self {
        todo!()
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

fn assemble_inst(assembler: &mut CodeAssembler, mir_inst: MachineInst) -> Result<(), iced_x86::IcedError> {
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

            let dst: Register = dst.into();
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

            let dst: Register = op1.into();
            let addend: Register = op2.into();
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

            let dst: Register = dst.into();
            let addend: MemoryOperand = op2.into();
            return assembler.add_instruction(Instruction::with2(code, dst, addend)?)
        },
        MachineInst::AddRegToMem { op1, op2 } => todo!(),
        MachineInst::AddImmToReg { dst, op1, op2 } => todo!(),
        MachineInst::AddImmToMem { op1, op2 } => todo!(),
        MachineInst::Load { dst, op2 } => {
            let (dst, width) = validate_preg(dst);

            let code = match width {
                OperandWidth::Byte => Code::Mov_r8_rm8,
                OperandWidth::Word => Code::Mov_r16_rm16,
                OperandWidth::Dword => Code::Mov_r32_rm32,
                OperandWidth::Qword => Code::Mov_r64_rm64,
            };

            let dst: Register = dst.into();
            let src: MemoryOperand = op2.into();
            return assembler.add_instruction(Instruction::with2(code, dst, src)?)
        },
        MachineInst::LoadImm { dst, op2 } => todo!(),
        MachineInst::StoreReg { op1, op2 } => todo!(),
        MachineInst::StoreImm { op1, op2 } => todo!(),
        MachineInst::ZeroExtend { dst, op2 } => todo!(),
        MachineInst::SignExtend { dst, op2 } => todo!(),
        MachineInst::Push { op1 } => {
            let (op1, width) = validate_preg(op1);
            assert!(width != OperandWidth::Byte && width != OperandWidth::Dword);

            todo!()
        },
        MachineInst::PushImm { op1 } => todo!(),
        MachineInst::Pop { dst } => todo!(),
        MachineInst::Jump { target } => todo!(),
        MachineInst::Call { target } => todo!(),
        MachineInst::CallIndirect { target } => {
            let (target, width) = validate_preg(target);
            assert!(width == OperandWidth::Qword);

            todo!()
        },
        MachineInst::Ret => {
            todo!()
        },
        MachineInst::MulRegToReg { dst, op1, op2 } => todo!(),
        MachineInst::MulMemToReg { dst, op1, op2 } => todo!(),
        MachineInst::MulRegWithImm { dst, op1, op2 } => {
            
        },
        MachineInst::MulMemWithImm { dst, op1, op2 } => todo!(),
        MachineInst::UDivByReg { dst_quo, dst_rem, op1 } => todo!(),
        MachineInst::UDivByMem { dst_quo, dst_rem, op1 } => todo!(),
        MachineInst::SDivByReg { dst_quo, dst_rem, op1 } => todo!(),
        MachineInst::SDivByMem { dst_quo, dst_rem, op1 } => todo!(),
        MachineInst::PrepareDiv { width } => todo!(),
        MachineInst::AndRegToReg { dst, op1, op2 } => todo!(),
        MachineInst::OrRegToReg { dst, op1, op2 } => todo!(),
        MachineInst::XorRegToReg { dst, op1, op2 } => todo!(),
        MachineInst::NotReg { dst, op1 } => todo!(),
        MachineInst::TestRegWithImm { op1, op2 } => todo!(),
        MachineInst::CmpRegWithImm { op1, op2 } => todo!(),
        MachineInst::JmpWithCond { cond, target } => todo!(),
    }

    todo!()
}