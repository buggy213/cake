use crate::mir::{ImmediateOperand, MachineInst, MemOperand, MemOperandDisplacement, MemOperandScale, OperandWidth, PhysReg, Reg};

/// ModR/M byte in x86 instruction encoding
struct ModRM {
    mod_: u8,
    reg: u8,
    rm: u8
}

impl From<ModRM> for u8 {
    fn from(value: ModRM) -> Self {
        value.mod_ << 6 | value.reg << 3 | value.rm
    }
}

struct SIB {
    scale: u8,
    index: u8,
    base: u8
}

impl From<SIB> for u8 {
    fn from(value: SIB) -> Self {
        value.scale << 6 | value.index << 3 | value.base
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct REX {
    w: bool,
    r: bool,
    x: bool,
    b: bool
}

impl REX {
    fn new() -> REX {
        Self {
            w: false,
            r: false,
            x: false,
            b: false,
        }
    }

    fn is_empty(self) -> bool {
        self == REX::default()
    }
}

impl Default for REX {
    fn default() -> Self {
        Self::new()
    }
}

impl From<REX> for u8 {
    fn from(value: REX) -> Self {
        0x40 | (value.w as u8) << 3 | (value.r as u8) << 2 | (value.x as u8) << 1 | value.b as u8
    }
}

#[derive(Clone, Copy)]
enum AssemblerMemOperandDisplacement {
    Disp8(u8),
    Disp32(u32),
}

impl AssemblerMemOperandDisplacement {
    fn assemble_into_buffer(self, buffer: &mut Vec<u8>) {
        match self {
            AssemblerMemOperandDisplacement::Disp8(b) => buffer.push(b),
            AssemblerMemOperandDisplacement::Disp32(dw) => {
                for b in dw.to_le_bytes() {
                    buffer.push(b);
                }
            },
        }
    }
}

impl From<MemOperandDisplacement> for Option<AssemblerMemOperandDisplacement> {
    fn from(value: MemOperandDisplacement) -> Self {
        match value {
            MemOperandDisplacement::Disp32(disp) => Some(AssemblerMemOperandDisplacement::Disp32(disp)),
            MemOperandDisplacement::Disp8(disp) => Some(AssemblerMemOperandDisplacement::Disp8(disp)),
            MemOperandDisplacement::Zero => None,
        }
    }
}

impl From<MemOperandScale> for u8 {
    fn from(value: MemOperandScale) -> Self {
        match value {
            MemOperandScale::One => 0b00,
            MemOperandScale::Two => 0b01,
            MemOperandScale::Four => 0b10,
            MemOperandScale::Eight => 0b11,
        }
    }
}

struct AssemblerMemOperand {
    sib: Option<SIB>,
    disp: Option<AssemblerMemOperandDisplacement>,
    width: OperandWidth,
}

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

/// Performs logic needed to assemble an MIR memory operand
/// Including selecting correct B.R/M, Mod, and returning an SIB / displacement if they are required
fn assemble_mem_operand(mem_operand: MemOperand, mod_rm: &mut ModRM, rex: &mut REX) -> AssemblerMemOperand {
    match mem_operand {
        MemOperand::PcRelative { disp, width } => {
            mod_rm.mod_ = 0b00;
            mod_rm.rm = 0b101;
            rex.b = false;

            AssemblerMemOperand {
                sib: None,
                disp: disp.into(),
                width
            }
        },
        MemOperand::Full { base, index, scale, mut disp, width } => {
            let base = validate_mem_operand_preg(base);
            let index = validate_mem_operand_preg(index);
            assert!(index != PhysReg::rsp && index != PhysReg::r12);

            // mod=0b00 and rm=0b101 encodes no PC-relative addressing, so we need to use mod=0b01 instead
            if matches!(disp, MemOperandDisplacement::Zero) && matches!(base, PhysReg::rbp | PhysReg::r13) {
                disp = MemOperandDisplacement::Disp8(0);
            }
            
            mod_rm.mod_ = match disp {
                MemOperandDisplacement::Disp32(_) => 0b10,
                MemOperandDisplacement::Disp8(_) => 0b01,
                MemOperandDisplacement::Zero => 0b00,
            };
            mod_rm.rm = 0b100;
            
            let sib = SIB {
                scale: scale.into(),
                index: index.encoding(),
                base: base.encoding(),
            };

            rex.x = index.is_extended_reg();
            rex.b = base.is_extended_reg();

            AssemblerMemOperand { 
                sib: Some(sib), 
                disp: disp.into(),
                width
            }
        },
        MemOperand::BasePlusDisp { base, mut disp, width } => {
            let base = validate_mem_operand_preg(base);

            // mod=0b00 and rm=0b101 encodes PC-relative addressing, so we need to use mod=0b01 instead
            if matches!(disp, MemOperandDisplacement::Zero) && matches!(base, PhysReg::rbp | PhysReg::r13) {
                disp = MemOperandDisplacement::Disp8(0);
            }

            mod_rm.mod_ = match disp {
                MemOperandDisplacement::Disp32(_) => 0b10,
                MemOperandDisplacement::Disp8(_) => 0b01,
                MemOperandDisplacement::Zero => 0b00,
            };
            mod_rm.rm = base.encoding();

            // when rm=0b100, we must put in a SIB byte; use index=0b100 (rsp) to not include index
            let sib = if matches!(base, PhysReg::rsp) {
                Some(SIB { scale: 0b00, index: 0b100, base: base.encoding() })
            } else { None };

            rex.b = base.is_extended_reg();

            AssemblerMemOperand { 
                sib, 
                disp: disp.into(), 
                width 
            }
        },
        MemOperand::AbsoluteDisp { mut disp, width } => {
            // mod=0b00, rm=0b100, base=0b101, index=0b100 encodes [disp32], so widen displacement if needed
            match disp {
                MemOperandDisplacement::Disp32(_) => (),
                MemOperandDisplacement::Disp8(v) => {
                    disp = MemOperandDisplacement::Disp32(v as u32)
                },
                MemOperandDisplacement::Zero => {
                    disp = MemOperandDisplacement::Disp32(0u32)
                },
            }
            
            mod_rm.mod_ = 0b00;
            mod_rm.rm = 0b100;
            
            let sib = SIB {
                scale: 0b00,
                index: 0b100,
                base: 0b101,
            };

            AssemblerMemOperand { 
                sib: Some(sib), 
                disp: disp.into(), 
                width 
            }
        },
    }
}

fn assemble_inst(buffer: &mut Vec<u8>, mir_inst: MachineInst) {
    // Assembles instructions with one register operand and one memory operand specified by ModR/M
    fn assemble_reg_mem_modrm(
        width: OperandWidth, 
        operand_prefix: bool, 
        reg: PhysReg,
        mem: MemOperand,
        opcode: u8,
        buffer: &mut Vec<u8>
    ) {
        let mut rex = REX::default();
        if width == OperandWidth::Qword {
            rex.w = true;
        }
        if reg.is_extended_reg() {
            rex.r = true;
        }

        let mut mod_rm = ModRM {
            mod_: 0b00,
            reg: reg.encoding(),
            rm: 0b000,
        };

        let AssemblerMemOperand { 
            sib, 
            disp, 
            width 
        } = assemble_mem_operand(mem, &mut mod_rm, &mut rex);

        if operand_prefix {
            buffer.push(0x66);
        }
        if !rex.is_empty() || reg.needs_rex(width) {
            buffer.push(rex.into());
        }
        buffer.push(opcode);
        buffer.push(mod_rm.into());
        if let Some(sib) = sib {
            buffer.push(sib.into());
        }
        if let Some(disp) = disp {
            disp.assemble_into_buffer(buffer);
        }
    }

    // Assembles instructions with two register operands specified by ModR/M
    fn assemble_reg_reg_modrm(
        width: OperandWidth, 
        operand_prefix: bool, 
        reg: PhysReg,
        r: PhysReg,
        opcode: u8,
        buffer: &mut Vec<u8>
    ) {
        let mut rex = REX::default();
        if width == OperandWidth::Qword {
            rex.w = true;
        }
        if r.is_extended_reg() {
            rex.b = true;
        }
        if reg.is_extended_reg() {
            rex.r = true;
        }

        let mod_rm = ModRM {
            mod_: 0b11,
            reg: reg.encoding(),
            rm: r.encoding(),
        };

        if operand_prefix {
            buffer.push(0x66);
        }
        if !rex.is_empty() | reg.needs_rex(width) | r.needs_rex(width) {
            buffer.push(rex.into());
        }
        buffer.push(opcode);
        buffer.push(mod_rm.into());
    }

    // Assembles instructions with one register operand in the opcode
    fn assemble_reg_opcode(
        width: OperandWidth,
        operand_prefix: bool,
        reg: PhysReg,
        opcode: u8,
        buffer: &mut Vec<u8>
    ) {
        let mut rex = REX::default();
        if width == OperandWidth::Qword {
            rex.w = true;
        }
        if reg.is_extended_reg() {
            rex.b = true;
        }

        if operand_prefix {
            buffer.push(0x66);
        }
        if !rex.is_empty() {
            buffer.push(rex.into())
        }
        buffer.push(opcode + reg.encoding());
    }


    match mir_inst {
        MachineInst::Lea { dst, op2 } => {
            let (dst, width) = validate_preg(dst);
            assert!(width != OperandWidth::Byte);

            let opcode: u8 = 0x8d;
            let operand_prefix = width == OperandWidth::Word;

            
            assemble_reg_mem_modrm(width, operand_prefix, dst, op2, opcode, buffer);
        },
        MachineInst::AddRegToReg { dst, op1, op2 } => {
            let (op1, op2, width) = validate_tied_pregs(dst, op1, op2);

            let (opcode, operand_prefix): (u8, bool) = match width {
                OperandWidth::Byte => (0x00, false),
                OperandWidth::Word => (0x01, true),
                OperandWidth::Dword
                | OperandWidth::Qword => (0x01, false),
            };

            assemble_reg_reg_modrm(width, operand_prefix, op2, op1, opcode, buffer);
        },
        MachineInst::AddMemToReg { dst, op1, op2 } => todo!(),
        MachineInst::AddRegToMem { op1, op2 } => todo!(),
        MachineInst::AddImmToReg { dst, op1, op2 } => todo!(),
        MachineInst::AddImmToMem { op1, op2 } => todo!(),
        MachineInst::Load { dst, op2 } => {
            let (dst, width) = validate_preg(dst);

            let (opcode, operand_prefix) = match width {
                OperandWidth::Byte => (0x8a, false),
                OperandWidth::Word => (0x8b, true),
                OperandWidth::Dword
                | OperandWidth::Qword => (0x8b, false),
            };

            assemble_reg_mem_modrm(width, operand_prefix, dst, op2, opcode, buffer);
        },
        MachineInst::LoadImm { dst, op2 } => todo!(),
        MachineInst::StoreReg { op1, op2 } => todo!(),
        MachineInst::StoreImm { op1, op2 } => todo!(),
        MachineInst::ZeroExtend { dst, op2 } => todo!(),
        MachineInst::SignExtend { dst, op2 } => todo!(),
        MachineInst::Push { op1 } => {
            let (op1, width) = validate_preg(op1);
            assert!(width != OperandWidth::Byte && width != OperandWidth::Dword);

            let operand_prefix = width == OperandWidth::Word;
            let opcode: u8 = 0x50;

            assemble_reg_opcode(width, operand_prefix, op1, opcode, buffer);
        },
        MachineInst::PushImm { op1 } => todo!(),
        MachineInst::Pop { dst } => todo!(),
        MachineInst::Jump { target } => todo!(),
        MachineInst::Call { target } => todo!(),
        MachineInst::CallIndirect { target } => {
            let (target, width) = validate_preg(target);
            assert!(width == OperandWidth::Qword);

            let mut rex = REX::default();
            if target.is_extended_reg() {
                rex.b = true;
            }

            let opcode = 0xff;
            
            if !rex.is_empty() {
                buffer.push(rex.into());
            }
            buffer.push(opcode);
        },
        MachineInst::Ret => {
            buffer.push(0xc3);
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
}