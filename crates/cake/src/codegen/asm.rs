// Abstractions over RV64G (only target for now)

#[derive(Debug, Copy, Clone)]
struct RegisterName(u8);
#[derive(Debug, Copy, Clone)]
struct Immediate(u16);
#[derive(Debug, Copy, Clone)]
struct UImmediate(u32);

#[derive(Debug, Copy, Clone)]
enum RVInstructions {
    // rd, rs1, imm (I-type)
    addi(RegisterName, RegisterName, Immediate),
    slti(RegisterName, RegisterName, Immediate),
    sltiu(RegisterName, RegisterName, Immediate),
    xori(RegisterName, RegisterName, Immediate),
    ori(RegisterName, RegisterName, Immediate),
    andi(RegisterName, RegisterName, Immediate),
    slli(RegisterName, RegisterName, Immediate),
    srli(RegisterName, RegisterName, Immediate),
    srai(RegisterName, RegisterName, Immediate),

    jalr(RegisterName, RegisterName, Immediate),

    lb(RegisterName, RegisterName, Immediate),
    lh(RegisterName, RegisterName, Immediate),
    lw(RegisterName, RegisterName, Immediate),
    lbu(RegisterName, RegisterName, Immediate),
    lhu(RegisterName, RegisterName, Immediate),

    // rd, rs1, rs2 (R-type)
    add(RegisterName, RegisterName, RegisterName),
    sub(RegisterName, RegisterName, RegisterName),
    sll(RegisterName, RegisterName, RegisterName),
    slt(RegisterName, RegisterName, RegisterName),
    sltu(RegisterName, RegisterName, RegisterName),
    xor(RegisterName, RegisterName, RegisterName),
    srl(RegisterName, RegisterName, RegisterName),
    sra(RegisterName, RegisterName, RegisterName),
    or(RegisterName, RegisterName, RegisterName),
    and(RegisterName, RegisterName, RegisterName),

    // rs1, rs2, immediate (B-type)
    beq(RegisterName, RegisterName, Immediate),
    bne(RegisterName, RegisterName, Immediate),
    blt(RegisterName, RegisterName, Immediate),
    bge(RegisterName, RegisterName, Immediate),
    bltu(RegisterName, RegisterName, Immediate),
    bgeu(RegisterName, RegisterName, Immediate),

    // rs1, rs2, immediate (S-type)
    sb(RegisterName, RegisterName, Immediate),
    sh(RegisterName, RegisterName, Immediate),
    sw(RegisterName, RegisterName, Immediate),

    // rd, immediate (U-type)
    lui(RegisterName, UImmediate),
    auipc(RegisterName, UImmediate),

    // rd, immediate (J-type)
    jal(RegisterName, UImmediate),
}

// lo inclusive, hi inclusive
const fn extract_bits(n: u32, lo: u32, hi: u32) -> u32 {
    let mask = (1 << (hi - lo + 1)) - 1;
    (n >> lo) & mask
}

impl RVInstructions {
    const fn r_type(rd: RegisterName, rs1: RegisterName, rs2: RegisterName, funct3: u8, funct7: u8, opcode: u8) -> u32 {
        assert!(funct3 <= 0b111, "f3 must be representable in 3 bits");
        assert!(funct7 <= 0b1111111, "f7 must be representable in 7 bits");
        assert!(opcode <= 0b1111111, "opcode must be representable in 7 bits");

        let opcode = opcode as u32;
        let rd = (rd.0 as u32) << 7;
        let f3 = (funct3 as u32) << 12;
        let rs1 = (rs1.0 as u32) << 15;
        let rs2 = (rs2.0 as u32) << 20;
        let f7 = (funct7 as u32) << 25;

        f7 | rs2 | rs1 | f3 | rd | opcode
    }

    const fn i_type(rd: RegisterName, rs1: RegisterName, imm: Immediate, funct3: u8, opcode: u8) -> u32 {
        assert!(funct3 <= 0b111, "f3 must be representable in 3 bits");
        assert!(opcode <= 0b1111111, "opcode must be representable in 7 bits");

        let opcode = opcode as u32;
        let rd = (rd.0 as u32) << 7;
        let f3 = (funct3 as u32) << 12;
        let rs1 = (rs1.0 as u32) << 15;
        let imm = (imm.0 as u32) << 20;

        imm | rs1 | f3 | rd | opcode
    }

    const fn b_type(rs1: RegisterName, rs2: RegisterName, imm: Immediate, funct3: u8) -> u32 {
        assert!(funct3 <= 0b111, "f3 must be representable in 3 bits");

        let opcode = 0b1100011u32;
        let low_imm = (extract_bits(imm.0 as u32, 1, 4) << 8) | (extract_bits(imm.0 as u32, 11, 11) << 7);
        let f3 = (funct3 as u32) << 12;
        let rs1 = (rs1.0 as u32) << 15;
        let rs2 = (rs2.0 as u32) << 20;
        let hi_imm = (extract_bits(imm.0 as u32, 12, 12) << 31) | (extract_bits(imm.0 as u32, 5, 10) << 25);

        hi_imm | rs2 | rs1 | f3 | low_imm | opcode
    }

    const fn s_type(rs1: RegisterName, rs2: RegisterName, imm: Immediate, funct3: u8) -> u32 {
        assert!(funct3 <= 0b111, "f3 must be representable in 3 bits");

        let opcode = 0b0100011u32;
        let low_imm = extract_bits(imm.0 as u32, 0, 4) << 7;
        let f3 = (funct3 as u32) << 12;
        let rs1 = (rs1.0 as u32) << 15;
        let rs2 = (rs2.0 as u32) << 20;
        let hi_imm = extract_bits(imm.0 as u32, 5, 11) << 25;

        hi_imm | rs2 | rs1 | f3 | low_imm | opcode
    }

    const fn j_type(rd: RegisterName, uimm: UImmediate) -> u32 {
        let opcode = 0b1101111u32;
        let rd = (rd.0 as u32) << 7;
        let imm =
            (extract_bits(uimm.0, 12, 19) << 12)
            | (extract_bits(uimm.0, 11, 11) << 20)
            | (extract_bits(uimm.0, 1, 10) << 21)
            | (extract_bits(uimm.0, 20, 20) << 31);

        imm | rd | opcode
    }

    const fn u_type(rd: RegisterName, uimm: UImmediate, opcode: u8) -> u32 {
        assert!(opcode <= 0b1111111, "opcode must be representable in 7 bits");

        let opcode = opcode as u32;
        let rd = (rd.0 as u32) << 7;
        let imm = extract_bits(uimm.0, 12, 31) << 12;

        imm | rd | opcode
    }

    const fn encoding(self) -> u32 {
        match self {
            // i-type
            RVInstructions::addi(rd, rs1, imm)  => Self::i_type(rd, rs1, imm, 0b000, 0b0010011),
            RVInstructions::slti(rd, rs1, imm)  => Self::i_type(rd, rs1, imm, 0b010, 0b0010011),
            RVInstructions::sltiu(rd, rs1, imm) => Self::i_type(rd, rs1, imm, 0b011, 0b0010011),
            RVInstructions::xori(rd, rs1, imm)  => Self::i_type(rd, rs1, imm, 0b100, 0b0010011),
            RVInstructions::ori(rd, rs1, imm)   => Self::i_type(rd, rs1, imm, 0b110, 0b0010011),
            RVInstructions::andi(rd, rs1, imm)  => Self::i_type(rd, rs1, imm, 0b111, 0b0010011),
            RVInstructions::slli(rd, rs1, imm)  => Self::i_type(rd, rs1, imm, 0b001, 0b0010011),
            RVInstructions::srli(rd, rs1, imm)  => Self::i_type(rd, rs1, imm, 0b101, 0b0010011),
            RVInstructions::srai(rd, rs1, imm)  => {
                Self::i_type(rd, rs1, Immediate(imm.0 | (1 << 10)), 0b101, 0b0010011)
            },

            RVInstructions::jalr(rd, rs1, imm) => {
                Self::i_type(rd, rs1, imm, 0b000, 0b1100111)
            },

            RVInstructions::lb(rd, rs1, imm)  => Self::i_type(rd, rs1, imm, 0b000, 0b0000011),
            RVInstructions::lh(rd, rs1, imm)  => Self::i_type(rd, rs1, imm, 0b001, 0b0000011),
            RVInstructions::lw(rd, rs1, imm)  => Self::i_type(rd, rs1, imm, 0b010, 0b0000011),
            RVInstructions::lbu(rd, rs1, imm) => Self::i_type(rd, rs1, imm, 0b100, 0b0000011),
            RVInstructions::lhu(rd, rs1, imm) => Self::i_type(rd, rs1, imm, 0b101, 0b0000011),

            // r-type
            RVInstructions::add(rd, rs1, rs2)  => Self::r_type(rd, rs1, rs2, 0b000, 0b0000000, 0b0110011),
            RVInstructions::sub(rd, rs1, rs2)  => Self::r_type(rd, rs1, rs2, 0b000, 0b0100000, 0b0110011),
            RVInstructions::sll(rd, rs1, rs2)  => Self::r_type(rd, rs1, rs2, 0b001, 0b0000000, 0b0110011),
            RVInstructions::slt(rd, rs1, rs2)  => Self::r_type(rd, rs1, rs2, 0b010, 0b0000000, 0b0110011),
            RVInstructions::sltu(rd, rs1, rs2) => Self::r_type(rd, rs1, rs2, 0b011, 0b0000000, 0b0110011),
            RVInstructions::xor(rd, rs1, rs2)  => Self::r_type(rd, rs1, rs2, 0b100, 0b0000000, 0b0110011),
            RVInstructions::srl(rd, rs1, rs2)  => Self::r_type(rd, rs1, rs2, 0b101, 0b0000000, 0b0110011),
            RVInstructions::sra(rd, rs1, rs2)  => Self::r_type(rd, rs1, rs2, 0b101, 0b0100000, 0b0110011),
            RVInstructions::or(rd, rs1, rs2)   => Self::r_type(rd, rs1, rs2, 0b110, 0b0000000, 0b0110011),
            RVInstructions::and(rd, rs1, rs2)  => Self::r_type(rd, rs1, rs2, 0b111, 0b0000000, 0b0110011),

            // b-type
            RVInstructions::beq(rs1, rs2, imm)  => Self::b_type(rs1, rs2, imm, 0b000),
            RVInstructions::bne(rs1, rs2, imm)  => Self::b_type(rs1, rs2, imm, 0b001),
            RVInstructions::blt(rs1, rs2, imm)  => Self::b_type(rs1, rs2, imm, 0b100),
            RVInstructions::bge(rs1, rs2, imm)  => Self::b_type(rs1, rs2, imm, 0b101),
            RVInstructions::bltu(rs1, rs2, imm) => Self::b_type(rs1, rs2, imm, 0b110),
            RVInstructions::bgeu(rs1, rs2, imm) => Self::b_type(rs1, rs2, imm, 0b111),

            // s-type
            RVInstructions::sb(rs1, rs2, imm) => Self::s_type(rs1, rs2, imm, 0b000),
            RVInstructions::sh(rs1, rs2, imm) => Self::s_type(rs1, rs2, imm, 0b001),
            RVInstructions::sw(rs1, rs2, imm) => Self::s_type(rs1, rs2, imm, 0b010),

            // u-type
            RVInstructions::lui(rd, uimm)   => Self::u_type(rd, uimm, 0b0110111),
            RVInstructions::auipc(rd, uimm) => Self::u_type(rd, uimm, 0b0010111),

            // j-type
            RVInstructions::jal(rd, uimm) => Self::j_type(rd, uimm)
        }
    }
}