use std::{io::Read, mem::size_of};

use bitflags::bitflags;
use byteorder::{ByteOrder, ReadBytesExt, LE};
use thiserror::Error;

type Elf64Addr = u64;
type Elf64Off = u64;
type Elf64Half = u16;
type Elf64Word = u32;
type Elf64Sword = i32;
type Elf64Xword = u64;
type Elf64Sxword = u64;
type uchar = u8;

#[derive(Error, Debug)]
pub enum ElfError {
    #[error("bad or unsupported object file type {0}")]
    ElfTypeError(Elf64Half),
    #[error("bad or unsupported machine type {0}")]
    ElfMachineError(Elf64Half),
    #[error("bad or unsupported data encoding {0}")]
    ElfEndiannessError(uchar),
    #[error("bad or unsupported elf class {0}")]
    ElfClassError(uchar),
    #[error("bad version {0}")]
    ElfVersionError(Elf64Word),
    #[error("bad or unsupported section type {0}")]
    ElfSectionTypeError(Elf64Word),
    #[error("unrecognized or unsupported section flags {0}")]
    ElfSectionFlagsError(Elf64Xword),
    #[error("unrecognized or unsupported segment type {0}")]
    ElfSegmentTypeError(Elf64Word),
    #[error("bad or unknown symbol type {0}")]
    ElfSymbolTypeError(uchar),
    #[error("bad or unknown symbol visibility {0}")]
    ElfSymbolVisibilityError(uchar),
    #[error("bad or unknown symbol binding type {0}")]
    ElfSymbolBindingError(uchar),
    #[error("not an elf file")]
    NotAnElfError,
    #[error("unexpected eof while parsing")]
    UnexpectedEOFError,
    #[error("unknown error while parsing")]
    Unknown,
}

impl From<std::io::Error> for ElfError {
    fn from(value: std::io::Error) -> Self {
        match value.kind() {
            std::io::ErrorKind::UnexpectedEof => ElfError::UnexpectedEOFError,
            _ => {
                debug_assert!(false);
                ElfError::Unknown
            }
        }
    }
}

pub enum ElfClass {
    ElfClass32,
    ElfClass64,
}

impl TryFrom<uchar> for ElfClass {
    type Error = ElfError;

    fn try_from(value: uchar) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::ElfClass32),
            2 => Ok(Self::ElfClass64),
            _ => Err(ElfError::ElfClassError(value)),
        }
    }
}

// non 2's complement encodings are (probably) never going to be added
// for now, only support little endian
#[derive(PartialEq, Eq)]
pub enum ElfEndianness {
    Little,
    Big,
}

impl TryFrom<uchar> for ElfEndianness {
    type Error = ElfError;

    fn try_from(value: uchar) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Little),
            2 => Ok(Self::Big),
            _ => Err(ElfError::ElfEndiannessError(value)),
        }
    }
}

pub enum ElfVersion {
    Current,
}

impl TryFrom<uchar> for ElfVersion {
    type Error = ElfError;

    fn try_from(value: uchar) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Current),
            _ => Err(ElfError::ElfVersionError(value as Elf64Word)),
        }
    }
}

impl TryFrom<Elf64Word> for ElfVersion {
    type Error = ElfError;

    fn try_from(value: Elf64Word) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Current),
            _ => Err(ElfError::ElfVersionError(value)),
        }
    }
}

pub enum ElfMachine {
    X86,
    X86_64,
    RISCV,
}

impl TryFrom<Elf64Half> for ElfMachine {
    type Error = ElfError;

    fn try_from(value: Elf64Half) -> Result<Self, Self::Error> {
        match value {
            3 => Ok(Self::X86),
            62 => Ok(Self::X86_64),
            243 => Ok(Self::RISCV),
            _ => Err(ElfError::ElfMachineError(value)),
        }
    }
}

pub enum ElfType {
    Relocatable,
    Executable,
    SharedObject,
    Core,
}

impl TryFrom<Elf64Half> for ElfType {
    type Error = ElfError;

    fn try_from(value: Elf64Half) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Relocatable),
            2 => Ok(Self::Executable),
            3 => Ok(Self::SharedObject),
            4 => Ok(Self::Core),
            _ => Err(ElfError::ElfTypeError(value)),
        }
    }
}

pub enum ElfSectionType {
    Null,
    ProgBits,
    Symtab,
    DynSym,
    StrTab,
    RelA,
    Hash,
    Dynamic,
    Note,
    NoBits,
    Rel,
    // others unsupported for now
}

impl TryFrom<Elf64Word> for ElfSectionType {
    type Error = ElfError;

    fn try_from(value: Elf64Word) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Null),
            1 => Ok(Self::ProgBits),
            2 => Ok(Self::Symtab),
            3 => Ok(Self::StrTab),
            4 => Ok(Self::RelA),
            5 => Ok(Self::Hash),
            6 => Ok(Self::Dynamic),
            7 => Ok(Self::Note),
            8 => Ok(Self::NoBits),
            9 => Ok(Self::Rel),
            _ => Err(ElfError::ElfSectionTypeError(value)),
        }
    }
}

bitflags! {
    pub struct ElfSectionFlags: Elf64Xword {
        const WRITE = 1;
        const ALLOC = 1 << 1;
        const EXECINSTR = 1 << 2;
        const MERGE = 1 << 3;
        const STRINGS = 1 << 4;
        const INFO_LINK = 1 << 5;
        const LINK_ORDER = 1 << 6;
        const OS_NONCONFORMING = 1 << 7;
        const GROUP = 1 << 8;
        const TLS = 1 << 9;
        const COMPRESSED = 1 << 10;
    }
}

pub enum ElfSymbolType {
    NoType,
    Object,
    Func,
    Section,
    File,
    Common,
    TLS,
}

impl TryFrom<uchar> for ElfSymbolType {
    type Error = ElfError;

    fn try_from(value: uchar) -> Result<Self, Self::Error> {
        match value & 0xF {
            0 => Ok(Self::NoType),
            1 => Ok(Self::Object),
            2 => Ok(Self::Func),
            3 => Ok(Self::Section),
            4 => Ok(Self::File),
            5 => Ok(Self::Common),
            6 => Ok(Self::TLS),
            _ => Err(ElfError::ElfSymbolTypeError(value)),
        }
    }
}

pub enum ElfSymbolBinding {
    Local,
    Global,
    Weak,
}

impl TryFrom<uchar> for ElfSymbolBinding {
    type Error = ElfError;

    fn try_from(value: uchar) -> Result<Self, Self::Error> {
        match value >> 4 {
            0 => Ok(Self::Local),
            1 => Ok(Self::Global),
            2 => Ok(Self::Weak),
            _ => Err(ElfError::ElfSymbolBindingError(value)),
        }
    }
}

pub enum ElfSymbolVisibility {
    Default,
    Internal,
    Hidden,
    Protected,
}

impl TryFrom<uchar> for ElfSymbolVisibility {
    type Error = ElfError;

    fn try_from(value: uchar) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Default),
            1 => Ok(Self::Internal),
            2 => Ok(Self::Hidden),
            3 => Ok(Self::Protected),
            _ => Err(ElfError::ElfSymbolVisibilityError(value)),
        }
    }
}

pub enum ElfSegmentType {
    Null,
    Load,
    Dynamic,
    Interp,
    Note,
    SharedLib,
    ProgramHeader,
    TLS,
}

impl TryFrom<Elf64Word> for ElfSegmentType {
    type Error = ElfError;

    fn try_from(value: Elf64Word) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Null),
            1 => Ok(Self::Load),
            2 => Ok(Self::Dynamic),
            3 => Ok(Self::Interp),
            4 => Ok(Self::Note),
            5 => Ok(Self::SharedLib),
            6 => Ok(Self::ProgramHeader),
            7 => Ok(Self::TLS),
            _ => Err(ElfError::ElfSegmentTypeError(value)),
        }
    }
}

bitflags! {
    pub struct ElfSegmentPermissions: Elf64Word {
        const X = 1;
        const W = 1 << 1;
        const R = 1 << 2;
    }
}

// TODO: right now, assuming 0 padding when reading out of ELF structs
// but, maybe there is some? standard specifies it might exist but this is
// unclear.

/*
typedef struct {
        unsigned char   e_ident[EI_NIDENT];
        Elf64_Half      e_type;
        Elf64_Half      e_machine;
        Elf64_Word      e_version;
        Elf64_Addr      e_entry;
        Elf64_Off       e_phoff;
        Elf64_Off       e_shoff;
        Elf64_Word      e_flags;
        Elf64_Half      e_ehsize;
        Elf64_Half      e_phentsize;
        Elf64_Half      e_phnum;
        Elf64_Half      e_shentsize;
        Elf64_Half      e_shnum;
        Elf64_Half      e_shstrndx;
} Elf64_Ehdr;
*/

pub struct Elf64Header {
    class: ElfClass,
    endianness: ElfEndianness,
    version: ElfVersion,
    elf_type: ElfType,
    machine: ElfMachine,
    entry: Elf64Addr,
    program_header_table_offset: Elf64Off,
    section_header_table_offset: Elf64Off,
    flags: Elf64Word,
    program_header_size: Elf64Half,
    program_header_count: Elf64Half,
    section_header_size: Elf64Half,
    section_header_count: Elf64Half,
    header_name_section_idx: Elf64Half,
}

impl Elf64Header {
    fn parse_elf_header(data: &[u8]) -> Result<Self, ElfError> {
        const ELF_MAGIC: [u8; 4] = [0x7f, b'E', b'L', b'F'];
        if data[..ELF_MAGIC.len()] != ELF_MAGIC {
            return Err(ElfError::NotAnElfError);
        }

        let mut ident_data = &data[ELF_MAGIC.len()..];
        let class: ElfClass = ident_data.read_u8()?.try_into()?;
        let endianness: ElfEndianness = ident_data.read_u8()?.try_into()?;
        let _: ElfVersion = ident_data.read_u8()?.try_into()?;

        if endianness != ElfEndianness::Little {
            // only little endian is supported
            return Err(ElfError::ElfEndiannessError(endianness as u8));
        }

        let mut header_data = &data[16..];
        let elf_type: ElfType = header_data.read_u16::<LE>()?.try_into()?;
        let machine: ElfMachine = header_data.read_u16::<LE>()?.try_into()?;
        let version: ElfVersion = header_data.read_u32::<LE>()?.try_into()?;
        let entry: Elf64Addr = header_data.read_u64::<LE>()?;

        let program_header_table_offset: Elf64Off = header_data.read_u64::<LE>()?;
        let section_header_table_offset: Elf64Off = header_data.read_u64::<LE>()?;
        let flags: Elf64Word = header_data.read_u32::<LE>()?;

        let program_header_size: Elf64Half = header_data.read_u16::<LE>()?;
        let program_header_count: Elf64Half = header_data.read_u16::<LE>()?;
        let section_header_size: Elf64Half = header_data.read_u16::<LE>()?;
        let section_header_count: Elf64Half = header_data.read_u16::<LE>()?;
        let header_name_section_idx: Elf64Half = header_data.read_u16::<LE>()?;

        Ok(Elf64Header {
            class,
            endianness,
            version,
            elf_type,
            machine,
            entry,
            program_header_table_offset,
            section_header_table_offset,
            flags,
            program_header_size,
            program_header_count,
            section_header_size,
            section_header_count,
            header_name_section_idx,
        })
    }

    fn size() -> usize {
        16 // e_ident
        + size_of::<Elf64Half>() // e_type
        + size_of::<Elf64Half>() // e_machine
        + size_of::<Elf64Word>() // e_version
        + size_of::<Elf64Addr>() // e_entry
        + size_of::<Elf64Off>() // e_phoff
        + size_of::<Elf64Off>() // e_shoff
        + size_of::<Elf64Word>() // e_flags
        + size_of::<Elf64Half>() // e_ehsize
        + size_of::<Elf64Half>() // e_phentsize
        + size_of::<Elf64Half>() // e_phnum
        + size_of::<Elf64Half>() // e_shentsize
        + size_of::<Elf64Half>() // e_shnum
        + size_of::<Elf64Half>() // e_shstrndx
    }
}

/*
typedef struct {
    Elf64_Word	sh_name;
    Elf64_Word	sh_type;
    Elf64_Xword	sh_flags;
    Elf64_Addr	sh_addr;
    Elf64_Off	sh_offset;
    Elf64_Xword	sh_size;
    Elf64_Word	sh_link;
    Elf64_Word	sh_info;
    Elf64_Xword	sh_addralign;
    Elf64_Xword	sh_entsize;
} Elf64_Shdr;
*/
pub struct Elf64SectionHeader {
    name_offset: Elf64Word,
    section_type: ElfSectionType,
    flags: ElfSectionFlags,
    addr: Elf64Addr,
    offset: Elf64Off,
    size: Elf64Xword,
    link: Elf64Word,
    info: Elf64Word,
    addr_align: Elf64Xword,
    entry_size: Elf64Xword,
}

impl Elf64SectionHeader {
    fn parse_section_header(data: &[u8]) -> Result<Self, ElfError> {
        let mut data = data;
        let name_offset: Elf64Word = data.read_u32::<LE>()?;
        let section_type: ElfSectionType = data.read_u32::<LE>()?.try_into()?;
        let flags = data.read_u64::<LE>()?;
        let flags: ElfSectionFlags =
            ElfSectionFlags::from_bits(flags).ok_or(ElfError::ElfSectionFlagsError(flags))?;

        let addr: Elf64Addr = data.read_u64::<LE>()?;
        let offset: Elf64Off = data.read_u64::<LE>()?;
        let size: Elf64Xword = data.read_u64::<LE>()?;
        let link: Elf64Word = data.read_u32::<LE>()?;
        let info: Elf64Word = data.read_u32::<LE>()?;
        let addr_align: Elf64Xword = data.read_u64::<LE>()?;
        let entry_size: Elf64Xword = data.read_u64::<LE>()?;

        Ok(Self {
            name_offset,
            section_type,
            flags,
            addr,
            offset,
            size,
            link,
            info,
            addr_align,
            entry_size,
        })
    }
}

/*
typedef struct {
    Elf64_Word	st_name;
    unsigned char	st_info;
    unsigned char	st_other;
    Elf64_Half	st_shndx;
    Elf64_Addr	st_value;
    Elf64_Xword	st_size;
} Elf64_Sym;
*/
pub struct Elf64Symbol {
    name_offset: Elf64Word,
    symbol_type: ElfSymbolType,
    binding: ElfSymbolBinding,
    visibility: ElfSymbolVisibility,
    size: Option<Elf64Xword>,
    value: Elf64Addr,
    section_header_idx: Elf64Half,
}

pub struct Elf64RelocationEntry {
    offset: Elf64Addr,
    symbol_index: u32,
    relocation_type: u32,
    addend: Option<Elf64Sxword>,
}

/*
typedef struct {
    Elf64_Word	p_type;
    Elf64_Word	p_flags;
    Elf64_Off	p_offset;
    Elf64_Addr	p_vaddr;
    Elf64_Addr	p_paddr;
    Elf64_Xword	p_filesz;
    Elf64_Xword	p_memsz;
    Elf64_Xword	p_align;
} Elf64_Phdr;
*/

pub struct Elf64ProgramHeader {
    segment_type: ElfSegmentType,
    flags: ElfSegmentPermissions,
    offset: Elf64Off,
    vaddr: Elf64Addr,
    paddr: Elf64Addr,
    file_size: Elf64Xword,
    mem_size: Elf64Xword,
    align: Elf64Xword,
}

pub struct StringTableView {}

pub struct StringTableSection {
    strings: Vec<(String, usize)>,
}

pub struct SymbolTableSection {
    entries: Vec<Elf64Symbol>,
}

// representation of ELF section. Owns data associated w/ section
pub enum Section {
    ProgBits {},
    StringTable(StringTableSection),
    SymbolTable(SymbolTableSection),
}

// representation of ELF section. Does not own data associated w/ section
pub enum SectionView {
    StringTable(StringTableView),
}

pub struct Elf {
    header: Elf64Header,
    section_headers: Vec<Elf64SectionHeader>,
}

impl Elf {}

pub struct ReadElf<'buffer> {
    elf: Elf,
    data: &'buffer [u8],
    section_data: Vec<&'buffer [u8]>,
}

impl<'buffer> ReadElf<'buffer> {
    pub fn new(buffer: &'buffer [u8]) -> Result<Self, ElfError> {
        let elf_header = Elf64Header::parse_elf_header(buffer)?;
        let mut section_headers: Vec<Elf64SectionHeader> = Vec::new();
        let mut section_data: Vec<&'buffer [u8]> = Vec::new();
        if elf_header.section_header_table_offset != 0 {
            for k in 0..(elf_header.section_header_count as usize) {
                let index = elf_header.program_header_table_offset as usize
                    + k * (elf_header.section_header_size as usize);
                let section_header_bytes = &buffer[index..];
                let section_header =
                    Elf64SectionHeader::parse_section_header(section_header_bytes)?;
                let section_data_bytes = &buffer[(section_header.offset) as usize..];

                section_headers.push(section_header);
                section_data.push(section_data_bytes);
            }
        }

        let elf = Elf {
            header: elf_header,
            section_headers,
        };

        Ok(Self {
            elf,
            data: buffer,
            section_data: section_data,
        })
    }
}

pub struct WriteElf {
    elf: Elf,
}
