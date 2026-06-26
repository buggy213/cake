//! Cake outputs object files directly

use std::mem::size_of;

use bitflags::bitflags;
use byteorder::{LE, LittleEndian, ReadBytesExt, WriteBytesExt};
use thiserror::Error;

pub(crate) type Elf64Addr = u64;
pub(crate) type Elf64Off = u64;
pub(crate) type Elf64Half = u16;
pub(crate) type Elf64Word = u32;
pub(crate) type Elf64Sword = i32;
pub(crate) type Elf64Xword = u64;
pub(crate) type Elf64Sxword = u64;
pub(crate) type Uchar = u8;

bitflags! {
    pub(crate) struct ElfSectionFlags: Elf64Xword {
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

#[derive(Debug, Clone, Copy)]
pub(crate) enum ElfSymbolType {
    NoType,
    Object,
    Func,
    Section,
    File,
}


#[derive(Debug, Clone, Copy)]
pub(crate) enum ElfSymbolBinding {
    Local,
    Global,
    Weak,
}

fn symbol_info(symbol_type: ElfSymbolType, binding: ElfSymbolBinding) -> u8 {
    let binding = match binding {
        ElfSymbolBinding::Local => 0u8,
        ElfSymbolBinding::Global => 1u8,
        ElfSymbolBinding::Weak => 2u8,
    };

    let symbol_type = match symbol_type {
        ElfSymbolType::NoType => 0u8,
        ElfSymbolType::Object => 1u8,
        ElfSymbolType::Func => 2u8,
        ElfSymbolType::Section => 3u8,
        ElfSymbolType::File => 4u8,
    };

    (binding << 4) | symbol_type
}

mod consts {
    use crate::elf::Section;

    pub(super) const HEADER_SIZE: u16 = 64;
    
    // everything aligned to 16 bytes for simplicity
    pub(super) const SECTION_ALIGN: usize = 16;

    pub(super) const PROGBITS: u32 = 1;
    pub(super) const SYMTAB: u32 = 2;
    pub(super) const STRTAB: u32 = 3;
    pub(super) const RELA: u32 = 4;
    pub(super) const NOBITS: u32 = 8;

    pub(super) const SHN_UNDEF: u32 = 0;
    pub(super) const SHN_TEXT: u32 = 1;
    pub(super) const SHN_DATA: u32 = 2;
    pub(super) const SHN_SYMTAB: u32 = 6;
    pub(super) const SHN_STRTAB: u32 = 7;
    pub(super) fn section_index(section: Section) -> u32 {
        match section {
            Section::Text => 1,
            Section::Data => 2,
            Section::Bss => 3,
            Section::RelaText => 4,
            Section::RelaData => 5,
            Section::Symtab => 6,
            Section::Strtab => 7,
        }
    }

    pub(super) const RELA_ENTSIZE: u64 = 24;
    pub(super) const SYMTAB_ENTSIZE: u64 = 24;
    pub(super) fn entry_size(section: Section) -> u64 {
        match section {
            Section::RelaText => RELA_ENTSIZE,
            Section::RelaData => RELA_ENTSIZE,
            Section::Symtab => SYMTAB_ENTSIZE,
            Section::Text |
            Section::Data |
            Section::Bss |
            Section::Strtab => 0,
        }
    }
}

struct Elf {
    text: BasicSection,
    data: BasicSection,
    bss: EmptySection,
    rela_text: RelocationSection,
    rela_data: RelocationSection,
    symbol_table: SymbolTableSection,
    string_table: StringTableSection,
    section_names: StringTableSection,
    section_name_offsets: [StringTableOffset; 8],
}

impl Elf {
    pub(crate) fn new() -> Elf {
        let mut section_names = StringTableSection::new();
        let text_offset = section_names.add_string(".text");
        let data_offset = section_names.add_string(".data");
        let bss_offset = section_names.add_string(".bss");
        let rela_text_offset = section_names.add_string(".rela.text");
        let rela_data_offset = section_names.add_string(".rela.data");
        let symbol_table_offset = section_names.add_string(".symtab");
        let string_table_offset = section_names.add_string(".strtab");
        let section_names_offset = section_names.add_string(".shstrtab");


        Elf {
            text: BasicSection::new(),
            data: BasicSection::new(),
            bss: EmptySection::new(),
            rela_text: RelocationSection::new(),
            rela_data: RelocationSection::new(),
            symbol_table: SymbolTableSection::new(),
            string_table: StringTableSection::new(),
            section_names,

            section_name_offsets: [
                text_offset, 
                data_offset, 
                bss_offset, 
                rela_text_offset, 
                rela_data_offset, 
                symbol_table_offset, 
                string_table_offset, 
                section_names_offset
            ]
        }
    }

    pub(crate) fn add_text(&mut self, bytes: &[u8]) -> usize {
        self.text.write_bytes(bytes)
    }

    pub(crate) fn add_data(&mut self, bytes: &[u8]) -> usize {
        self.data.write_bytes(bytes)
    }

    pub(crate) fn add_bss(&mut self, count: usize) -> usize {
        self.bss.add_bytes(count)
    }

    pub(crate) fn define_symbol(
        &mut self, 
        name: &str,
        binding: ElfSymbolBinding,
        symbol_type: ElfSymbolType,
        section: Section,
        value: usize,
        size: usize
    ) -> SymbolTableIndex {
        let name = self.string_table.add_string(name);
        self.symbol_table.add_symbol(
            name, 
            binding, 
            symbol_type, 
            section, 
            value, 
            size
        )
    }

    pub(crate) fn reloc_text(
        &mut self,
        offset: usize,
        symtab_idx: SymbolTableIndex,
        reloc_type: RelocationType,
        addend: usize
    ) {
        self.rela_text.add_relocation(offset, symtab_idx, reloc_type, addend);
    }

    pub(crate) fn reloc_data(
        &mut self,
        offset: usize,
        symtab_idx: SymbolTableIndex,
        reloc_type: RelocationType,
        addend: usize
    ) {
        self.rela_data.add_relocation(offset, symtab_idx, reloc_type, addend);
    }

    pub(crate) fn write(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        use consts::*;

        // helpers for alignment
        fn align_to(size: usize, align: usize) -> usize {
            size.next_multiple_of(align)
        }

        // create ELF header
        let ident = [
            // magic
            0x7f, b'E', b'L', b'F',
            // 64-bit
            0x02,
            // little-endian
            0x01,
            // version
            0x01,
            // sysv abi
            0x00,
            // abi version 0
            0x00,
            // padding
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            // size of ident
            0x10,
        ];

        writer.write_all(&ident)?;

        // relocatable object
        writer.write_u16::<LittleEndian>(0x0001)?;
        // x86_64
        writer.write_u16::<LittleEndian>(0x003E)?;
        // version
        writer.write_u32::<LittleEndian>(0x00000001)?;
        // no entry point
        writer.write_u64::<LittleEndian>(0x0)?;
        // no program header
        writer.write_u64::<LittleEndian>(0x0)?;
        
        let section_header_offset = 
            HEADER_SIZE as usize +
            align_to(self.text.size(), SECTION_ALIGN) +
            align_to(self.data.size(), SECTION_ALIGN) + 
            align_to(self.rela_text.size(), SECTION_ALIGN) +
            align_to(self.rela_data.size(), SECTION_ALIGN) + 
            align_to(self.symbol_table.size(), SECTION_ALIGN) +
            align_to(self.string_table.size(), SECTION_ALIGN) + 
            align_to(self.section_names.size(), SECTION_ALIGN);

        writer.write_u64::<LittleEndian>(section_header_offset as u64)?;

        // no flags
        writer.write_u32::<LittleEndian>(0x0)?;

        writer.write_u16::<LittleEndian>(HEADER_SIZE)?;

        const PHENT_SIZE: u16 = 56;
        writer.write_u16::<LittleEndian>(PHENT_SIZE)?;
        // no program header entries
        writer.write_u16::<LittleEndian>(0x0)?;

        const SHENT_SIZE: u16 = 64;
        writer.write_u16::<LittleEndian>(SHENT_SIZE)?;
        // fixed 8 sections
        writer.write_u16::<LittleEndian>(8)?;
        // header names always at index 8 (index 0 is SHN_UNDEF)
        writer.write_u16::<LittleEndian>(8)?;

        fn write_padding(size: usize, writer: &mut impl std::io::Write) -> std::io::Result<()> {
            let zeros = [0x00_u8; 16];
            let pad_bytes = size.next_multiple_of(SECTION_ALIGN) - size;
            writer.write_all(&zeros[..pad_bytes])?;

            Ok(())
        };

        let text_offset = HEADER_SIZE as usize;
        let text_size = self.text.size().next_multiple_of(SECTION_ALIGN);
        self.text.write(writer)?;
        write_padding(self.text.size(), writer)?;

        let data_offset = text_offset + text_size;
        let data_size = self.data.size().next_multiple_of(SECTION_ALIGN);
        self.data.write(writer)?;
        write_padding(self.data.size(), writer)?;
        
        let bss_size = self.bss.size();

        let rela_text_offset = data_offset + data_size;
        let rela_text_size = self.rela_data.size().next_multiple_of(SECTION_ALIGN);
        self.rela_text.write(writer)?;
        write_padding(self.rela_text.size(), writer)?;
        
        let rela_data_offset = rela_text_offset + rela_text_size;
        let rela_data_size = self.rela_data.size().next_multiple_of(SECTION_ALIGN);
        self.rela_data.write(writer)?;
        write_padding(self.rela_data.size(), writer)?;
        
        let symbol_table_offset = rela_data_offset + rela_data_size;
        let symbol_table_size = self.symbol_table.size().next_multiple_of(SECTION_ALIGN);
        self.symbol_table.write(writer)?;
        write_padding(self.symbol_table.size(), writer)?;
        
        let string_table_offset = symbol_table_offset + symbol_table_size;
        let string_table_size = self.string_table.size().next_multiple_of(SECTION_ALIGN);
        self.string_table.write(writer)?;
        write_padding(self.string_table.size(), writer)?;
        
        let section_names_offset = string_table_offset + string_table_size;
        let section_names_size = self.section_names.size().next_multiple_of(SECTION_ALIGN);
        self.section_names.write(writer)?;
        write_padding(self.section_names.size(), writer)?;

        fn write_section_header(
            writer: &mut impl std::io::Write, 
            name: StringTableOffset,
            section: Section,
            offset: usize,
            size: usize,
            
            // only for symtab
            local_symbols: usize

        ) -> std::io::Result<()> {
            writer.write_u32::<LittleEndian>(name.0 as u32)?;

            let section_type = match section {
                Section::Text => PROGBITS,
                Section::Data => PROGBITS,
                Section::Bss => NOBITS,
                Section::RelaText => RELA,
                Section::RelaData => RELA,
                Section::Symtab => SYMTAB,
                Section::Strtab => STRTAB,
            };
            writer.write_u32::<LittleEndian>(section_type)?;

            let section_flags = match section {
                Section::Text => ElfSectionFlags::ALLOC | ElfSectionFlags::EXECINSTR,
                Section::Data => ElfSectionFlags::ALLOC | ElfSectionFlags::WRITE,
                Section::Bss => ElfSectionFlags::ALLOC | ElfSectionFlags::WRITE,
                Section::RelaText |
                Section::RelaData |
                Section::Symtab |
                Section::Strtab => ElfSectionFlags::empty(),
            };
            writer.write_u64::<LittleEndian>(section_flags.bits())?;
            
            // no address
            writer.write_u64::<LittleEndian>(0x0)?;
            // offset in file
            writer.write_u64::<LittleEndian>(offset as u64)?;
            // size in file
            writer.write_u64::<LittleEndian>(size as u64)?;

            // link
            let section_link = match section {
                Section::RelaText => SHN_SYMTAB,
                Section::RelaData => SHN_SYMTAB,
                Section::Symtab => SHN_STRTAB,
                _ => SHN_UNDEF
            };
            writer.write_u32::<LittleEndian>(section_link)?;
            // info
            let section_info = match section {
                Section::RelaText => SHN_TEXT,
                Section::RelaData => SHN_DATA,
                Section::Symtab => local_symbols as u32,
                _ => 0
            };

            writer.write_u32::<LittleEndian>(section_info)?;
            // addralign
            writer.write_u64::<LittleEndian>(SECTION_ALIGN as u64)?;
            // entsize
            let section_entsize = consts::entry_size(section);
            writer.write_u64::<LittleEndian>(section_entsize)?;

            Ok(())
        }

        // write section headers
        write_section_header(writer, self.section_name_offsets[0], Section::Text, text_offset, text_size, 0)?;
        write_section_header(writer, self.section_name_offsets[1], Section::Data, data_offset, data_size, 0)?;
        write_section_header(writer, self.section_name_offsets[2], Section::Bss, rela_text_offset, bss_size, 0)?;
        write_section_header(writer, self.section_name_offsets[3], Section::RelaText, rela_text_offset, rela_text_size, 0)?;
        write_section_header(writer, self.section_name_offsets[4], Section::RelaData, rela_data_offset, rela_data_size, 0)?;
        write_section_header(writer, self.section_name_offsets[5], Section::Symtab, symbol_table_offset, symbol_table_size, 0)?;
        write_section_header(writer, self.section_name_offsets[6], Section::Strtab, string_table_offset, string_table_size, 0)?;
        write_section_header(writer, self.section_name_offsets[7], Section::Strtab, symbol_table_offset, symbol_table_size, 0)?;

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
enum Section {
    Text,
    Data,
    Bss,
    RelaText,
    RelaData,
    Symtab,
    Strtab
}

struct BasicSection {
    data: Vec<u8>
}

impl BasicSection {
    fn new() -> BasicSection {
        BasicSection { data: Vec::new() }
    }

    fn write_bytes(&mut self, bytes: &[u8]) -> usize {
        let offset = self.data.len();
        self.data.extend(bytes.iter());
        offset
    }

    fn size(&self) -> usize {
        self.data.len()
    }

    fn write(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        writer.write_all(&self.data)
    }
}

struct EmptySection {
    capacity: usize
}

impl EmptySection {
    fn new() -> EmptySection {
        EmptySection { capacity: 0 }
    }

    fn add_bytes(&mut self, count: usize) -> usize {
        let offset = self.capacity;
        self.capacity += count;
        offset
    }
    
    fn size(&self) -> usize {
        self.capacity
    }
}

#[allow(nonstandard_style)]
#[derive(Clone, Copy)]
enum RelocationType {
    R_AMD64_NONE,
    R_AMD64_64
}

impl From<RelocationType> for u32 {
    fn from(value: RelocationType) -> Self {
        match value {
            RelocationType::R_AMD64_NONE => 0,
            RelocationType::R_AMD64_64 => 1,
        }
    }
}

#[derive(Clone, Copy)]
struct Relocation {
    offset: usize,
    symtab_idx: SymbolTableIndex,
    reloc_type: RelocationType,
    addend: usize
}
struct RelocationSection {
    relocations: Vec<Relocation>
}

impl RelocationSection {
    fn new() -> RelocationSection {
        RelocationSection { relocations: Vec::new() }
    }

    fn add_relocation(
        &mut self, 
        offset: usize, 
        symtab_idx: SymbolTableIndex, 
        reloc_type: RelocationType, 
        addend: usize
    ) {
        let reloc = Relocation {
            offset,
            symtab_idx,
            reloc_type,
            addend,
        };
        self.relocations.push(reloc);
    }

    fn size(&self) -> usize {
        consts::RELA_ENTSIZE as usize * self.relocations.len()
    }

    fn write(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        for rela_entry in &self.relocations {
            writer.write_u64::<LittleEndian>(rela_entry.offset as u64)?;
            writer.write_u32::<LittleEndian>(rela_entry.reloc_type.into())?;
            writer.write_u32::<LittleEndian>(rela_entry.symtab_idx.0)?;
            writer.write_u64::<LittleEndian>(rela_entry.addend as u64)?;
        }

        Ok(())
    }
}

struct StringTableSection {
    data: Vec<u8>
}

#[derive(Clone, Copy)]
struct StringTableOffset(usize);

impl StringTableSection {
    fn new() -> StringTableSection {
        StringTableSection { data: vec![0x00] }
    }

    fn add_string(&mut self, string: &str) -> StringTableOffset {
        let offset = StringTableOffset(self.data.len());
        self.data.extend(string.bytes());
        self.data.push(0x00);

        offset
    }

    fn size(&self) -> usize {
        self.data.len()
    }

    fn write(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        writer.write_all(&self.data)
    }
}

struct Symbol {
    name: StringTableOffset,
    binding: ElfSymbolBinding,
    symbol_type: ElfSymbolType,
    section: Section,
    value: usize,
    size: usize,
}
struct SymbolTableSection {
    symbols: Vec<Symbol>
}

#[derive(Clone, Copy)]
struct SymbolTableIndex(u32);

impl SymbolTableSection {
    fn new() -> SymbolTableSection {
        SymbolTableSection { symbols: Vec::new() }
    }

    fn add_symbol(
        &mut self, 
        name: StringTableOffset, 
        binding: ElfSymbolBinding, 
        symbol_type: ElfSymbolType, 
        section: Section, 
        value: usize, 
        size: usize
    ) -> SymbolTableIndex {
        let idx = SymbolTableIndex(self.symbols.len() as u32);
        let symbol = Symbol {
            name,
            binding,
            symbol_type,
            section,
            value,
            size,
        };
        self.symbols.push(symbol);
        idx
    }

    fn size(&self) -> usize {
        consts::SYMTAB_ENTSIZE as usize * self.symbols.len()
    }

    fn write(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        for symbol in &self.symbols {
            writer.write_u32::<LittleEndian>(symbol.name.0 as u32)?;
            
            let info = symbol_info(symbol.symbol_type, symbol.binding);
            writer.write_u8(info)?;
            writer.write_u8(0)?;

            let section_index = consts::section_index(symbol.section);
            writer.write_u16::<LittleEndian>(section_index as u16)?;
            writer.write_u64::<LittleEndian>(symbol.value as u64)?;
            writer.write_u64::<LittleEndian>(symbol.size as u64)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_create_object_file() {
        let mut elf = Elf::new();
        elf.
    }
}