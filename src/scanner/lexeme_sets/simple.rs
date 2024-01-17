use crate::scanner::lexemes::{LexemeSet, LexemeIterator};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u32)]
pub enum Simple {
    Identifier,
    Whitespace,
}

impl LexemeSet for Simple {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "Identifier" => Some(Simple::Identifier),
            "Whitespace" => Some(Simple::Whitespace),
            _ => None
        }
    }

    fn from_id(id: u32) -> Option<Self> {
        if id >= Self::size() { return None; }
        unsafe { Some(std::mem::transmute::<u32, Self>(id)) }
    }

    fn to_name(self) -> &'static str {
        match self {
            Simple::Identifier => "Identifier",
            Simple::Whitespace => "Whitespace",
        }
    }

    fn to_id(self) -> u32 {
        self as u32
    }

    fn pattern(self) -> &'static str {
        match self {
            Simple::Identifier => "[_a-zA-Z][_a-zA-Z0-9]*",
            Simple::Whitespace => "[ \\t\\n\\v\\f]",
        }
    }

    fn next(self) -> Option<Self> {
        if self.to_id() >= 2 - 1 { None } else { Self::from_id(self.to_id() + 1) }
    }

    fn size() -> u32 {
        2
    }
}