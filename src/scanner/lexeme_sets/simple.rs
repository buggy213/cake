use crate::scanner::lexemes::{LexemeSet, LexemeIterator};

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Simple {
    Identifier,
    Whitespace,
}

impl LexemeSet for Simple {
    fn from_name(name: &str) -> Self {
        match name {
            "Identifier" => Simple::Identifier,
            "Whitespace" => Simple::Whitespace,
            _ => panic!("unrecognized variant")
        }
    }

    fn from_id(id: u32) -> Self {
        unsafe { std::mem::transmute::<u32, Self>(id) }
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
        if self.to_id() >= 2 - 1 { None } else { Some(Self::from_id(self.to_id() + 1)) }
    }
}