use crate::scanner::lexemes::{LexemeSet, LexemeIterator};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
#[repr(u32)]
pub enum Expressions {
    LParen,
    RParen,
    Plus,
    Minus,
    Times,
    Divide,
    Number,
}

impl LexemeSet for Expressions {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "LParen" => Some(Expressions::LParen),
            "RParen" => Some(Expressions::RParen),
            "Plus" => Some(Expressions::Plus),
            "Minus" => Some(Expressions::Minus),
            "Times" => Some(Expressions::Times),
            "Divide" => Some(Expressions::Divide),
            "Number" => Some(Expressions::Number),
            _ => None
        }
    }

    fn from_id(id: u32) -> Option<Self> {
        if id >= Self::size() { return None; }
        unsafe { Some(std::mem::transmute::<u32, Self>(id)) }
    }

    fn to_name(self) -> &'static str {
        match self {
            Expressions::LParen => "LParen",
            Expressions::RParen => "RParen",
            Expressions::Plus => "Plus",
            Expressions::Minus => "Minus",
            Expressions::Times => "Times",
            Expressions::Divide => "Divide",
            Expressions::Number => "Number",
        }
    }

    fn to_id(self) -> u32 {
        self as u32
    }

    fn pattern(self) -> &'static str {
        match self {
            Expressions::LParen => "\\(",
            Expressions::RParen => "\\)",
            Expressions::Plus => "\\+",
            Expressions::Minus => "\\-",
            Expressions::Times => "\\*",
            Expressions::Divide => "\\/",
            Expressions::Number => "[0-9]+",
        }
    }

    fn next(self) -> Option<Self> {
        if self.to_id() >= 7 - 1 { None } else { Self::from_id(self.to_id() + 1) }
    }

    fn size() -> u32 {
        7
    }
}