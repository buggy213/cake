use crate::scanner::lexemes::{LexemeSet, LexemeIterator};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
#[repr(u32)]
pub enum Ebnf {
    DoubleNewline,
    Nonterminal,
    Define,
    Terminal,
    Whitespace,
    Bar,
    Plus,
    Star,
    Question,
    LParen,
    RParen,
}

impl LexemeSet for Ebnf {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "DoubleNewline" => Some(Ebnf::DoubleNewline),
            "Nonterminal" => Some(Ebnf::Nonterminal),
            "Define" => Some(Ebnf::Define),
            "Terminal" => Some(Ebnf::Terminal),
            "Whitespace" => Some(Ebnf::Whitespace),
            "Bar" => Some(Ebnf::Bar),
            "Plus" => Some(Ebnf::Plus),
            "Star" => Some(Ebnf::Star),
            "Question" => Some(Ebnf::Question),
            "LParen" => Some(Ebnf::LParen),
            "RParen" => Some(Ebnf::RParen),
            _ => None
        }
    }

    fn from_id(id: u32) -> Option<Self> {
        if id >= Self::size() { return None; }
        unsafe { Some(std::mem::transmute::<u32, Self>(id)) }
    }

    fn to_name(self) -> &'static str {
        match self {
            Ebnf::DoubleNewline => "DoubleNewline",
            Ebnf::Nonterminal => "Nonterminal",
            Ebnf::Define => "Define",
            Ebnf::Terminal => "Terminal",
            Ebnf::Whitespace => "Whitespace",
            Ebnf::Bar => "Bar",
            Ebnf::Plus => "Plus",
            Ebnf::Star => "Star",
            Ebnf::Question => "Question",
            Ebnf::LParen => "LParen",
            Ebnf::RParen => "RParen",
        }
    }

    fn to_id(self) -> u32 {
        self as u32
    }

    fn pattern(self) -> &'static str {
        match self {
            Ebnf::DoubleNewline => "\\n\\n|\\r\\n\\r\\n",
            Ebnf::Nonterminal => "<[a-z\\-]+>",
            Ebnf::Define => "::=",
            Ebnf::Terminal => "[A-Za-z\\-]+",
            Ebnf::Whitespace => "[ \\t\\n\\v\\f]",
            Ebnf::Bar => "\\|",
            Ebnf::Plus => "\\+",
            Ebnf::Star => "\\*",
            Ebnf::Question => "\\?",
            Ebnf::LParen => "\\(",
            Ebnf::RParen => "\\)",
        }
    }

    fn next(self) -> Option<Self> {
        if self.to_id() >= 11 - 1 { None } else { Self::from_id(self.to_id() + 1) }
    }

    fn size() -> u32 {
        11
    }
}