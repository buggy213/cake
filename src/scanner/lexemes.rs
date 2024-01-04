// Expected usage: implement this trait for a Enum whose variants encode all tokens
// one expects to see given a language's microsyntax

// 3 representations for each token: Enum variant, string name, and numeric id
// Enum variant is canonical, but the other two might be more convenient for 
// automatic / generated code to use
// Should be clone + copy (plain data type)
pub trait LexemeSet: Clone + Copy {
    fn from_name(name: &str) -> Self;
    fn from_id(id: u32) -> Self;
    fn to_name(self) -> &'static str;
    fn to_id(self) -> u32;

    fn next(self) -> Option<Self>;
    fn iter() -> LexemeIterator<Self> {
        LexemeIterator { state: Some(Self::from_id(0)) }
    }

    fn pattern(self) -> &'static str;
}
pub struct LexemeIterator<T: LexemeSet> {
    state: Option<T>
}

impl<T: LexemeSet> Iterator for LexemeIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let tmp = self.state;
        self.state = match self.state {
            Some(s) => s.next(),
            None => None,
        };
        tmp
    }
}