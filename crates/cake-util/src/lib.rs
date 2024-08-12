use std::ops::Mul;

// basic helper for doing range arithmetic
#[derive(Clone, Copy, Debug)]
pub enum RangeUInt {
    Finite(u32),
    Infinite,
}

impl Mul for RangeUInt {
    type Output = RangeUInt;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (RangeUInt::Finite(a), RangeUInt::Finite(b)) => RangeUInt::Finite(a * b),
            _ => RangeUInt::Infinite,
        }
    }
}
