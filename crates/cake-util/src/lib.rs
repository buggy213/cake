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

#[macro_export]
macro_rules! make_type_idx {
    ($type_idx_name:tt, $type_name:tt) => {
        // TODO: consider newtyping Vec and adding push which returns type_idx
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub(crate) struct $type_idx_name(u32);

        impl $type_idx_name {
            pub(crate) fn from_push(vec: &mut Vec<$type_name>, val: $type_name) -> $type_idx_name {
                let idx = $type_idx_name(vec.len() as u32);
                vec.push(val);
                idx
            }
        }

        impl std::ops::Index<$type_idx_name> for [$type_name] {
            type Output = $type_name;

            fn index(&self, index: $type_idx_name) -> &Self::Output {
                &self[index.0 as usize]
            }
        }

        impl std::ops::IndexMut<$type_idx_name> for [$type_name] {
            fn index_mut(&mut self, index: $type_idx_name) -> &mut Self::Output {
                &mut self[index.0 as usize]
            }
        }

        impl std::ops::Index<$type_idx_name> for Vec<$type_name> {
            type Output = $type_name;

            fn index(&self, index: $type_idx_name) -> &Self::Output {
                self.as_slice().index(index)
            }
        }

        impl std::ops::IndexMut<$type_idx_name> for Vec<$type_name> {
            fn index_mut(&mut self, index: $type_idx_name) -> &mut Self::Output {
                self.as_mut_slice().index_mut(index)
            }
        }
    };
}
