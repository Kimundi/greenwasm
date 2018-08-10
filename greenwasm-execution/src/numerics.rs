use structure::types::*;

/*
struct N32;
struct N64;

trait Numerics {
    type S;
    type I;
    type U;
    type F;
}

impl Numerics for N32 {
    type S = S32;
    type I = I32;
    type U = U32;
    type F = F32;
}

impl Numerics for N64 {
    type S = S64;
    type I = I64;
    type U = U64;
    type F = F64;
}
*/

macro_rules! define_trait {
    (
        trait $traitname:ident for {$implty:ty, $($t:tt)*};

        $(
            type $at:ident = {$atimplty:ty, $($u:tt)*};
        )*

        $(
            fn $name:ident($($arg:ident : $argty:ty),*) -> $rty:ty $b:block
        )*
    ) => {
        impl $traitname for $implty {
            $(
                type $at = $atimplty;
            )*

            $(
                #[inline(always)]
                fn $name($($arg : $argty),*) -> $rty $b
            )*
        }
        define_trait! {
            trait $traitname for {$($t)*};
            $(
                type $at = {$($u)*};
            )*

            $(
                fn $name($($arg : $argty),*) -> $rty $b
            )*
        }
    };
    (
        trait $traitname:ident for {};

        $(
            type $at:ident = {};
        )*

        $(
            fn $name:ident($($arg:ident : $argty:ty),*) -> $rty:ty $b:block
        )*
    ) => {
        pub trait $traitname: Sized {
            $(
                type $at;
            )*

            $(
                fn $name($($arg : $argty),*) -> $rty;
            )*
        }
    }
}

pub enum Partial<T> {
    Val(T),
    None,
}
impl<T> From<Option<T>> for Partial<T> {
    fn from(o: Option<T>) -> Self {
        o.map(Partial::Val).unwrap_or(Partial::None)
    }
}

define_trait! {
    trait IntegerOperations for { I32, I64, };

    type S = { S32, S64, };
    type U = { U32, U64, };

    fn signed(i: Self) -> Self::S { i as Self::S }
    fn unsigned(i: Self) -> Self::U { i as Self::U }

    fn rsigned(i: Self::S) -> Self { i as Self }
    fn runsigned(i: Self::U) -> Self { i as Self }

    fn bool(v: bool) -> Self { if v { 1 } else { 0 } }
    fn iadd(i1: Self, i2: Self) -> Self {
        i1.wrapping_add(i2)
    }
    fn isub(i1: Self, i2: Self) -> Self {
        i1.wrapping_sub(i2)
    }
    fn imul(i1: Self, i2: Self) -> Self {
        i1.wrapping_mul(i2)
    }
    fn idiv_u(i1: Self, i2: Self) -> Partial<Self> {
        Self::unsigned(i1).checked_div(Self::unsigned(i2)).into()
    }
    fn idiv_s(i1: Self, i2: Self) -> Partial<Self> {
        let j1 = Self::signed(i1);
        let j2 = Self::signed(i2);

        j1.checked_div(j2).map(Self::rsigned).into()
    }
    fn irem_u(i1: Self, i2: Self) -> Partial<Self> {
        Self::unsigned(i1).checked_rem(Self::unsigned(i2)).into()
    }
    fn irem_s(i1: Self, i2: Self) -> Partial<Self> {
        let j1 = Self::signed(i1);
        let j2 = Self::signed(i2);

        j1.checked_rem(j2).map(Self::rsigned).into()
    }
}
