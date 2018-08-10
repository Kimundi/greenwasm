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
    fn rsigned(i: Self::S) -> Self { i as Self }
    fn n() -> Self { ::std::mem::size_of::<Self>() as Self * 8 }

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
        i1.checked_div(i2).into()
    }
    fn idiv_s(i1: Self, i2: Self) -> Partial<Self> {
        let j1 = Self::signed(i1);
        let j2 = Self::signed(i2);

        j1.checked_div(j2).map(Self::rsigned).into()
    }
    fn irem_u(i1: Self, i2: Self) -> Partial<Self> {
        i1.checked_rem(i2).into()
    }
    fn irem_s(i1: Self, i2: Self) -> Partial<Self> {
        let j1 = Self::signed(i1);
        let j2 = Self::signed(i2);

        j1.checked_rem(j2).map(Self::rsigned).into()
    }
    fn iand(i1: Self, i2: Self) -> Self {
        i1 & i2
    }
    fn ior(i1: Self, i2: Self) -> Self {
        i1 | i2
    }
    fn ixor(i1: Self, i2: Self) -> Self {
        i1 ^ i2
    }
    fn ishl(i1: Self, i2: Self) -> Self {
        let k = i2 % Self::n();
        i1 << k
    }
    fn ishr_u(i1: Self, i2: Self) -> Self {
        let k = i2 % Self::n();
        i1 >> k
    }
    fn ishr_s(i1: Self, i2: Self) -> Self {
        let k = i2 % Self::n();
        Self::rsigned(Self::signed(i1) >> Self::signed(k))
    }
    fn irotl(i1: Self, i2: Self) -> Self {
        let k = i2 % Self::n();
        i1.rotate_left(k as u32)
    }
    fn irotr(i1: Self, i2: Self) -> Self {
        let k = i2 % Self::n();
        i1.rotate_right(k as u32)
    }
    fn iclz(i: Self) -> Self {
        i.leading_zeros() as Self
    }
    fn ipopcnt(i: Self) -> Self {
        i.count_ones() as Self
    }
    fn ieqz(i: Self) -> Self {
        Self::bool(i == 0)
    }
    fn ieq(i1: Self, i2: Self) -> Self {
        Self::bool(i1 == i2)
    }
    fn ine(i1: Self, i2: Self) -> Self {
        Self::bool(i1 != i2)
    }
    fn ilt_u(i1: Self, i2: Self) -> Self {
        Self::bool(i1 < i2)
    }
    fn ilt_s(i1: Self, i2: Self) -> Self {
        Self::bool(Self::signed(i1) < Self::signed(i2))
    }
    fn igt_u(i1: Self, i2: Self) -> Self {
        Self::bool(i1 > i2)
    }
    fn igt_s(i1: Self, i2: Self) -> Self {
        Self::bool(Self::signed(i1) > Self::signed(i2))
    }
    fn ile_u(i1: Self, i2: Self) -> Self {
        Self::bool(i1 <= i2)
    }
    fn ile_s(i1: Self, i2: Self) -> Self {
        Self::bool(Self::signed(i1) <= Self::signed(i2))
    }
    fn ige_u(i1: Self, i2: Self) -> Self {
        Self::bool(i1 >= i2)
    }
    fn ige_s(i1: Self, i2: Self) -> Self {
        Self::bool(Self::signed(i1) >= Self::signed(i2))
    }
}
