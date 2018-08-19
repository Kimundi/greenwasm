use structure::types::*;

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
    fn ictz(i: Self) -> Self {
        i.trailing_zeros() as Self
    }
    fn ipopcnt(i: Self) -> Self {
        i.count_ones() as Self
    }
    fn ieqz(i: Self) -> I32 {
        I32::bool(i == 0)
    }
    fn ieq(i1: Self, i2: Self) -> I32 {
        I32::bool(i1 == i2)
    }
    fn ine(i1: Self, i2: Self) -> I32 {
        I32::bool(i1 != i2)
    }
    fn ilt_u(i1: Self, i2: Self) -> I32 {
        I32::bool(i1 < i2)
    }
    fn ilt_s(i1: Self, i2: Self) -> I32 {
        I32::bool(Self::signed(i1) < Self::signed(i2))
    }
    fn igt_u(i1: Self, i2: Self) -> I32 {
        I32::bool(i1 > i2)
    }
    fn igt_s(i1: Self, i2: Self) -> I32 {
        I32::bool(Self::signed(i1) > Self::signed(i2))
    }
    fn ile_u(i1: Self, i2: Self) -> I32 {
        I32::bool(i1 <= i2)
    }
    fn ile_s(i1: Self, i2: Self) -> I32 {
        I32::bool(Self::signed(i1) <= Self::signed(i2))
    }
    fn ige_u(i1: Self, i2: Self) -> I32 {
        I32::bool(i1 >= i2)
    }
    fn ige_s(i1: Self, i2: Self) -> I32 {
        I32::bool(Self::signed(i1) >= Self::signed(i2))
    }
}

trait Copysign {
    fn copysign(z1: Self, z2: Self) -> Self;
}
impl Copysign for f32 {
    fn copysign(z1: Self, z2: Self) -> Self {
        unsafe { std::intrinsics::copysignf32(z1, z2) }
    }
}
impl Copysign for f64 {
    fn copysign(z1: Self, z2: Self) -> Self {
        unsafe { std::intrinsics::copysignf64(z1, z2) }
    }
}

// NB: These operations hold under the assumptions from the WA spec
// regarding the rounding and tie breaking modes of the hardware.
//
// TODO: Figure out how to enforce/check/set them for Rust
define_trait! {
    trait FloatingPointOperations for { F32, F64, };

    fn fadd(z1: Self, z2: Self) -> Self {
        z1 + z2
    }
    fn fsub(z1: Self, z2: Self) -> Self {
        z1 - z2
    }
    fn fmul(z1: Self, z2: Self) -> Self {
        z1 * z2
    }
    fn fdiv(z1: Self, z2: Self) -> Self {
        z1 / z2
    }
    fn fmin(z1: Self, z2: Self) -> Self {
        z1.min(z2)
    }
    fn fmax(z1: Self, z2: Self) -> Self {
        z1.max(z2)
    }
    fn fcopysign(z1: Self, z2: Self) -> Self {
        Copysign::copysign(z1, z2)
    }
    fn fabs(z: Self) -> Self {
        z.abs()
    }
    fn fneg(z: Self) -> Self {
        -z
    }
    fn fsqrt(z: Self) -> Self {
        z.sqrt() // TODO: figure out how deterministic this is
    }
    fn fceil(z: Self) -> Self {
        z.ceil()
    }
    fn ffloor(z: Self) -> Self {
        z.floor()
    }
    fn ftrunc(z: Self) -> Self {
        z.trunc()
    }
    fn fnearest(z: Self) -> Self {
        z.round() // TODO: Depends on rounding modes
    }
    fn feq(z1: Self, z2: Self) -> I32 {
        I32::bool(z1 == z2)
    }
    fn fne(z1: Self, z2: Self) -> I32 {
        I32::bool(z1 != z2)
    }
    fn flt(z1: Self, z2: Self) -> I32 {
        I32::bool(z1 < z2)
    }
    fn fgt(z1: Self, z2: Self) -> I32 {
        I32::bool(z1 > z2)
    }
    fn fle(z1: Self, z2: Self) -> I32 {
        I32::bool(z1 <= z2)
    }
    fn fge(z1: Self, z2: Self) -> I32 {
        I32::bool(z1 >= z2)
    }
}

// Conversion

#[inline(always)]
pub fn extend_u(i: I32) -> I64 {
    i as I64
}
#[inline(always)]
pub fn extend_s(i: I32) -> I64 {
    I64::rsigned(I32::signed(i) as S64)
}
#[inline(always)]
pub fn wrap(i: I64) -> I32 {
    i as I32
}

// NB: Due to precision reasons, we need to be careful with what values we compare
const F32_MAX_I32_EXCLUSIVE: f32 = 2147483648_f32;
const F32_MIN_I32_INCLUSIVE: f32 = -2147483648_f32;
const F32_MAX_U32_EXCLUSIVE: f32 = 4294967296_f32;

const F32_MAX_I64_EXCLUSIVE: f32 = 9223372036854775808_f32;
const F32_MIN_I64_INCLUSIVE: f32 = -9223372036854775808_f32;
const F32_MAX_U64_EXCLUSIVE: f32 = 18446744073709551616_f32;

const F64_MAX_I32_EXCLUSIVE: f64 = 2147483648_f64;
const F64_MIN_I32_INCLUSIVE: f64 = -2147483648_f64;
const F64_MAX_U32_EXCLUSIVE: f64 = 4294967296_f64;

const F64_MAX_I64_EXCLUSIVE: f64 = 9223372036854775808_f64;
const F64_MIN_I64_INCLUSIVE: f64 = -9223372036854775808_f64;
const F64_MAX_U64_EXCLUSIVE: f64 = 18446744073709551616_f64;

#[inline(always)]
pub fn trunc_u_f32_i32(z: F32) -> Partial<I32> {
    if z.is_finite() {
        let z = z.trunc();
        if (0.0 <= z) && (z < F32_MAX_U32_EXCLUSIVE) {
            return Partial::Val(z as I32);
        }
    }
    Partial::None
}
#[inline(always)]
pub fn trunc_u_f32_i64(z: F32) -> Partial<I64> {
    if z.is_finite() {
        let z = z.trunc();
        if (0.0 <= z) && (z < F32_MAX_U64_EXCLUSIVE) {
            return Partial::Val(z as I64);
        }
    }
    Partial::None
}
#[inline(always)]
pub fn trunc_u_f64_i32(z: F64) -> Partial<I32> {
    if z.is_finite() {
        let z = z.trunc();
        if (0.0 <= z) && (z < F64_MAX_U32_EXCLUSIVE) {
            return Partial::Val(z as I32);
        }
    }
    Partial::None
}
#[inline(always)]
pub fn trunc_u_f64_i64(z: F64) -> Partial<I64> {
    if z.is_finite() {
        let z = z.trunc();
        if (0.0 <= z) && (z < F64_MAX_U64_EXCLUSIVE) {
            return Partial::Val(z as I64);
        }
    }
    Partial::None
}
#[inline(always)]
pub fn trunc_s_f32_i32(z: F32) -> Partial<I32> {
    if z.is_finite() {
        let z = z.trunc();
        if (F32_MIN_I32_INCLUSIVE <= z) && (z < F32_MAX_I32_EXCLUSIVE) {
            return Partial::Val(z as S32 as I32);
        }
    }
    Partial::None
}
#[inline(always)]
pub fn trunc_s_f32_i64(z: F32) -> Partial<I64> {
    if z.is_finite() {
        let z = z.trunc();
        if (F32_MIN_I64_INCLUSIVE <= z) && (z < F32_MAX_I64_EXCLUSIVE) {
            return Partial::Val(z as S64 as I64);
        }
    }
    Partial::None
}
#[inline(always)]
pub fn trunc_s_f64_i32(z: F64) -> Partial<I32> {
    if z.is_finite() {
        let z = z.trunc();
        if (F64_MIN_I32_INCLUSIVE <= z) && (z < F64_MAX_I32_EXCLUSIVE){
            return Partial::Val(z as S32 as I32);
        }
    }
    Partial::None
}
#[inline(always)]
pub fn trunc_s_f64_i64(z: F64) -> Partial<I64> {
    if z.is_finite() {
        let z = z.trunc();
        if (F64_MIN_I64_INCLUSIVE <= z) && (z < F64_MAX_I64_EXCLUSIVE) {
            return Partial::Val(z as S64 as I64);
        }
    }
    Partial::None
}

#[inline(always)]
pub fn promote(z: F32) -> F64 {
    z as F64
}

#[inline(always)]
pub fn demote(z: F64) -> F32 {
    z as F32
}

#[inline(always)]
pub fn convert_u_i32_f32(i: I32) -> F32 {
    i as F32
}
#[inline(always)]
pub fn convert_u_i32_f64(i: I32) -> F64 {
    i as F64
}
#[inline(always)]
pub fn convert_u_i64_f32(i: I64) -> F32 {
    i as F32
}
#[inline(always)]
pub fn convert_u_i64_f64(i: I64) -> F64 {
    i as F64
}
#[inline(always)]
pub fn convert_s_i32_f32(i: I32) -> F32 {
    IntegerOperations::signed(i) as F32
}
#[inline(always)]
pub fn convert_s_i32_f64(i: I32) -> F64 {
    IntegerOperations::signed(i) as F64
}
#[inline(always)]
pub fn convert_s_i64_f32(i: I64) -> F32 {
    IntegerOperations::signed(i) as F32
}
#[inline(always)]
pub fn convert_s_i64_f64(i: I64) -> F64 {
    IntegerOperations::signed(i) as F64
}

#[inline(always)]
pub fn reinterpret_i32_f32(i: I32) -> F32 {
    F32::from_bits(i)
}
#[inline(always)]
pub fn reinterpret_i64_f64(i: I64) -> F64 {
    F64::from_bits(i)
}
#[inline(always)]
pub fn reinterpret_f32_i32(z: F32) -> I32 {
    z.to_bits()
}
#[inline(always)]
pub fn reinterpret_f64_i64(z: F64) -> I64 {
    z.to_bits()
}
