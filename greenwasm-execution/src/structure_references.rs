use std::borrow::Borrow;
use std::sync::Arc;
use std::ops::Deref;
use std::marker::PhantomData;

use structure::modules::*;
use structure::types::*;

#[derive(Clone)]
pub struct SelfDeref<U, T>(pub T, PhantomData<U>);
impl<T, U> Deref for SelfDeref<U, T>
    where T: Borrow<U>
{
    type Target = U;
    fn deref(&self) -> &Self::Target {
        self.0.borrow()
    }
}

pub trait StructureReference:
    Deref<Target=Module>
{
    type NameRef: Deref<Target=Name> + Clone;
    fn name_ref(&self, export_idx: usize) -> Self::NameRef;

    type FuncRef: Deref<Target=Func> + Clone;
    fn func_ref(&self, func_idx: usize) -> Self::FuncRef;

}

macro_rules! generate_refs {
    (ALL: $($t:tt)*) => (
        generate_refs!(CLONE: $($t)*);
        generate_refs!(ARC: $($t)*);
        generate_refs!(REF: $($t)*);
    );
    (CLONE: $self:ident; $(
        $t:ty, $refty:ident, $fnname:ident($($argname:ident: $argty:ty),*), |$s:ident| $map:expr
    ;)*) => {
        impl StructureReference for SelfDeref<Module, Module> {
            $(
                type $refty = SelfDeref<$t, $t>;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty {
                    let $s = $self;
                    SelfDeref($map.clone(), PhantomData)
                }
            )*
        }
    };
    (REF: $self:ident; $(
        $t:ty, $refty:ident, $fnname:ident($($argname:ident: $argty:ty),*), |$s:ident| $map:expr
    ;)*) => {
        impl<'a> StructureReference for SelfDeref<Module, &'a Module> {
            $(
                type $refty = SelfDeref<$t, &'a $t>;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty {
                    let $s = &$self.0;
                    SelfDeref(&$map, PhantomData)
                }
            )*
        }
    };
    (ARC: $self:ident; $(
        $t:ty, $refty:ident, $fnname:ident($($argname:ident: $argty:ty),*), |$s:ident| $map:expr
    ;)*) => {
        pub mod arc {
            use super::*;

            $(
                #[derive(Clone)]
                pub struct $refty {
                    pub(super) module: Arc<Module>,
                    $(
                        pub(super) $argname: $argty
                    ),*
                }
                impl Borrow<$t> for $refty {
                    fn borrow(&self) -> &$t {
                        let $s = &self.module;
                        $(
                            let $argname = self.$argname;
                        )*
                        &$map
                    }
                }
            )*
        }
        impl StructureReference for SelfDeref<Module, Arc<Module>> {
            $(
                type $refty = SelfDeref<$t, self::arc::$refty>;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty {
                    let module: &Arc<Module> = &$self.0;

                    SelfDeref(self::arc::$refty {
                        module: module.clone(),
                        $(
                            $argname
                        ),*
                    }, PhantomData)
                }
            )*
        }
    }
}

generate_refs! {
    ALL: self;
    Name, NameRef, name_ref(export_idx: usize), |s| s.exports[export_idx].name;
    Func, FuncRef, func_ref(func_idx: usize),   |s| s.funcs[func_idx];
}

pub type ModuleCloneRef = SelfDeref<Module, Module>;
pub type ModuleRefRef<'a> = SelfDeref<Module, &'a Module>;
pub type ModuleArcRef = SelfDeref<Module, Arc<Module>>;
