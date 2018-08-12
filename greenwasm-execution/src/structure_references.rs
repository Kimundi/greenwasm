use std::borrow::Borrow;
use std::sync::Arc;
use std::ops::Deref;
use std::marker::PhantomData;

use structure::modules::*;
use structure::types::*;
use validation::*;

// TODO: util module
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

macro_rules! generate_refs {
    (ALL: $($t:tt)*) => (
        generate_refs!(TRAIT: $($t)*);
        generate_refs!(CLONE: $($t)*);
        generate_refs!(ARC: $($t)*);
        generate_refs!(REF: $($t)*);
    );
    (TRAIT: $modty:ty, $self:ident; $(
        $t:ty, $refty:ident, $fnname:ident($($argname:ident: $argty:ty),*), |$s:ident| $map:expr
    ;)*) => {
        pub trait StructureReference<'ast>: Deref<Target=$modty> + 'ast {
            $(
                type $refty: Deref<Target=$t> + Clone;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty;
            )*
        }
    };
    (CLONE: $modty:ty, $self:ident; $(
        $t:ty, $refty:ident, $fnname:ident($($argname:ident: $argty:ty),*), |$s:ident| $map:expr
    ;)*) => {
        impl<'ast> StructureReference<'ast> for SelfDeref<$modty, $modty> {
            $(
                type $refty = SelfDeref<$t, $t>;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty {
                    let $s = $self;
                    SelfDeref($map.clone(), PhantomData)
                }
            )*
        }
        pub type ModuleCloneRef = SelfDeref<$modty, $modty>;
    };
    (REF: $modty:ty, $self:ident; $(
        $t:ty, $refty:ident, $fnname:ident($($argname:ident: $argty:ty),*), |$s:ident| $map:expr
    ;)*) => {
        impl<'ast, 'a: 'ast> StructureReference<'ast> for SelfDeref<$modty, &'a $modty> {
            $(
                type $refty = SelfDeref<$t, &'a $t>;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty {
                    let $s = &$self.0;
                    SelfDeref(&$map, PhantomData)
                }
            )*
        }
        pub type ModuleRefRef<'a> = SelfDeref<$modty, &'a $modty>;
    };
    (ARC: $modty:ty, $self:ident; $(
        $t:ty, $refty:ident, $fnname:ident($($argname:ident: $argty:ty),*), |$s:ident| $map:expr
    ;)*) => {
        pub mod arc {
            use super::*;

            $(
                // TODO: If Module itself had configurable subelement types,
                // we could delegate inner Arc references without
                // indirection.
                #[derive(Clone)]
                pub struct $refty {
                    pub(super) module: Arc<$modty>,
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
        impl<'ast> StructureReference<'ast> for SelfDeref<$modty, Arc<$modty>> {
            $(
                type $refty = SelfDeref<$t, self::arc::$refty>;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty {
                    let module: &Arc<$modty> = &$self.0;

                    SelfDeref(self::arc::$refty {
                        module: module.clone(),
                        $(
                            $argname
                        ),*
                    }, PhantomData)
                }
            )*
        }
        pub type ModuleArcRef = SelfDeref<$modty, Arc<$modty>>;
    }
}

generate_refs! {
    ALL: ValidatedModule, self;

    Name, NameRef, name_ref(export_idx: usize), |s| s.exports[export_idx].name;
    Func, FuncRef, func_ref(func_idx: usize), |s| s.funcs[func_idx];
    FuncType, FuncTypeRef, functype_ref(type_idx: usize), |s| s.types[type_idx];
    Wec<FuncType>, FuncTypesRef, functypes_ref(), |s| s.types;
}
