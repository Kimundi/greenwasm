use std::borrow::Borrow;
use std::sync::Arc;

use structure::modules::*;
use structure::types::*;
use crate::runtime_structure::*;

pub trait StructureReference {
    type NameRef: Borrow<Name> + Clone;
    fn name_ref(&self, export_idx: usize) -> Self::NameRef;

    type FuncRef: Borrow<Func> + Clone;
    fn func_ref(&self, func_idx: usize) -> Self::FuncRef;

}

macro_rules! generate_refs {
    (ALL: $($t:tt)*) => (
        generate_refs!(CLONE: $($t)*);
        generate_refs!(ARC: $($t)*);
        generate_refs!(REF: $($t)*);
    );
    (CLONE: $modty:ty, $self:ident; $(
        $t:ty, $refty:ident, $fnname:ident($($argname:ident: $argty:ty),*), |$s:ident| $map:expr
    ;)*) => {
        impl StructureReference for $modty {
            $(
                type $refty = $t;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty {
                    let $s = $self;
                    $map.clone()
                }
            )*
        }
    };
    (REF: $modty:ty, $self:ident; $(
        $t:ty, $refty:ident, $fnname:ident($($argname:ident: $argty:ty),*), |$s:ident| $map:expr
    ;)*) => {
        impl<'a> StructureReference for &'a $modty {
            $(
                type $refty = &'a $t;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty {
                    let $s = $self;
                    &$map
                }
            )*
        }
    };
    (ARC: $modty:ty, $self:ident; $(
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
        impl StructureReference for Arc<$modty> {
            $(
                type $refty = self::arc::$refty;
                fn $fnname(&$self, $($argname: $argty),*) -> Self::$refty {
                    self::arc::$refty {
                        module: $self.clone(),
                        $(
                            $argname
                        ),*
                    }
                }
            )*
        }
    }
}

generate_refs! {
    ALL: Module, self;
    Name, NameRef, name_ref(export_idx: usize), |s| s.exports[export_idx].name;
    Func, FuncRef, func_ref(func_idx: usize),   |s| s.funcs[func_idx];
}
