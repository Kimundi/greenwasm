use structure::types::*;
use structure::modules::*;
use crate::runtime_structure::*;

mod external_typing {
    use super::*;

    pub fn func(s: &Store, a: usize) -> ExternType {
        let functype = match &s.funcs[a] {
            | FuncInst::Internal { type_, ..}
            | FuncInst::Host { type_, .. }
            => type_
        };
        ExternType::Func(functype.clone()) // TODO: bad copy
    }

    pub fn tables(s: &Store, a: usize) -> ExternType {
        let TableInst { elem, max } = &s.tables[a];
        let n = elem.len();
        let m = *max;

        assert!(n <= WEC_MAX_SIZE);
        let n = n as u32;

        ExternType::Table(TableType {
            limits: Limits {
                min: n,
                max: m,
            },
            elemtype: ElemType::AnyFunc,
        })
    }

    pub fn mem(s: &Store, a: usize) -> ExternType {
        // TODO: more central definition
        const WASM_PAGE_SIZE: usize = 65536;

        let MemInst { data, max } = &s.mems[a];
        let n = data.len() / WASM_PAGE_SIZE;
        let m = *max;

        assert!(n <= WEC_MAX_SIZE);
        let n = n as u32;

        ExternType::Mem(MemType {
            limits: Limits {
                min: n,
                max: m,
            }
        })
    }

    pub fn global(s: &Store, a: usize) -> ExternType {
        let GlobalInst { ref value, mutability } = s.globals[a];
        let t = value.ty();

        ExternType::Global(GlobalType {
            mutability,
            valtype: t,
        })
    }
}
