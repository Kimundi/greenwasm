use structure::types::*;
use structure::modules::*;
use crate::runtime_structure::*;

// TODO: more central definition
const WASM_PAGE_SIZE: usize = 65536;

pub mod external_typing {
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

pub mod import_matching {
    use super::*;

    pub fn limits(a: &Limits, b: &Limits) -> bool {
        let Limits { min: n1, max: m1 } = a;
        let Limits { min: n2, max: m2 } = b;

        (n1 >= n2) && (
            (m2.is_none()) || (
                m1.is_some() && m2.is_some() && m1.unwrap() <= m2.unwrap()
            )
        )
    }

    pub fn extern_type(a: &ExternType, b: &ExternType) -> bool {
        match (a, b) {
            (ExternType::Func(a), ExternType::Func(b)) => {
                a == b
            }
            (ExternType::Table(a), ExternType::Table(b)) => {
                limits(&a.limits, &b.limits) && a.elemtype == b.elemtype
            }
            (ExternType::Mem(a), ExternType::Mem(b)) => {
                limits(&a.limits, &b.limits)
            }
            (ExternType::Global(a), ExternType::Global(b)) => {
                a == b
            }
            _ => false
        }
    }
}

pub mod allocation {
    use super::*;

    pub fn function(s: &mut Store, func: &Func, moduleinst: &ModuleInst) -> FuncAddr {
        let a = s.funcs.len();
        let functype = &moduleinst.types[func.type_.0 as usize];
        let funcinst = FuncInst::Internal {
            type_: functype.clone(), // TODO: bad copy
            module: moduleinst.clone(), // TODO: bad copy
            code: func.clone() // TODO: bad copy
        };
        s.funcs.push(funcinst);

        FuncAddr(a)
    }
    pub fn host_function(s: &mut Store, hostfunc: HostFunc, functype: FuncType) -> FuncAddr {
        let a = s.funcs.len();
        let funcinst = FuncInst::Host {
            type_: functype,
            hostcode: hostfunc,
        };
        s.funcs.push(funcinst);

        FuncAddr(a)
    }
    pub fn table(s: &mut Store, tabletype: &TableType) -> TableAddr {
        let TableType {
            limits: Limits { min: n, max: m },
            elemtype: _ // TODO: Why does the spec ignore this here?
        } = *tabletype;
        let a = s.tables.len();
        let tableinst = TableInst {
            elem: ::std::iter::repeat(FuncElem(None)).take(n as usize).collect(),
            max: m,
        };
        s.tables.push(tableinst);

        TableAddr(a)
    }
    pub fn mem(s: &mut Store, memtype: &MemType) -> MemAddr {
        let MemType {
            limits: Limits { min: n, max: m },
        } = *memtype;
        let a = s.mems.len();
        let meminst = MemInst {
            data: vec![0x00; (n as usize) * WASM_PAGE_SIZE].into(),
            max: m,
        };
        s.mems.push(meminst);

        MemAddr(a)
    }
    pub fn global(s: &mut Store, globaltype: &GlobalType, val: Val) -> GlobalAddr {
        let GlobalType {
            mutability,
            valtype: t,
        } = *globaltype;
        assert!(t == val.ty());
        let a = s.globals.len();
        let globalinst = GlobalInst {
            value: val,
            mutability,
        };
        s.globals.push(globalinst);

        GlobalAddr(a)
    }
}


