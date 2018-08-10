use structure::types::*;
use structure::modules::*;

use crate::runtime_structure::*;
use crate::structure_references::*;

// TODO: more central definition
const WASM_PAGE_SIZE: usize = 65536;

pub mod external_typing {
    use super::*;

    pub fn func<Refs>(s: &Store<Refs>, a: FuncAddr) -> ExternType
        where Refs: StructureReference
    {
        let functype = match &s.funcs[a] {
            | FuncInst::Internal { type_, ..}
            | FuncInst::Host { type_, .. }
            => type_
        };
        ExternType::Func((**functype).clone()) // TODO: bad copy
    }

    pub fn table<Refs>(s: &Store<Refs>, a: TableAddr) -> ExternType
        where Refs: StructureReference
    {
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

    pub fn mem<Refs>(s: &Store<Refs>, a: MemAddr) -> ExternType
        where Refs: StructureReference
    {
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

    pub fn global<Refs>(s: &Store<Refs>, a: GlobalAddr) -> ExternType
        where Refs: StructureReference
    {
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

    pub enum AllocError {
        AllocatingTableBeyondMaxLimit,
        AllocatingMemBeyondMaxLimit,
    }
    use self::AllocError::*;
    pub type AResult = ::std::result::Result<(), AllocError>;

    pub fn alloc_function<Refs>(s: &mut Store<Refs>,
                                func: Refs::FuncRef,
                                module: &Refs,
                                moduleaddr: ModuleAddr) -> FuncAddr
        where Refs: StructureReference
    {
        let a = s.funcs.next_addr();
        let functype = module.functype_ref(func.type_.0 as usize);
        let funcinst = FuncInst::Internal {
            type_: functype,
            module: moduleaddr,
            code: func,
        };
        s.funcs.push(funcinst);

        a
    }
    pub fn alloc_host_function<Refs>(s: &mut Store<Refs>,
                                     hostfunc: HostFunc,
                                     functype: Refs::FuncTypeRef) -> FuncAddr
        where Refs: StructureReference
    {
        let a = s.funcs.next_addr();
        let funcinst = FuncInst::Host {
            type_: functype,
            hostcode: hostfunc,
        };
        s.funcs.push(funcinst);

        a
    }
    pub fn alloc_table<Refs>(s: &mut Store<Refs>,
                             tabletype: &TableType) -> TableAddr
        where Refs: StructureReference
    {
        let TableType {
            limits: Limits { min: n, max: m },
            elemtype: _ // TODO: Why does the spec ignore this here?
        } = *tabletype;
        let a = s.tables.next_addr();
        let tableinst = TableInst {
            elem: ::std::iter::repeat(FuncElem(None)).take(n as usize).collect(),
            max: m,
        };
        s.tables.push(tableinst);

        a
    }
    pub fn alloc_mem<Refs>(s: &mut Store<Refs>,
                           memtype: &MemType) -> MemAddr
        where Refs: StructureReference
    {
        let MemType {
            limits: Limits { min: n, max: m },
        } = *memtype;
        let a = s.mems.next_addr();
        let meminst = MemInst {
            data: vec![0x00; (n as usize) * WASM_PAGE_SIZE].into(),
            max: m,
        };
        s.mems.push(meminst);

        a
    }
    pub fn alloc_global<Refs>(s: &mut Store<Refs>,
                              globaltype: &GlobalType,
                              val: Val) -> GlobalAddr
        where Refs: StructureReference
    {
        let GlobalType {
            mutability,
            valtype: t,
        } = *globaltype;
        assert!(t == val.ty());
        let a = s.globals.next_addr();
        let globalinst = GlobalInst {
            value: val,
            mutability,
        };
        s.globals.push(globalinst);

        a
    }
    pub fn grow_table_by(tableinst: &mut TableInst,
                         n: usize) -> AResult {
        if let Some(max) = tableinst.max {
            if (max as usize) < (tableinst.elem.len() as usize + n) {
                Err(AllocatingTableBeyondMaxLimit)?;
            }
        }

        tableinst.elem.safe_append(n, || FuncElem(None));

        Ok(())
    }
    pub fn grow_memory_by(meminst: &mut MemInst,
                          n: usize) -> AResult {
        let len = n * WASM_PAGE_SIZE;
        if let Some(max) = meminst.max {
            if (max as usize * WASM_PAGE_SIZE) < (meminst.data.len() as usize + len) {
                Err(AllocatingMemBeyondMaxLimit)?;
            }
        }

        meminst.data.safe_append(len, || 0x00);

        Ok(())
    }
    pub fn alloc_module<Refs>(s: &mut Store<Refs>,
                              module: &Refs,
                              externvals_im: &[ExternVal],
                              vals: &[Val]) -> ModuleAddr
        where Refs: StructureReference
    {
        // NB: This is a modification to the spec to allow cycles between
        // function instances and module instances
        let a = s.modules.next_addr();
        let moduleaddr = a;

        let mut funcaddrs = vec![];
        for (i, _) in module.funcs.iter().enumerate() {
            let funci = module.func_ref(i);
            let funcaddri = alloc_function(s, funci, &module, moduleaddr);
            funcaddrs.push(funcaddri);
        }

        let mut tableaddrs = vec![];
        for tablei in &module.tables {
            let tableaddri = alloc_table(s, &tablei.type_);
            tableaddrs.push(tableaddri);
        }

        let mut memaddrs = vec![];
        for memi in &module.mems {
            let memaddri = alloc_mem(s, &memi.type_);
            memaddrs.push(memaddri);
        }

        let mut globaladdrs = vec![];
        for (i, globali) in module.globals.iter().enumerate() {
            let globaladdri = alloc_global(s, &globali.type_, vals[i]);
            globaladdrs.push(globaladdri);
        }

        let funcaddrs_mod: TypedIndexVec<_, _> = externvals_im.iter()
            .filter_map(|e| match e {
                ExternVal::Func(f) => Some(*f),
                _ => None,
            }).chain(funcaddrs).collect::<Vec<_>>().into();

        let tableaddrs_mod: TypedIndexVec<_, _>  = externvals_im.iter()
            .filter_map(|e| match e {
                ExternVal::Table(f) => Some(*f),
                _ => None,
            }).chain(tableaddrs).collect::<Vec<_>>().into();

        let memaddrs_mod: TypedIndexVec<_, _>  = externvals_im.iter()
            .filter_map(|e| match e {
                ExternVal::Mem(f) => Some(*f),
                _ => None,
            }).chain(memaddrs).collect::<Vec<_>>().into();

        let globaladdrs_mod: TypedIndexVec<_, _>  = externvals_im.iter()
            .filter_map(|e| match e {
                ExternVal::Global(f) => Some(*f),
                _ => None,
            }).chain(globaladdrs).collect::<Vec<_>>().into();

        let mut exportinsts = vec![];
        for (i, exporti) in module.exports.iter().enumerate() {
            let externvali = match exporti.desc {
                ExportDesc::Func(funcidx)
                    => ExternVal::Func(funcaddrs_mod[funcidx]),
                ExportDesc::Table(tableidx)
                    => ExternVal::Table(tableaddrs_mod[tableidx]),
                ExportDesc::Mem(memidx)
                    => ExternVal::Mem(memaddrs_mod[memidx]),
                ExportDesc::Global(globalidx)
                    => ExternVal::Global(globaladdrs_mod[globalidx]),
            };
            let exportinsti = ExportInst {
                name: module.name_ref(i),
                value: externvali,
            };
            exportinsts.push(exportinsti);
        }

        let moduleinst = ModuleInst {
            types: module.functypes_ref(),
            funcaddrs: funcaddrs_mod,
            tableaddrs: tableaddrs_mod,
            memaddrs: memaddrs_mod,
            globaladdrs: globaladdrs_mod,
            exports: exportinsts,
        };

        s.modules.push(moduleinst);

        moduleaddr
    }
}

pub mod instantiation {
    use super::*;

    #[derive(Debug)]
    pub enum InstantiationError {
        ModuleNotValid,
        MismatchedNumberOfProvidedImports,
        WrongExternTypeInImport,
        ElemIdxOutOfBounds,
        DataIdxOutOfBounds,
    }
    use self::InstantiationError::*;

    pub type IResult = std::result::Result<ModuleAddr, InstantiationError>;

    pub fn instantiate_module<Ref>(s: &mut Store<Ref>,
                                   module: &Ref,
                                   externvals: &[ExternVal]) -> IResult
        where Ref: StructureReference
    {
        let n = externvals.len();

        // module is valid per definition
        // ... return Err(ModuleNotValid);

        // TODO: not sure what to assert here
        // "Assert: module is valid with external types
        //  externtype^m_im classifying its imports."

        let externtypes_im = &module.import_export_mapping().imports;
        let m = externtypes_im.len();
        if m != n {
            Err(MismatchedNumberOfProvidedImports)?;
        }

        for (externvali, externtypei_) in externvals.iter().zip(externtypes_im.iter()) {
            // TODO: Verify that validation can never fail here

            let externtypei = match externvali {
                ExternVal::Func(x) => {
                    external_typing::func(s, *x)
                }
                ExternVal::Table(x) => {
                    external_typing::table(s, *x)
                }
                ExternVal::Mem(x) => {
                    external_typing::mem(s, *x)
                }
                ExternVal::Global(x) => {
                    external_typing::global(s, *x)
                }
            };

            if !import_matching::extern_type(&externtypei, &externtypei_) {
                Err(WrongExternTypeInImport)?;
            }
        }

        // TODO: implement evaluation
        let evaluate = |_| unimplemented!();

        let mut vals = vec![];
        {
            let moduleinst_im = ModuleInst {
                globaladdrs: externvals.iter().filter_map(|e| match e {
                    ExternVal::Global(f) => Some(*f),
                    _ => None,
                }).collect::<Vec<_>>().into(),
                exports: vec![],
                funcaddrs: vec![].into(),
                memaddrs: vec![].into(),
                tableaddrs: vec![].into(),
                types: module.functypes_ref(), // TODO: this should be empty
            };

            // NB: Because our Frame stores a ModuleAddr,
            // we need to temporarily allocate the auxilary
            // module instance in the store

            let aux_moduleaddr = s.modules.next_addr();
            s.modules.push(moduleinst_im);

            let f_im = Frame {
                locals: vec![],
                module: aux_moduleaddr,
            };

            let mut stack = Stack::new();
            stack.push(StackElem::Activation {
                // TODO: What value to pick for n here?
                // assuming n = 1 due to needing the result
                n: 1,
                frame: f_im,
            });

            for globali in &module.globals {
                let vali = evaluate(&globali.init);

                vals.push(vali);
            }

            assert!(stack.last() == Some(&StackElem::Activation {
                n: 1,
                frame: Frame {
                    locals: vec![],
                    module: aux_moduleaddr,
                },
            }));

            stack.pop();
            s.modules.pop_aux();
        }

        let moduleaddr = allocation::alloc_module(s, module, &externvals, &vals);

        let f = Frame {
            locals: vec![],
            module: moduleaddr,
        };

        let mut stack = Stack::new();
        stack.push(StackElem::Activation {
            // TODO: What value to pick for n here?
            // assuming n = 1 due to needing the result
            n: 1,
            frame: f,
        });

        let mut eoi_tabeladdri = vec![];
        for elemi in &module.elem {
            let eovali = evaluate(&elemi.offset);
            let eoi = if let Val::I32(eoi) = eovali {
                eoi
            } else {
                panic!("Due to validation, this should be a I32")
            } as usize;
            let tableidxi = elemi.table;
            let tableaddri = s.modules[moduleaddr].tableaddrs[tableidxi];
            let tableinsti = &s.tables[tableaddri];

            let eendi = eoi + elemi.init.len();
            if eendi > tableinsti.elem.len() {
                Err(ElemIdxOutOfBounds)?;
            }

            eoi_tabeladdri.push((eoi, tableaddri));
        }

        let mut doi_memaddri = vec![];
        for datai in &module.data {
            let dovali = evaluate(&datai.offset);
            let doi = if let Val::I32(doi) = dovali {
                doi
            } else {
                panic!("Due to validation, this should be a I32")
            } as usize;
            let memidxi = datai.data;
            let memaddri = s.modules[moduleaddr].memaddrs[memidxi];
            let meminsti = &s.mems[memaddri];

            let dendi = doi + datai.init.len();
            if dendi > meminsti.data.len() {
                Err(DataIdxOutOfBounds)?;
            }

            doi_memaddri.push((doi, memaddri));
        }

        assert!(stack.last() == Some(&StackElem::Activation {
            n: 1,
            frame: Frame {
                locals: vec![],
                module: moduleaddr,
            },
        }));

        stack.pop();

        for ((eoi, tableaddri), elemi) in eoi_tabeladdri.into_iter().zip(&module.elem) {
            let tableinsti = &mut s.tables[tableaddri];

            for (j, &funcidxij) in elemi.init.iter().enumerate() {
                let funcaddrij = s.modules[moduleaddr].funcaddrs[funcidxij];
                tableinsti.elem[eoi + j] = FuncElem(Some(funcaddrij));
            }
        }

        for ((doi, memaddri), datai) in doi_memaddri.into_iter().zip(&module.data) {
            let meminsti = &mut s.mems[memaddri];

            for (j, &bij) in datai.init.iter().enumerate() {
                meminsti.data[doi + j] = bij;
            }
        }

        if let Some(start) = &module.start {
            // TODO
        }

        unimplemented!()
    }
}


