#![feature(nll)]
extern crate greenwasm;

use greenwasm::structure::types::*;
use greenwasm::binary_format::*;
use greenwasm::validation::*;
use greenwasm::execution::modules::*;
use greenwasm::execution::runtime_structure::*;
use greenwasm::execution::modules::instantiation::*;
use greenwasm::binary_format;

fn main() {
    let details = ::std::env::args().nth(2) == Some("--details".to_string());

    if let Some(ref path) = ::std::env::args().nth(1) {
        if let Err(e) = ::std::panic::catch_unwind(|| run(path, details).unwrap()) {
            let msg = if let Some(s) = e.downcast_ref::<String>() {
                &s[..]
            } else if let Some(s) = e.downcast_ref::<&'static str>() {
                s
            } else {
                "Unknown panic type"
            };

            println!("Process raised a panic:\n{}", msg);
            ::std::process::exit(2);
        }
    } else {
        println!("Usage: prog <wasmfile> [--details]");
        ::std::process::exit(1);
    }
}

#[derive(Debug)]
enum FuzzError {
    IoError(std::io::Error),
    ParseError(binary_format::NomErrorKind),
    ValidationError(ValidationError),
    InstantiationError(InstantiationError),
}

fn run(path: &str, details: bool) -> ::std::result::Result<(), FuzzError> {
    let file = std::fs::read(&path)
        .map_err(FuzzError::IoError)?;

    println!("Parsing...");
    let (module, _custom_sections) = parse_binary_format(&file).map_err(|e| {
        if !details {
            match e {
                ParseError::NomError(e) => {
                    e.into_error_kind()
                }
            }
        } else {
            unimplemented!()
        }
    })
    .map_err(FuzzError::ParseError)?;

    println!("Validation...");
    let validated_module = validate_module(module)
        .map_err(FuzzError::ValidationError)?;

    println!("Instantiation...");
    let mut store = Default::default();
    let mut stack = Stack::new();

    let moduleaddr = instantiation::instantiate_module(&mut store, &mut stack, &validated_module, &[])
        .map_err(FuzzError::InstantiationError)?;

    println!("Invocation of exports...");

    assert!(validated_module.start.is_none());

    let moduleinst = &store.modules[moduleaddr];

    let mut hanglimitinit = None;
    let mut export_addrs = vec![];
    for e in &moduleinst.exports {
        //println!("{:?}", e);
        if **e.name == "hangLimitInitializer" {
            hanglimitinit = Some(if let ExternVal::Func(faddr) = e.value { faddr } else { panic!() });
        }
        if e.name.ends_with("_invoker") {
            export_addrs.push(if let ExternVal::Func(faddr) = e.value { faddr } else { panic!() });
        }
    }
    if let Some(hanglimitinit) = hanglimitinit {
        invocation::invoke(&mut store, &mut stack, hanglimitinit, &[]).unwrap();

        for faddr in export_addrs {
            let fty = store.funcs[faddr].type_();
            let mut vals = vec![];
            for ty in &fty.args {
                match ty {
                    ValType::I32 => vals.push(Val::I32(0)),
                    ValType::I64 => vals.push(Val::I64(0)),
                    ValType::F32 => vals.push(Val::F32(0.0)),
                    ValType::F64 => vals.push(Val::F64(0.0)),
                }
            }

            invocation::invoke(&mut store, &mut stack, faddr, &vals).unwrap();
        }
    }

    Ok(())
}
