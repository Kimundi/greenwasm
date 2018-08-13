use binary_format::*;
use validation::*;
use execution::modules::*;
use execution::runtime_structure::*;
use execution::modules::instantiation::*;

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

    println!("Execution...");
    let mut store = Default::default();
    let mut stack = Stack::new();

    instantiation::instantiate_module(&mut store, &mut stack, &validated_module, &[])
        .map_err(FuzzError::InstantiationError)?;

    println!("Done");

    Ok(())
}
