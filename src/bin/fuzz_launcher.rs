use binary_format::parse_binary_format;
use validation::validate_module;

fn main() {
    if let Some(path) = ::std::env::args().nth(1) {
        let file = std::fs::read(&path).unwrap();

        println!("Parsing:");
        let (module, _custom_sections) = parse_binary_format(&file).unwrap();

        println!("Validation:");
        validate_module(module).unwrap();
    } else {
        println!("Usage: prog <wasmfile>")
    }
}
