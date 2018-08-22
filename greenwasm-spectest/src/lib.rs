use std::path::Path;
use wabt::script::*;

pub trait CommandDispatch {
    fn action_invoke(&mut self, module: Option<String>, field: String, args: Vec<Value>) -> InvokationResult;
    fn action_get(&mut self, module: Option<String>, field: String) -> Value;
    fn action(&mut self, action: Action) -> Vec<Value> {
        match action {
            Action::Invoke { module, field, args } => {
                if let InvokationResult::Vals(v) = self.action_invoke(module, field, args) {
                    v
                } else {
                    panic!("invokation returned Trap or exhausted the stack");
                }
            }
            Action::Get { module, field } => {
                vec![self.action_get(module, field)]
            }
        }
    }

    fn module(&mut self, bytes: Vec<u8>, name: Option<String>);
    fn assert_return(&mut self, action: Action, expected: Vec<Value>) {
        let results = self.action(action);
        assert_eq!(NanCompare(&expected), NanCompare(&results));
    }
    fn assert_trap(&mut self, action: Action) {
        match action {
            Action::Invoke { module, field, args } => {
                if let InvokationResult::Vals(results) = self.action_invoke(module, field, args) {
                    panic!("invokation did not trap, but return {:?}", results);
                }
            }
            Action::Get { .. } => {
                panic!("a global access can not trap!")
            }
        }
    }
    fn assert_malformed(&mut self, bytes: Vec<u8>);
    fn assert_invalid(&mut self, bytes: Vec<u8>);
    fn assert_unlinkable(&mut self, bytes: Vec<u8>) {
        // TODO: figure out the exact difference
        // Currently it looks like a link error is any instantiation error except
        // a runtime error during execution of a start function
        self.assert_uninstantiable(bytes);
    }
    fn assert_uninstantiable(&mut self, bytes: Vec<u8>);
    fn assert_exhaustion(&mut self, action: Action);
    fn assert_return_canonical_nan(&mut self, action: Action) {
        let results = self.action(action);
        match *results {
            [Value::F32(v)] if v.is_canonical_nan() => {}
            [Value::F64(v)] if v.is_canonical_nan() => {}
            ref x => {
                panic!("unexpected value {:?}", NanCompare(x));
            }
        }
    }
    fn assert_return_arithmetic_nan(&mut self, action: Action) {
        let results = self.action(action);
        match *results {
            [Value::F32(v)] if v.is_arithmetic_nan() => {}
            [Value::F64(v)] if v.is_arithmetic_nan() => {}
            ref x => {
                panic!("unexpected value {:?}", NanCompare(x));
            }
        }
    }
    fn register(&mut self, name: Option<String>, as_name: String);
}

pub struct NanCompare<'a>(pub &'a [Value]);
impl<'a> ::std::cmp::PartialEq for NanCompare<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        self.0.iter().zip(other.0.iter()).all(|pair| {
            match pair {
                (Value::I32(l), Value::I32(r)) => l == r,
                (Value::I64(l), Value::I64(r)) => l == r,
                (Value::F32(l), Value::F32(r)) if l.is_nan() && r.is_nan() => {
                    l.payload() == r.payload()
                },
                (Value::F64(l), Value::F64(r)) if l.is_nan() && r.is_nan() => {
                    l.payload() == r.payload()
                },
                (Value::F32(l), Value::F32(r)) => l == r,
                (Value::F64(l), Value::F64(r)) => l == r,
                _ => false,
            }
        })
    }
}
impl<'a> ::std::fmt::Debug for NanCompare<'a> {
    fn fmt(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        formatter.debug_list().entries(self.0.iter().map(|e| {
            match e {
                Value::F32(v) if v.is_nan() => {
                    let p = v.payload();
                    format!("F32(NaN:0x{:x})", p)
                }
                Value::F64(v) if v.is_nan() => {
                    let p = v.payload();
                    format!("F64(NaN:0x{:x})", p)
                }
                _ => format!("{:?}", e)
            }
        })).finish()
    }
}

pub enum InvokationResult {
    Vals(Vec<Value>),
    Trap,
    StackExhaustion,
}

pub trait NanPayload {
    fn payload(&self) -> u64;
    fn signif() -> u32;
    fn infinite() -> Self;
    fn canonical_payload() -> u64;
    fn arithmetic_nan(payload: u64) -> Self;
    fn canonical_nan() -> Self;
    fn is_arithmetic_nan(&self) -> bool;
    fn is_canonical_nan(&self) -> bool;
}
impl NanPayload for f32 {
    fn payload(&self) -> u64 {
        let bits: u32 = self.to_bits();
        let mask: u32 = (1u32 << 23) - 1;
        let p = bits & mask;
        p as u64
    }
    fn signif() -> u32 { 23 }
    fn infinite() -> Self { 1.0 / 0.0 }
    fn canonical_payload() -> u64 {
        1u64 << (Self::signif() - 1)
    }
    fn arithmetic_nan(payload: u64) -> Self {
        assert!(payload >= Self::canonical_payload());
        let bits: u32 = Self::infinite().to_bits();
        let mask: u32 = (1u32 << Self::signif()) - 1;
        let bits = bits | (mask & (payload as u32));
        Self::from_bits(bits)
    }
    fn canonical_nan() -> Self {
        Self::arithmetic_nan(Self::canonical_payload())
    }
    fn is_arithmetic_nan(&self) -> bool {
        self.is_nan()
    }
    fn is_canonical_nan(&self) -> bool {
        self.is_nan() && self.abs().to_bits() == Self::canonical_nan().to_bits()
    }
}
impl NanPayload for f64 {
    fn payload(&self) -> u64 {
        let bits: u64 = self.to_bits();
        let mask: u64 = (1u64 << 52) - 1;
        let p = bits & mask;
        p
    }
    fn signif() -> u32 { 52 }
    fn infinite() -> Self { 1.0 / 0.0 }
    fn canonical_payload() -> u64 {
        1u64 << (Self::signif() - 1)
    }
    fn arithmetic_nan(payload: u64) -> Self {
        assert!(payload >= Self::canonical_payload());
        let bits: u64 = Self::infinite().to_bits();
        let mask: u64 = (1u64 << Self::signif()) - 1;
        let bits = bits | (mask & payload);
        Self::from_bits(bits)
    }
    fn canonical_nan() -> Self {
        Self::arithmetic_nan(Self::canonical_payload())
    }
    fn is_arithmetic_nan(&self) -> bool {
        self.is_nan()
    }
    fn is_canonical_nan(&self) -> bool {
        self.is_nan() && self.abs().to_bits() == Self::canonical_nan().to_bits()
    }
}

#[must_use]
pub struct SpectestResult {
    pub failures: Vec<(String, u64, String)>,
    pub successes: usize,
}

impl SpectestResult {
    pub fn present(self) {
        if self.failures.len() > 0 {
            println!("wast failures:");
            for (i, f) in self.failures.iter().enumerate() {
                println!("    {}:{}, {}", f.0, f.1, f.2);
                if i > 10 {
                    println!("    ...");
                    break;
                }
            }
            println!("wast total: {} passed; {} failed", self.successes, self.failures.len());
            panic!("some wast commands failed");
        } else {
            println!("wast total: {} passed; {} failed", self.successes, self.failures.len());
        }
    }
}

pub fn run_mvp_spectest<C: CommandDispatch, F: FnMut() -> C>(create_dispatcher: F) -> SpectestResult {
    run_all_in_directory(format!("{}/testsuite", env!("CARGO_MANIFEST_DIR")).as_ref(), create_dispatcher)
}

pub fn run_all_in_directory<C: CommandDispatch, F: FnMut() -> C>(path: &Path, mut create_dispatcher: F) -> SpectestResult {
    use std::fs;
    let mut res = SpectestResult {
        failures: vec![],
        successes: 0,
    };

    'outer: for dir in fs::read_dir(&path).unwrap() {
        let dir = dir.unwrap();
        let path = dir.path();
        let filename = path.file_name().unwrap().to_str().unwrap();

        let mut sctrl = create_dispatcher();

        if path.metadata().unwrap().file_type().is_file() && filename.ends_with(".wast") {
            println!("Executing {} ...", filename);
            let res2 = run_single_file(&path, &mut sctrl);
            res.successes += res2.successes;
            res.failures.extend(res2.failures);
        }
    }

    return res;
}

pub fn run_single_file<C: CommandDispatch>(path: &Path, sctrl: &mut C) -> SpectestResult {
    use std::fs;

    let mut res = SpectestResult {
        failures: vec![],
        successes: 0,
    };

    let filename = path.file_name().unwrap().to_str().unwrap();
    let source = fs::read(&path).unwrap();

    let mut script = ScriptParser::<>::from_source_and_name(&source, filename).unwrap();
    let mut fatal = false;

    while let Some(Command { line, kind }) = script.next().unwrap() {
        if fatal {
            res.failures.push((filename.to_owned(), line, "<not attempted>".to_string()));
            continue;
        }
        match run_single_command(kind, sctrl) {
            Err(msg) => {
                res.failures.push((filename.to_owned(), line, msg));
                fatal = true;
            }
            Ok(()) => {
                res.successes += 1;
            }
        }
    }

    return res;
}

pub fn run_single_command<C: CommandDispatch>(kind: CommandKind, sctrl: &mut C) -> Result<(), String> {
    use std::panic::*;

    if let Err(msg) = catch_unwind(AssertUnwindSafe(|| {
        run_single_command_no_catch(kind, sctrl);
    })) {
        let msg = if let Some(msg) = msg.downcast_ref::<String>() {
            msg.to_string()
        } else if let Some(msg) = msg.downcast_ref::<&'static str>() {
            msg.to_string()
        } else {
            "<unknown>".to_string()
        };
        Err(msg)
    } else {
        Ok(())
    }
}
pub fn run_single_command_no_catch<C: CommandDispatch>(cmd: CommandKind, c: &mut C) {
    // TODO: Figure out if the "message" fields need to actually be handled
    use wabt::script::CommandKind::*;
    match cmd {
        Module { module, name } => {
            c.module(module.into_vec(), name);
        }
        AssertReturn { action, expected } => {
            c.assert_return(action, expected);
        }
        AssertReturnCanonicalNan { action } => {
            c.assert_return_canonical_nan(action);
        }
        AssertReturnArithmeticNan { action } => {
            c.assert_return_arithmetic_nan(action);
        }
        AssertTrap { action, message: _ } => {
            c.assert_trap(action);
        }
        AssertInvalid { module, message: _ } => {
            c.assert_invalid(module.into_vec());
        }
        AssertMalformed { module, message: _ } => {
            c.assert_malformed(module.into_vec());
        }
        AssertUninstantiable { module, message: _ } => {
            c.assert_uninstantiable(module.into_vec());
        }
        AssertExhaustion { action } => {
            c.assert_exhaustion(action);
        }
        AssertUnlinkable { module, message: _ } => {
            c.assert_unlinkable(module.into_vec());
        }
        Register { name, as_name } => {
            c.register(name, as_name);
        }
        PerformAction(action) => {
            c.action(action);
        }
    }
}
