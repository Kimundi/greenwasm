/*
instance module:
- take ast
- create memories, globals, etc
- for each function:
  - iterate over all opcodes
  - translate to internal opcode format
  - calculate jump addresses inside the hte function
- function layout:
  - index per function
- return stackframe: function index, address (potentially inside a single 64 bit value?)
- break stackframe - not existing, just jump addresses in ops
- integer/addresses as plain 64 bit number
- with backjump register, linked list on stack

---
stack: 64 bit vector, with optional tags

type StackElement

struct Stack<T: CheckStrategy> {
    std::vector<T::StackElement>
}

features:
 track memory exactly somehow

*/

trait InterpreterEngine {
    type StackElement;
}

struct SafeAsserted;
struct Safe;
struct Unsafe;

enum TaggedStackElem {
    I32(u32),
    I64(u64),
    F32(f32),
    F64(f64),
    CallCtx {
        functionaddr: u32,
        opcodeaddr: u32,
    },
}
impl InterpreterEngine for SafeAsserted {
    type StackElement = TaggedStackElem;
}
impl InterpreterEngine for Safe {
    type StackElement = u64;
}
impl InterpreterEngine for Unsafe {
    type StackElement = u64;
}

struct Stack<T: InterpreterEngine> {
    stack: Vec<T::StackElement>,
}
