# gnu-libjit
**Warning**: This wrapper contains only exactly the functionality from libjit I needed for my own project. You may find it to be missing a piece of functionality. It shouldn't be too hard to add it yourself (:

This crate is a safe and performant wrapper around [gnu-libjit](https://www.gnu.org/software/libjit/). A just in time compiler with its
own IR, optimization passes, and easy interoperability.

It's possible to jit a small program and execute it in < 5 ms compared to ~ 80 ms with llvm. 
I highly recmomend the [inkwell](https://github.com/TheDan64/inkwell) library as a safe, feature rich, and well documented
llvm rust wrapper. Do note that you are unlikely to execute a JIT'ed program in single digit ms with any llvm wrapper but you do
get much more powerful optimizations, and far more safety and stability than with this library.

# Example
```rust
use gnu_libjit::{Abi, Context};

fn main() {
    let mut context = Context::new();
    context.build_start();


    let int_type = Context::int_type();
    let params = vec![int_type, int_type, int_type];
    let mut func = context.function(Abi::Cdecl, int_type, params).unwrap();

    let x = func.arg(0).unwrap();
    let y = func.arg(1).unwrap();
    let z = func.arg(2).unwrap();
    let temp1 = func.insn_mult(&x, &y);
    let temp2 = func.insn_add(&temp1, &z);
    func.insn_return(temp2);
    func.compile();
    context.build_end();
    
    let result: extern "C" fn(i32,i32,i32) -> i32 = func.to_closure();
    println!("3*5+2 = {}", result(3,5,2))
}
```
See `./examples` and `./src/test.rs` for more. There are no docs. Functions are named almost exactly as in the libjit library. You can use its docs [here](https://www.gnu.org/software/libjit/doc/libjit.html).

# Features
- Most common types supported `f32`, `f64`, `i64`, `i32`, `i8`, `u8` `*mut c_void`
- Easy to call native rust functions
- Support for inserting labels into the IR
- `if` `if_not` branching plus the `eq` operator to call them with

# License
See LICENSE