## What is it?
An (INCOMPLETE) jit compiled awk (jawk) implementation leveraging GNU libjit. The goal is the to be the fastest awk for all programs.

In reality, it will never be faster for programs like `BEGIN { print "A" }` since an interpreter has less overhead. 
But for all programs >1ms (in say gawk or mawk) jawk aims to be faster.

## How to use

### Ubuntu:
```
sudo apt-get install autoconf pkg-config libtool flex bison automake make g++
cargo build --release
``` 

### Windows
For now you need to use WSL and follow the ubuntu instructions

### General:
```
cargo build --release
./target/release/jawk '{ print "Some awk program!}" }' 
./target/release/jawk -f run.awk some_file.txt
cargo run -- --debug 'BEGIN { print "this will print debug info including the AST and runtime calls" }'
```

## Todo:

1. Reading from stdin
2. Support for awk functions
    - Functions are mutually recursive but not first class. Global too. 
    - Cannot be declared within each other.
    - `function a() { b() }; function b() { a () };` is fine
3. Native math functions like sin, cos, etc, rand, srand (libjit supports many of these)
4. Native string functions gsub, index, length, match, split, sprintf, sub, substr, tolower, toupper
5. Regex expressions matched/not-matched (in JIT or runtime)
6. Array support
7. Redirect output to file
   - close() function
8. Missing Operators
   - Ternary
   - pre/post inc/dec a++ --b
   - exponential a^b
   - !logical_not
   - unary +a -b
   - expr in array a in b
   - modulus
9. Parsing / Lexing negative numbers
10. ARGV / ARGC and other ENV vars
11. Pattern Ranges 
12. Support for unicode in the string comparisons (subroutines.rs)
13. Parser need to be able to print the where it was when shit went wrong and what happened
14. Do we actually need numeric strings???
15. The columns runtime needs to be much faster and lazier.
16. Make this compile on Windows!
17. Divide by 0 needs to print an error (tests for this will probably need to be bespoke)

## License
GNU Libjit is GPLv2. Code written during the hackathon is not licensed.

## Running the tests

Install other awks to test against (they should be on your path with these exact names)
1. gawk (linux/mac you already have it)
2. [mawk](https://invisible-island.net/mawk/) - build from src
3. [goawk](https://github.com/benhoyt/goawk) - need the go toolchain, then go get
4. [onetrueawk](https://github.com/onetrueawk/awk) - super easy and fast build from src

Tests look in ./target/release/jawk for a binary to do perf comparisons so you need to do a release build to run the tests.

```
cargo build --release
cargo test
```
