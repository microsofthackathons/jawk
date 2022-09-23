# libjit-sys
[gnu-libjit homepage](https://www.gnu.org/software/libjit/)


This project adds raw rust bindings around the libjit package. It is useful as a low startup time jit compiler making it ideal
for command line tools, interpreted languages, etc. Time to compile a single function that returns a number is about ~5ms on my old laptop.

You can find a safe rust-style wrapper around the bindings @ [github.com/n8ta/gnu-libjit](https://github.com/n8ta/gnu-libjit).

# Usage
```
libjit-sys = "0.0.5"
```


Ubuntu:
```
sudo apt-get install autoconf pkg-config libtool yacc flex bison automake make g++
cargo build --release
```

