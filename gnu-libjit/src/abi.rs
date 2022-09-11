#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub enum Abi {
    Cdecl,
    VarArg,
    Stdcall,
    Fastcall,
}

// impl Into<jit_abi_t> for Abi {
//     fn into(self) -> jit_abi_t {
//         match self {
//             Abi::Cdecl => jit_abi_t_jit_abi_cdecl,
//             Abi::VarArg => jit_abi_t_jit_abi_vararg,
//             Abi::Stdcall => jit_abi_t_jit_abi_stdcall,
//             Abi::Fastcall => jit_abi_t_jit_abi_fastcall,
//         }
//     }
// }