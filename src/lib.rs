#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::module_name_repetitions)]

#[macro_use]
extern crate lalrpop_util;

pub mod ast;
pub mod builtins;
pub mod core;
pub mod errors;
pub mod interp;
pub mod lexer;
pub mod rand;
pub mod repl;
pub mod value;

#[cfg(test)]
pub mod test {
    include!(concat!(env!("OUT_DIR"), "/exp_tests.rs"));
}

lalrpop_mod!(#[allow(clippy::all)] #[allow(clippy::pedantic)] pub grammar);
