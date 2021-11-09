#[macro_use]
extern crate lalrpop_util;

pub mod ast;
pub mod interp;

#[cfg(test)]
pub mod test {
    include!(concat!(env!("OUT_DIR"), "/exp_tests.rs"));
}

lalrpop_mod!(#[allow(clippy::all)] pub grammar);
