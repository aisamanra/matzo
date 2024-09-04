use crate::interp::BuiltinFunc;

pub mod helpers;
pub mod number;
pub mod string;
pub mod tuple;

/// The list of builtins provided at startup.
pub const BUILTINS: &[BuiltinFunc] = &[
    // string functions
    string::REP,
    string::STR_UPPER,
    string::STR_CAPITALIZE,
    string::STR_LOWER,
    // smart string-builders
    string::WD,
    string::SE,
    // arithmetic functions
    number::ADD,
    number::SUB,
    number::MUL,
    // tuple functions
    tuple::TUPLE_LEN,
    tuple::TUPLE_CONCAT,
    tuple::TUPLE_FLATTEN,
    tuple::TUPLE_JOIN,
    tuple::TUPLE_INDEX,
    tuple::TUPLE_REP,
    tuple::TUPLE_REPLACE,
    tuple::TUPLE_FOLD,
    tuple::TUPLE_MAP,
];
