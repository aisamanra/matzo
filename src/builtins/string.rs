use crate::ast::{ExprRef, Literal};
use crate::errors::MatzoError;
use crate::interp::{BuiltinFunc, Env, State, Value};

use super::helpers::arity_error;

pub const REP: BuiltinFunc = BuiltinFunc {
    name: "rep",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [rep, expr] = exprs {
            let mut buf = String::new();
            let num = state
                .eval(*rep, env)?
                .as_num(&state.ast.borrow(), rep.loc)?;
            for _ in 0..num {
                buf.push_str(&state.force_to_string(*expr, env)?);
            }
            Ok(Value::Lit(Literal::Str(buf)))
        } else {
            arity_error("rep", 2, exprs)
        }
    },
};

pub const STR_UPPER: BuiltinFunc = BuiltinFunc {
    name: "str/upper",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [expr] = exprs {
            let s = state.force_to_string(*expr, env)?;
            Ok(Value::Lit(Literal::Str(s.to_uppercase())))
        } else {
            arity_error("str/upper", 1, exprs)
        }
    },
};

pub const STR_CAPITALIZE: BuiltinFunc = BuiltinFunc {
    name: "str/capitalize",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [expr] = exprs {
            let s = state.force_to_string(*expr, env)?;
            Ok(Value::Lit(Literal::Str(titlecase::titlecase(&s))))
        } else {
            arity_error("str/capitalize", 1, exprs)
        }
    },
};

pub const STR_LOWER: BuiltinFunc = BuiltinFunc {
    name: "str/lower",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [expr] = exprs {
            let s = state.force_to_string(*expr, env)?;
            Ok(Value::Lit(Literal::Str(s.to_lowercase())))
        } else {
            arity_error("str/lower", 1, exprs)
        }
    },
};

pub const WD: BuiltinFunc = BuiltinFunc {
    name: "wd",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        let mut buf = String::new();
        for expr in exprs {
            let s = state.force_to_string(*expr, env)?;
            buf.push_str(&s);
        }
        Ok(Value::Lit(Literal::Str(buf)))
    },
};

pub const SE: BuiltinFunc = BuiltinFunc {
    name: "se",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        let mut buf = String::new();
        let mut capitalized = false;
        let mut last_char = '\0';
        for expr in exprs {
            let s = state.force_to_string(*expr, env)?;
            if !capitalized && !s.trim().is_empty() {
                capitalized = true;
                let mut chars = s.chars();
                for c in chars.next().unwrap().to_uppercase() {
                    buf.push(c);
                }
                for c in chars {
                    buf.push(c);
                }
            } else if last_char.is_alphanumeric()
                && s.chars().next().is_some_and(char::is_alphanumeric)
            {
                buf.push(' ');
                buf.push_str(s.trim());
            } else {
                buf.push_str(s.trim());
            }
            if !buf.is_empty() {
                last_char = buf.chars().last().unwrap();
            }
        }
        Ok(Value::Lit(Literal::Str(buf)))
    },
};
