use crate::ast::{ExprRef, Literal};
use crate::errors::MatzoError;
use crate::interp::{BuiltinFunc, Env, State, Value};

use super::helpers::arity_error;

pub const ADD: BuiltinFunc = BuiltinFunc {
    name: "add",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        let mut sum = 0;
        for expr in exprs {
            let n = state
                .eval(*expr, env)?
                .as_num(&state.ast.borrow(), expr.loc)?;
            sum += n;
        }
        Ok(Value::Lit(Literal::Num(sum)))
    },
};

pub const SUB: BuiltinFunc = BuiltinFunc {
    name: "sub",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [x, y] = exprs {
            let x = state.eval(*x, env)?.as_num(&state.ast.borrow(), x.loc)?;
            let y = state.eval(*y, env)?.as_num(&state.ast.borrow(), y.loc)?;
            Ok(Value::Lit(Literal::Num(x - y)))
        } else {
            arity_error("sub", 2, exprs)
        }
    },
};

pub const MUL: BuiltinFunc = BuiltinFunc {
    name: "mul",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        let mut prod = 1;
        for expr in exprs {
            let n = state
                .eval(*expr, env)?
                .as_num(&state.ast.borrow(), expr.loc)?;
            prod *= n;
        }
        Ok(Value::Lit(Literal::Num(prod)))
    },
};
