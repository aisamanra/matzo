use crate::ast::{ExprRef, Literal};
use crate::errors::MatzoError;
use crate::interp::{BuiltinFunc, Env, State, Thunk, Value};

use super::helpers::arity_error;

fn tuple_flatten_helper(
    state: &State,
    val: Value,
    cb: &mut impl FnMut(Value),
) -> Result<(), MatzoError> {
    if let Value::Tup(thunks) = val {
        for thunk in &thunks {
            let val = state.hnf(thunk)?;
            tuple_flatten_helper(state, val, cb)?;
        }
    } else {
        cb(val);
    }
    Ok(())
}

pub const TUPLE_LEN: BuiltinFunc = BuiltinFunc {
    name: "tuple/len",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [expr] = exprs {
            let tup = state.eval(*expr, env)?;
            let raw_len = tup.as_tup(&state.ast.borrow(), expr.loc)?.len();
            match raw_len.try_into() {
                Ok(n) => Ok(Value::Lit(Literal::Num(n))),
                Err(_) => Err(MatzoError::new(
                    expr.loc,
                    "Tuple size too large to represent".to_string(),
                )),
            }
        } else {
            arity_error("tuple/len", 1, exprs)
        }
    },
};

pub const TUPLE_CONCAT: BuiltinFunc = BuiltinFunc {
    name: "tuple/concat",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [expr] = exprs {
            let val = state.eval(*expr, env)?;
            let tup = val.as_tup(&state.ast.borrow(), expr.loc)?;
            let mut contents = Vec::new();
            for elem in tup {
                for th in state.hnf(elem)?.as_tup(&state.ast.borrow(), expr.loc)? {
                    contents.push(th.clone());
                }
            }
            Ok(Value::Tup(contents))
        } else {
            arity_error("tuple/concat", 1, exprs)
        }
    },
};

pub const TUPLE_FLATTEN: BuiltinFunc = BuiltinFunc {
    name: "tuple/flatten",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        let mut contents = Vec::new();
        for expr in exprs {
            let val = state.eval(*expr, env)?;
            tuple_flatten_helper(state, val, &mut |v| {
                contents.push(Thunk::Value(v));
            })?;
        }
        Ok(Value::Tup(contents))
    },
};

pub const TUPLE_JOIN: BuiltinFunc = BuiltinFunc {
    name: "tuple/join",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if exprs.is_empty() || exprs.len() > 2 {
            return arity_error("tuple/index", 1, exprs);
        }

        let joiner = if let Some(join_e) = exprs.get(1) {
            state
                .eval(*join_e, env)?
                .as_str(&state.ast.borrow(), join_e.loc)?
                .to_string()
        } else {
            String::new()
        };

        let tup_e = exprs[0];
        let tup = state.eval(tup_e, env)?;

        let mut buf = String::new();
        for (idx, elem) in tup
            .as_tup(&state.ast.borrow(), tup_e.loc)?
            .iter()
            .enumerate()
        {
            if idx > 0 {
                buf.push_str(&joiner);
            }
            let elem = state.hnf(elem)?;
            buf.push_str(elem.as_str(&state.ast.borrow(), tup_e.loc)?);
        }
        Ok(Value::Lit(Literal::Str(buf)))
    },
};

pub const TUPLE_INDEX: BuiltinFunc = BuiltinFunc {
    name: "tuple/index",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [tup_e, idx_e] = exprs {
            let tup = state.eval(*tup_e, env)?;
            let idx = state.eval(*idx_e, env)?;
            state.hnf(
                &tup.as_tup(&state.ast.borrow(), tup_e.loc)?
                    [idx.as_usize(&state.ast.borrow(), idx_e.loc)?],
            )
        } else {
            arity_error("tuple/index", 1, exprs)
        }
    },
};

pub const TUPLE_REP: BuiltinFunc = BuiltinFunc {
    name: "tuple/rep",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [rep, expr] = exprs {
            let mut tup = Vec::new();
            let num = state
                .eval(*rep, env)?
                .as_num(&state.ast.borrow(), rep.loc)?;
            for _ in 0..num {
                tup.push(Thunk::Expr(*expr, env.clone()));
            }
            Ok(Value::Tup(tup))
        } else {
            arity_error("rep", 2, exprs)
        }
    },
};

pub const TUPLE_REPLACE: BuiltinFunc = BuiltinFunc {
    name: "tuple/replace",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [tup, idx, new] = exprs {
            let tup_val = state.eval(*tup, env)?;
            let tup = tup_val.as_tup(&state.ast.borrow(), tup.loc)?;
            let idx = state
                .eval(*idx, env)?
                .as_usize(&state.ast.borrow(), idx.loc)?;

            let mut modified = Vec::with_capacity(tup.len());
            for item in tup.iter().take(idx) {
                modified.push(item.clone());
            }
            modified.push(Thunk::Expr(*new, env.clone()));
            for item in tup.iter().skip(idx + 1) {
                modified.push(item.clone());
            }
            Ok(Value::Tup(modified))
        } else {
            arity_error("tuple/replace", 3, exprs)
        }
    },
};

pub const TUPLE_FOLD: BuiltinFunc = BuiltinFunc {
    name: "tuple/fold",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [func_e, init, tup_e] = exprs {
            let func = state.eval(*func_e, env)?;
            let tup = state.eval(*tup_e, env)?;

            let mut result = Thunk::Expr(*init, env.clone());
            for t in tup.as_tup(&state.ast.borrow(), tup_e.loc)? {
                result = Thunk::Value(state.eval_closure(
                    func.as_closure(&state.ast.borrow(), func_e.loc)?,
                    vec![result, t.clone()],
                )?);
            }

            state.hnf(&result)
        } else {
            arity_error("tuple/fold", 3, exprs)
        }
    },
};

pub const TUPLE_MAP: BuiltinFunc = BuiltinFunc {
    name: "tuple/map",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [func_e, tup_e] = exprs {
            let func = state.eval(*func_e, env)?;
            let tup = state.eval(*tup_e, env)?;

            let mut new_tup = Vec::new();
            let closure = func.as_closure(&state.ast.borrow(), func_e.loc)?;
            for t in tup.as_tup(&state.ast.borrow(), tup_e.loc)? {
                new_tup.push(Thunk::Value(state.eval_closure(closure, vec![t.clone()])?));
            }

            Ok(Value::Tup(new_tup))
        } else {
            arity_error("tuple/map", 2, exprs)
        }
    },
};
