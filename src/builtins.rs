use crate::ast::{ExprRef, Literal, Span};
use crate::core::Loc;
use crate::errors::MatzoError;
use crate::interp::{BuiltinFunc, Env, State, Thunk, Value};

fn arity_error(func: &str, expected: usize, actual: &[ExprRef]) -> Result<Value, MatzoError> {
    let msg = format!(
        "`{}`: expected {} argument{}, got {}",
        func,
        expected,
        if expected == 1 { "" } else { "s" },
        actual.len()
    );
    if actual.is_empty() {
        panic!("should not be possible to express")
        // Err(MatzoError::new(Span::empty(), msg))
    } else {
        let span = Span {
            start: actual[0].loc.span.start,
            end: actual[actual.len() - 1].loc.span.end,
        };
        let file = actual[0].loc.file;
        Err(MatzoError::new(Loc { span, file }, msg))
    }
}

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

const REP: BuiltinFunc = BuiltinFunc {
    name: "rep",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [rep, expr] = exprs {
            let mut buf = String::new();
            let num = state
                .eval(*rep, env)?
                .as_num(&state.ast.borrow(), rep.loc)?;
            for _ in 0..num {
                buf.push_str(
                    state
                        .eval(*expr, env)?
                        .as_str(&state.ast.borrow(), expr.loc)?,
                );
            }
            Ok(Value::Lit(Literal::Str(buf)))
        } else {
            arity_error("rep", 2, exprs)
        }
    },
};

const STR_UPPER: BuiltinFunc = BuiltinFunc {
    name: "str/upper",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [expr] = exprs {
            let s = state.eval(*expr, env)?;
            Ok(Value::Lit(Literal::Str(
                s.as_str(&state.ast.borrow(), expr.loc)?.to_uppercase(),
            )))
        } else {
            arity_error("str/upper", 1, exprs)
        }
    },
};

const STR_CAPITALIZE: BuiltinFunc = BuiltinFunc {
    name: "str/capitalize",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [expr] = exprs {
            let s = state.eval(*expr, env)?;
            Ok(Value::Lit(Literal::Str(titlecase::titlecase(
                s.as_str(&state.ast.borrow(), expr.loc)?,
            ))))
        } else {
            arity_error("str/capitalize", 1, exprs)
        }
    },
};

const STR_LOWER: BuiltinFunc = BuiltinFunc {
    name: "str/lower",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        if let [expr] = exprs {
            let s = state.eval(*expr, env)?;
            Ok(Value::Lit(Literal::Str(
                s.as_str(&state.ast.borrow(), expr.loc)?.to_lowercase(),
            )))
        } else {
            arity_error("str/lower", 1, exprs)
        }
    },
};

const WD: BuiltinFunc = BuiltinFunc {
    name: "wd",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        let mut buf = String::new();
        for expr in exprs {
            let s = state.eval(*expr, env)?;
            buf.push_str(s.as_str(&state.ast.borrow(), expr.loc)?);
        }
        Ok(Value::Lit(Literal::Str(buf)))
    },
};

const SE: BuiltinFunc = BuiltinFunc {
    name: "se",
    callback: &|state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
        let mut buf = String::new();
        let mut capitalized = false;
        let mut last_char = '\0';
        for expr in exprs {
            let s = state.eval(*expr, env)?;
            let s = s.as_str(&state.ast.borrow(), expr.loc)?;
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
                && s.chars().next().map_or(false, char::is_alphanumeric)
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

const ADD: BuiltinFunc = BuiltinFunc {
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

const SUB: BuiltinFunc = BuiltinFunc {
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

const MUL: BuiltinFunc = BuiltinFunc {
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

const TUPLE_LEN: BuiltinFunc = BuiltinFunc {
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

const TUPLE_CONCAT: BuiltinFunc = BuiltinFunc {
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

const TUPLE_FLATTEN: BuiltinFunc = BuiltinFunc {
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

const TUPLE_JOIN: BuiltinFunc = BuiltinFunc {
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

const TUPLE_INDEX: BuiltinFunc = BuiltinFunc {
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

const TUPLE_REP: BuiltinFunc = BuiltinFunc {
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

const TUPLE_REPLACE: BuiltinFunc = BuiltinFunc {
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

const TUPLE_FOLD: BuiltinFunc = BuiltinFunc {
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

const TUPLE_MAP: BuiltinFunc = BuiltinFunc {
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

/// The list of builtins provided at startup.
#[must_use]
pub fn builtins() -> Vec<BuiltinFunc> {
    vec![
        REP,
        // string functions
        STR_UPPER,
        STR_CAPITALIZE,
        STR_LOWER,
        // smart string-builders
        WD,
        SE,
        // arithmetic functions
        ADD,
        SUB,
        MUL,
        // tuple functions
        TUPLE_LEN,
        TUPLE_CONCAT,
        TUPLE_FLATTEN,
        TUPLE_JOIN,
        TUPLE_INDEX,
        TUPLE_REP,
        TUPLE_REPLACE,
        TUPLE_FOLD,
        TUPLE_MAP,
    ]
}
