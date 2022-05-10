use crate::ast::*;
use crate::errors::MatzoError;
use crate::interp::*;

fn arity_error(func: &str, expected: usize, actual: &[ExprRef]) -> Result<Value, MatzoError> {
    let msg = format!(
        "`{}`: expected {} arguments, got {}",
        func,
        expected,
        actual.len()
    );
    if actual.is_empty() {
        Err(MatzoError::new(Span::empty(), msg))
    } else {
        let span = Span {
            start: actual[0].span.start,
            end: actual[actual.len() - 1].span.end,
        };
        Err(MatzoError::new(span, msg))
    }
}

/// The list of builtins provided at startup.
pub fn builtins() -> Vec<BuiltinFunc> {
    vec![
        BuiltinFunc {
            name: "rep",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [rep, expr] = exprs {
                        let mut buf = String::new();
                        let num = state.eval(*rep, env)?.as_num(&state.ast.borrow())?;
                        for _ in 0..num {
                            buf.push_str(state.eval(*expr, env)?.as_str(&state.ast.borrow())?);
                        }
                        Ok(Value::Lit(Literal::Str(buf)))
                    } else {
                        arity_error("rep", 2, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "str/upper",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [expr] = exprs {
                        let s = state.eval(*expr, env)?;
                        Ok(Value::Lit(Literal::Str(
                            s.as_str(&state.ast.borrow())?.to_uppercase(),
                        )))
                    } else {
                        arity_error("str/upper", 1, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "str/capitalize",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [expr] = exprs {
                        let s = state.eval(*expr, env)?;
                        Ok(Value::Lit(Literal::Str(titlecase::titlecase(
                            s.as_str(&state.ast.borrow())?,
                        ))))
                    } else {
                        arity_error("str/capitalize", 1, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "str/lower",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [expr] = exprs {
                        let s = state.eval(*expr, env)?;
                        Ok(Value::Lit(Literal::Str(
                            s.as_str(&state.ast.borrow())?.to_lowercase(),
                        )))
                    } else {
                        arity_error("str/lower", 1, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "add",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [x, y] = exprs {
                        let x = state.eval(*x, env)?.as_num(&state.ast.borrow())?;
                        let y = state.eval(*y, env)?.as_num(&state.ast.borrow())?;
                        Ok(Value::Lit(Literal::Num(x + y)))
                    } else {
                        arity_error("add", 2, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "sub",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [x, y] = exprs {
                        let x = state.eval(*x, env)?.as_num(&state.ast.borrow())?;
                        let y = state.eval(*y, env)?.as_num(&state.ast.borrow())?;
                        Ok(Value::Lit(Literal::Num(x - y)))
                    } else {
                        arity_error("sub", 2, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "mul",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [x, y] = exprs {
                        let x = state.eval(*x, env)?.as_num(&state.ast.borrow())?;
                        let y = state.eval(*y, env)?.as_num(&state.ast.borrow())?;
                        Ok(Value::Lit(Literal::Num(x * y)))
                    } else {
                        arity_error("mul", 2, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/len",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [expr] = exprs {
                        let tup = state.eval(*expr, env)?;
                        Ok(Value::Lit(Literal::Num(
                            tup.as_tup(&state.ast.borrow())?.len() as i64,
                        )))
                    } else {
                        arity_error("tuple/len", 1, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/concat",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [expr] = exprs {
                        let val = state.eval(*expr, env)?;
                        let tup = val.as_tup(&state.ast.borrow())?;
                        let mut contents = Vec::new();
                        for elem in tup {
                            for th in state.hnf(elem)?.as_tup(&state.ast.borrow())? {
                                contents.push(th.clone());
                            }
                        }
                        Ok(Value::Tup(contents))
                    } else {
                        arity_error("tuple/concat", 1, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/index",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [tup, idx] = exprs {
                        let tup = state.eval(*tup, env)?;
                        let idx = state.eval(*idx, env)?;
                        state.hnf(
                            &tup.as_tup(&state.ast.borrow())?
                                [idx.as_num(&state.ast.borrow())? as usize],
                        )
                    } else {
                        arity_error("tuple/index", 1, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/replace",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [tup, idx, new] = exprs {
                        let tup_val = state.eval(*tup, env)?;
                        let tup = tup_val.as_tup(&state.ast.borrow())?;
                        let idx = state.eval(*idx, env)?.as_num(&state.ast.borrow())?;

                        let mut modified = Vec::with_capacity(tup.len());
                        for i in 0..idx {
                            modified.push(tup[i as usize].clone());
                        }
                        modified.push(Thunk::Expr(*new, env.clone()));
                        for i in (idx + 1)..(tup.len() as i64) {
                            modified.push(tup[i as usize].clone());
                        }
                        Ok(Value::Tup(modified))
                    } else {
                        arity_error("tuple/replace", 3, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/fold",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [func, init, tup] = exprs {
                        let func = state.eval(*func, env)?;
                        let tup = state.eval(*tup, env)?;

                        let mut result = Thunk::Expr(*init, env.clone());
                        for t in tup.as_tup(&state.ast.borrow())? {
                            result = Thunk::Value(state.eval_closure(
                                func.as_closure(&state.ast.borrow())?,
                                vec![result, t.clone()],
                            )?);
                        }

                        state.hnf(&result)
                    } else {
                        arity_error("tuple/fold", 3, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/map",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [func, tup] = exprs {
                        let func = state.eval(*func, env)?;
                        let tup = state.eval(*tup, env)?;

                        let mut new_tup = Vec::new();
                        let closure = func.as_closure(&state.ast.borrow())?;
                        for t in tup.as_tup(&state.ast.borrow())? {
                            new_tup
                                .push(Thunk::Value(state.eval_closure(closure, vec![t.clone()])?));
                        }

                        Ok(Value::Tup(new_tup))
                    } else {
                        arity_error("tuple/map", 2, exprs)
                    }
                },
            ),
        },
    ]
}
