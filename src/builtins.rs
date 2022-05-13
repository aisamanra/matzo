use crate::ast::*;
use crate::core::Loc;
use crate::errors::MatzoError;
use crate::interp::*;

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

/// The list of builtins provided at startup.
pub fn builtins() -> Vec<BuiltinFunc> {
    vec![
        BuiltinFunc {
            name: "rep",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
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
            ),
        },
        BuiltinFunc {
            name: "str/upper",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [expr] = exprs {
                        let s = state.eval(*expr, env)?;
                        Ok(Value::Lit(Literal::Str(
                            s.as_str(&state.ast.borrow(), expr.loc)?.to_uppercase(),
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
                            s.as_str(&state.ast.borrow(), expr.loc)?,
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
                            s.as_str(&state.ast.borrow(), expr.loc)?.to_lowercase(),
                        )))
                    } else {
                        arity_error("str/lower", 1, exprs)
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "wd",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    let mut buf = String::new();
                    for expr in exprs {
                        let s = state.eval(*expr, env)?;
                        buf.push_str(s.as_str(&state.ast.borrow(), expr.loc)?);
                    }
                    Ok(Value::Lit(Literal::Str(buf)))
                },
            ),
        },
        BuiltinFunc {
            name: "se",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    let mut buf = String::new();
                    let mut capitalized = false;
                    let mut last_char = '\0';
                    for expr in exprs.iter() {
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
                            && s.chars().next().map_or(false, |c| c.is_alphanumeric())
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
            ),
        },
        BuiltinFunc {
            name: "add",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [x, y] = exprs {
                        let x = state.eval(*x, env)?.as_num(&state.ast.borrow(), x.loc)?;
                        let y = state.eval(*y, env)?.as_num(&state.ast.borrow(), y.loc)?;
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
                        let x = state.eval(*x, env)?.as_num(&state.ast.borrow(), x.loc)?;
                        let y = state.eval(*y, env)?.as_num(&state.ast.borrow(), y.loc)?;
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
                        let x = state.eval(*x, env)?.as_num(&state.ast.borrow(), x.loc)?;
                        let y = state.eval(*y, env)?.as_num(&state.ast.borrow(), y.loc)?;
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
                            tup.as_tup(&state.ast.borrow(), expr.loc)?.len() as i64,
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
            ),
        },
        BuiltinFunc {
            name: "tuple/index",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [tup_e, idx_e] = exprs {
                        let tup = state.eval(*tup_e, env)?;
                        let idx = state.eval(*idx_e, env)?;
                        state.hnf(
                            &tup.as_tup(&state.ast.borrow(), tup_e.loc)?
                                [idx.as_num(&state.ast.borrow(), idx_e.loc)? as usize],
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
                        let tup = tup_val.as_tup(&state.ast.borrow(), tup.loc)?;
                        let idx = state
                            .eval(*idx, env)?
                            .as_num(&state.ast.borrow(), idx.loc)?;

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
            ),
        },
        BuiltinFunc {
            name: "tuple/map",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, MatzoError> {
                    if let [func_e, tup_e] = exprs {
                        let func = state.eval(*func_e, env)?;
                        let tup = state.eval(*tup_e, env)?;

                        let mut new_tup = Vec::new();
                        let closure = func.as_closure(&state.ast.borrow(), func_e.loc)?;
                        for t in tup.as_tup(&state.ast.borrow(), tup_e.loc)? {
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
