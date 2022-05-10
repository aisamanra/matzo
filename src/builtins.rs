use crate::ast::*;
use crate::interp::*;

use anyhow::{bail, Error};

/// The list of builtins provided at startup.
pub fn builtins() -> Vec<BuiltinFunc> {
    vec![
        BuiltinFunc {
            name: "rep",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
                    if let [rep, expr] = exprs {
                        let mut buf = String::new();
                        let num = state.eval(*rep, env)?.as_num(&state.ast.borrow())?;
                        for _ in 0..num {
                            buf.push_str(
                                state
                                    .eval(*expr, env)?
                                    .as_str(&state.ast.borrow())?,
                            );
                        }
                        Ok(Value::Lit(Literal::Str(buf)))
                    } else {
                        bail!("`rep`: expected two arguments, got {}", exprs.len())
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "str/upper",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
                    if let [expr] = exprs {
                        let s = state.eval(*expr, env)?;
                        Ok(Value::Lit(Literal::Str(
                            s.as_str(&state.ast.borrow())?.to_uppercase(),
                        )))
                    } else {
                        bail!(
                            "`str/capitalize`: expected 1 argument1, got {}",
                            exprs.len()
                        );
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "str/capitalize",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
                    if let [expr] = exprs {
                        let s = state.eval(*expr, env)?;
                        Ok(Value::Lit(Literal::Str(titlecase::titlecase(
                            s.as_str(&state.ast.borrow())?,
                        ))))
                    } else {
                        bail!(
                            "`str/capitalize`: expected 1 argument1, got {}",
                            exprs.len()
                        );
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "str/lower",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
                    if let [expr] = exprs {
                        let s = state.eval(*expr, env)?;
                        Ok(Value::Lit(Literal::Str(
                            s.as_str(&state.ast.borrow())?.to_lowercase(),
                        )))
                    } else {
                        bail!("`str/lower`: expected 1 argument1, got {}", exprs.len());
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "add",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
                    if let [x, y] = exprs {
                        let x = state.eval(*x, env)?.as_num(&state.ast.borrow())?;
                        let y = state.eval(*y, env)?.as_num(&state.ast.borrow())?;
                        Ok(Value::Lit(Literal::Num(x + y)))
                    } else {
                        bail!("`add`: expected 2 arguments, got {}", exprs.len());
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "sub",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
                    if let [x, y] = exprs {
                        let x = state.eval(*x, env)?.as_num(&state.ast.borrow())?;
                        let y = state.eval(*y, env)?.as_num(&state.ast.borrow())?;
                        Ok(Value::Lit(Literal::Num(x - y)))
                    } else {
                        bail!("`sub`: expected 2 arguments, got {}", exprs.len());
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "mul",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
                    if let [x, y] = exprs {
                        let x = state.eval(*x, env)?.as_num(&state.ast.borrow())?;
                        let y = state.eval(*y, env)?.as_num(&state.ast.borrow())?;
                        Ok(Value::Lit(Literal::Num(x * y)))
                    } else {
                        bail!("`mul`: expected 2 arguments, got {}", exprs.len());
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/len",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
                    if let [expr] = exprs {
                        let tup = state.eval(*expr, env)?;
                        Ok(Value::Lit(Literal::Num(
                            tup.as_tup(&state.ast.borrow())?.len() as i64,
                        )))
                    } else {
                        bail!("`tuple/len`: expected 1 argument, got {}", exprs.len())
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/concat",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
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
                        bail!("tuple/concat: expected 1 argument, got {}", exprs.len());
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/index",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
                    if let [tup, idx] = exprs {
                        let tup = state.eval(*tup, env)?;
                        let idx = state.eval(*idx, env)?;
                        state.hnf(
                            &tup.as_tup(&state.ast.borrow())?
                                [idx.as_num(&state.ast.borrow())? as usize],
                        )
                    } else {
                        bail!("`tuple/index`: expected 2 arguments, got {}", exprs.len());
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/replace",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
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
                        bail!("`tuple/replace`: expected 3 arguments, got {}", exprs.len());
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/fold",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
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
                        bail!("`tuple/fold`: expected 3 arguments, got {}", exprs.len());
                    }
                },
            ),
        },
        BuiltinFunc {
            name: "tuple/map",
            callback: Box::new(
                |state: &State, exprs: &[ExprRef], env: &Env| -> Result<Value, Error> {
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
                        bail!("`tuple/map`: expected 2 arguments, got {}", exprs.len());
                    }
                },
            ),
        },
    ]
}
