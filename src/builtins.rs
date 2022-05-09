use crate::ast::*;
use crate::interp::*;

use anyhow::{bail, Error};

/// The list of builtins provided at startup.
///
/// TODO: move this to a separate file and clean it up
pub fn builtins() -> Vec<BuiltinFunc> {
    vec![
        BuiltinFunc {
            name: "rep",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let (rep, expr) = {
                    let ast = state.ast.borrow();
                    let args = match &ast[expr] {
                        Expr::Tup(tup) => tup,
                        _ => {
                            let span = state.ast.borrow().get_line(expr.file, expr.span);
                            bail!("`rep`: expected tuple\n{}", span)
                        }
                    };
                    if args.len() != 2 {
                        let span = state.ast.borrow().get_line(expr.file, expr.span);
                        bail!(
                            "`rep`: expected two arguments, got {}\n{}",
                            args.len(),
                            span
                        )
                    }
                    (args[0], args[1])
                };
                let mut buf = String::new();
                let num = state.eval(rep, env)?.as_num(&state.ast.borrow())?;
                for _ in 0..num {
                    buf.push_str(
                        &state
                            .eval(expr, env)?
                            .as_str(&state.ast.borrow())?
                            .to_string(),
                    );
                }
                Ok(Value::Lit(Literal::Str(buf)))
            },
        },
        BuiltinFunc {
            name: "length",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let args = match state.eval(expr, env)? {
                    Value::Tup(tup) => tup,
                    _ => bail!("`length`: expected tuple"),
                };
                Ok(Value::Lit(Literal::Num(args.len() as i64)))
            },
        },
        BuiltinFunc {
            name: "to-upper",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let s = state.eval(expr, env)?;
                Ok(Value::Lit(Literal::Str(
                    s.as_str(&state.ast.borrow())?.to_uppercase(),
                )))
            },
        },
        BuiltinFunc {
            name: "capitalize",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let s = state.eval(expr, env)?;
                Ok(Value::Lit(Literal::Str(titlecase::titlecase(
                    s.as_str(&state.ast.borrow())?,
                ))))
            },
        },
        BuiltinFunc {
            name: "to-lower",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let s = state.eval(expr, env)?;
                Ok(Value::Lit(Literal::Str(
                    s.as_str(&state.ast.borrow())?.to_lowercase(),
                )))
            },
        },
        BuiltinFunc {
            name: "sub",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let val = state.eval(expr, env)?;
                let args = val.as_tup(&state.ast.borrow())?;
                if let [x, y] = args {
                    let x = state.hnf(x)?.as_num(&state.ast.borrow())?;
                    let y = state.hnf(y)?.as_num(&state.ast.borrow())?;
                    Ok(Value::Lit(Literal::Num(x - y)))
                } else {
                    bail!("`sub`: expected 2 arguments, got {}", args.len());
                }
            },
        },
        BuiltinFunc {
            name: "concat",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let val = state.eval(expr, env)?;
                let tup = val.as_tup(&state.ast.borrow())?;
                let mut contents = Vec::new();
                for elem in tup {
                    for th in state.hnf(elem)?.as_tup(&state.ast.borrow())? {
                        contents.push(th.clone());
                    }
                }
                Ok(Value::Tup(contents))
            },
        },
        BuiltinFunc {
            name: "tuple-index",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let val = state.eval(expr, env)?;
                let args = val.as_tup(&state.ast.borrow())?;
                if let [tup, idx] = args {
                    let tup = state.hnf(tup)?;
                    let idx = state.hnf(idx)?;
                    state.hnf(
                        &tup.as_tup(&state.ast.borrow())?
                            [idx.as_num(&state.ast.borrow())? as usize],
                    )
                } else {
                    bail!("`tuple-index`: expected 2 arguments, got {}", args.len());
                }
            },
        },
        BuiltinFunc {
            name: "tuple-replace",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let val = state.eval(expr, env)?;
                let args = val.as_tup(&state.ast.borrow())?;
                if let [tup, idx, new] = args {
                    let tup_val = state.hnf(tup)?;
                    let tup = tup_val.as_tup(&state.ast.borrow())?;
                    let idx = state.hnf(idx)?.as_num(&state.ast.borrow())?;

                    let mut modified = Vec::with_capacity(tup.len());
                    for i in 0..idx {
                        modified.push(tup[i as usize].clone());
                    }
                    modified.push(new.clone());
                    for i in (idx + 1)..(tup.len() as i64) {
                        modified.push(tup[i as usize].clone());
                    }
                    Ok(Value::Tup(modified))
                } else {
                    bail!("`tuple-index`: expected 2 arguments, got {}", args.len());
                }
            },
        },
        BuiltinFunc {
            name: "tuple-fold",
            callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
                let val = state.eval(expr, env)?;
                let args = val.as_tup(&state.ast.borrow())?;
                if let [func, init, tup] = args {
                    let func = state.hnf(func)?;
                    let tup = state.hnf(tup)?;

                    let mut result = init.clone();
                    for t in tup.as_tup(&state.ast.borrow())? {
                        let partial =
                            state.eval_closure(func.as_closure(&state.ast.borrow())?, result)?;
                        result =
                            Thunk::Value(state.eval_closure(
                                partial.as_closure(&state.ast.borrow())?,
                                t.clone(),
                            )?);
                    }

                    state.hnf(&result)
                } else {
                    bail!("`tuple-fold`: expected 3 arguments, got {}", args.len());
                }
            },
        },
    ]
}
