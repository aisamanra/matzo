use crate::ast::*;
use rand::Rng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

macro_rules! bail {
    ($fmt:expr) => { return Err(Error { message: format!($fmt), }) };
    ($fmt:expr, $($e:expr),*) => { return Err(Error { message: format!($fmt, $($e),*), }) }
}

#[derive(Debug, Clone)]
pub enum Value {
    Lit(Literal),
    Tup(Vec<Thunk>),
    Builtin(&'static BuiltinFunc),
    Closure(Closure),
    Nil,
}

impl Value {
    fn as_num(&self) -> Result<i64, Error> {
        match self {
            Value::Lit(Literal::Num(n)) => Ok(*n),
            _ => self.with_str(|s| bail!("Expected number, got {}", s)),
        }
    }

    fn as_str(&self) -> Result<&str, Error> {
        match self {
            Value::Lit(Literal::Str(s)) => Ok(s),
            _ => self.with_str(|s| bail!("Expected string, got {}", s)),
        }
    }

    fn as_tup(&self) -> Result<&[Thunk], Error> {
        match self {
            Value::Tup(vals) => Ok(vals),
            _ => self.with_str(|s| bail!("Expected tuple, got {}", s)),
        }
    }

    fn as_closure(&self) -> Result<&Closure, Error> {
        match self {
            Value::Closure(closure) => Ok(closure),
            _ => self.with_str(|s| bail!("Expected tuple, got {}", s)),
        }
    }

    fn with_str<U>(&self, f: impl FnOnce(&str) -> U) -> U {
        match self {
            Value::Nil => f(""),
            Value::Lit(Literal::Str(s)) => f(s),
            Value::Lit(Literal::Atom(s)) => f(&format!("{:?}", s)),
            Value::Lit(Literal::Num(n)) => f(&format!("{}", n)),
            Value::Tup(values) => {
                let mut buf = String::new();
                buf.push('<');
                for (i, val) in values.iter().enumerate() {
                    if i > 0 {
                        buf.push_str(", ");
                    }
                    match val {
                        Thunk::Value(v) => buf.push_str(&v.to_string()),
                        Thunk::Expr(..) => buf.push_str("#<unevaluated>"),
                        Thunk::Builtin(func) => buf.push_str(&format!("#<builtin {}>", func.name)),
                    }
                }
                buf.push('>');
                f(&buf)
            }
            Value::Builtin(func) => f(&format!("#<builtin {}>", func.name)),
            Value::Closure(_) => f("#<lambda ...>"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.with_str(|s| write!(f, "{}", s))
    }
}

pub struct BuiltinFunc {
    name: &'static str,
    callback: &'static dyn Fn(&State, ExprRef, &Env) -> Result<Value, Error>,
}

#[derive(Debug)]
pub struct Error {
    message: String,
}

impl From<lalrpop_util::ParseError<usize, crate::lexer::Token<'_>, crate::lexer::LexerError>>
    for Error
{
    fn from(
        err: lalrpop_util::ParseError<usize, crate::lexer::Token<'_>, crate::lexer::LexerError>,
    ) -> Error {
        Error {
            message: format!("{:?}", err),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.message)
    }
}

impl std::error::Error for Error {}

const BUILTINS: &[BuiltinFunc] = &[
    BuiltinFunc {
        name: "rep",
        callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
            let (rep, expr) = {
                let ast = state.ast.borrow();
                let args = match &ast[expr] {
                    Expr::Tup(tup) => tup,
                    _ => bail!("`rep`: expected tuple"),
                };
                if args.len() != 2 {
                    bail!("`rep`: expected two arguments, got {}", args.len())
                }
                (args[0], args[1])
            };
            let mut buf = String::new();
            let num = state.eval(rep, env)?.as_num()?;
            for _ in 0..num {
                buf.push_str(&state.eval(expr, env)?.as_str()?.to_string());
            }
            Ok(Value::Lit(Literal::Str(buf)))
        },
    },
    BuiltinFunc {
        name: "length",
        callback: &|state: &State, expr: ExprRef, _env: &Env| -> Result<Value, Error> {
            let ast = state.ast.borrow();
            let args = match &ast[expr] {
                Expr::Tup(tup) => tup,
                _ => bail!("`length`: expected tuple"),
            };
            Ok(Value::Lit(Literal::Num(args.len() as i64)))
        },
    },
    BuiltinFunc {
        name: "to-upper",
        callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
            let s = state.eval(expr, env)?;
            Ok(Value::Lit(Literal::Str(s.as_str()?.to_uppercase())))
        },
    },
    BuiltinFunc {
        name: "to-lower",
        callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
            let s = state.eval(expr, env)?;
            Ok(Value::Lit(Literal::Str(s.as_str()?.to_lowercase())))
        },
    },
    BuiltinFunc {
        name: "concat",
        callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
            let val = state.eval(expr, env)?;
            let tup = val.as_tup()?;
            let mut contents = Vec::new();
            for elem in tup {
                for th in state.hnf(elem)?.as_tup()? {
                    contents.push(th.clone());
                }
            }
            Ok(Value::Tup(contents))
        },
    },
    BuiltinFunc {
        name: "tuple-fold",
        callback: &|state: &State, expr: ExprRef, env: &Env| -> Result<Value, Error> {
            let val = state.eval(expr, env)?;
            let args = val.as_tup()?;
            if args.len() != 3 {
                bail!("`tuple-fold`: expected 3 arguments, got {}", args.len());
            }

            let func = &args[0];
            let init = &args[1];
            let tup = &args[2];

            let func = state.hnf(func)?;
            let tup = state.hnf(tup)?;

            let mut result = init.clone();
            for t in tup.as_tup()? {
                let partial = state.eval_closure(func.as_closure()?, result)?;
                result = Thunk::Value(state.eval_closure(partial.as_closure()?, t.clone())?);
            }

            state.hnf(&result)
        },
    },
];

impl fmt::Debug for BuiltinFunc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "BuiltinFunc {{ name: {:?}, ... }}", self.name)
    }
}

#[derive(Debug, Clone)]
pub enum Thunk {
    Expr(ExprRef, Env),
    Value(Value),
    Builtin(&'static BuiltinFunc),
}

type Env = Option<Rc<Scope>>;
#[derive(Debug)]
pub struct Scope {
    vars: HashMap<Name, Thunk>,
    parent: Env,
}

#[derive(Debug, Clone)]
pub struct Closure {
    func: ExprRef,
    scope: Env,
}

pub struct State {
    ast: RefCell<ASTArena>,
    root_scope: RefCell<HashMap<Name, Thunk>>,
    rand: RefCell<rand::rngs::ThreadRng>,
    parser: crate::grammar::StmtsParser,
}

impl Default for State {
    fn default() -> State {
        Self::new()
    }
}

impl State {
    pub fn new() -> State {
        let s = State {
            root_scope: RefCell::new(HashMap::new()),
            rand: RefCell::new(rand::thread_rng()),
            parser: crate::grammar::StmtsParser::new(),
            ast: RefCell::new(ASTArena::new()),
        };
        for builtin in BUILTINS {
            let sym = s.ast.borrow_mut().add_string(builtin.name);
            s.root_scope
                .borrow_mut()
                .insert(sym, Thunk::Builtin(builtin));
        }
        s
    }

    fn lookup(&self, env: &Env, name: Name) -> Result<Thunk, Error> {
        if let Some(env) = env {
            if let Some(ne) = env.vars.get(&name) {
                Ok(ne.clone())
            } else {
                self.lookup(&env.parent, name)
            }
        } else {
            match self.root_scope.borrow().get(&name) {
                None => bail!("no such thing: {}", &self.ast.borrow()[name]),
                Some(ne) => Ok(ne.clone()),
            }
        }
    }

    pub fn run(&self, src: &str) -> Result<(), Error> {
        let lexed = crate::lexer::tokens(src);
        let stmts = self.parser.parse(&mut self.ast.borrow_mut(), lexed)?;
        for stmt in stmts {
            self.execute(&stmt)?;
        }
        Ok(())
    }

    pub fn run_repl(&self, src: &str) -> Result<(), Error> {
        let lexed = crate::lexer::tokens(src);
        let stmts = {
            let mut ast = self.ast.borrow_mut();
            self.parser.parse(&mut ast, lexed)
        };
        let stmts = match stmts {
            Ok(stmts) => stmts,
            Err(err) => {
                let with_puts = format!("puts {}", src);
                let lexed = crate::lexer::tokens(&with_puts);
                if let Ok(stmts) = self.parser.parse(&mut self.ast.borrow_mut(), lexed) {
                    stmts
                } else {
                    return Err(err.into());
                }
            }
        };
        for stmt in stmts {
            self.execute(&stmt)?;
        }
        Ok(())
    }

    pub fn autocomplete(&self, fragment: &str, at_beginning: bool) -> Vec<String> {
        let mut possibilities = Vec::new();
        for name in self.root_scope.borrow().keys() {
            if self.ast.borrow()[*name].starts_with(fragment) {
                possibilities.push(self.ast.borrow()[*name].to_string());
            }
        }
        if at_beginning && "puts".starts_with(fragment) {
            possibilities.push("puts ".to_owned());
        }
        possibilities
    }

    pub fn execute(&self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Puts(expr) => {
                let val = self.eval(*expr, &None)?;
                let val = self.force(val)?;
                println!("{}", val.to_string());
            }

            Stmt::Fix(name) => {
                let (expr, env) = match self.lookup(&None, *name)? {
                    Thunk::Expr(e, env) => (e, env),
                    // if it's not an expr, then our work here is done
                    _ => return Ok(()),
                };
                let val = self.eval(expr, &env)?;
                let val = self.force(val)?;
                self.root_scope
                    .borrow_mut()
                    .insert(*name, Thunk::Value(val));
            }

            Stmt::Assn(fixed, name, expr) => {
                if *fixed {
                    let val = self.eval(*expr, &None)?;
                    self.root_scope
                        .borrow_mut()
                        .insert(*name, Thunk::Value(val));
                } else {
                    self.root_scope
                        .borrow_mut()
                        .insert(*name, Thunk::Expr(*expr, None));
                }
            }

            Stmt::LitAssn(fixed, name, strs) => {
                if *fixed {
                    let choice = &strs[self.rand.borrow_mut().gen_range(0..strs.len())];
                    self.root_scope.borrow_mut().insert(
                        *name,
                        Thunk::Value(Value::Lit(Literal::Str(choice.clone()))),
                    );
                    return Ok(());
                }

                let choices = strs
                    .iter()
                    .map(|s| Choice {
                        weight: None,
                        value: self
                            .ast
                            .borrow_mut()
                            .add_expr(Expr::Lit(Literal::Str(s.clone()))),
                    })
                    .collect();
                let choices = self.ast.borrow_mut().add_expr(Expr::Chc(choices));
                self.root_scope
                    .borrow_mut()
                    .insert(*name, Thunk::Expr(choices, None));
            }
        }
        Ok(())
    }

    fn force(&self, val: Value) -> Result<Value, Error> {
        match val {
            Value::Tup(values) => Ok(Value::Tup(
                values
                    .into_iter()
                    .map(|t| {
                        let v = self.hnf(&t)?;
                        let v = self.force(v)?;
                        Ok(Thunk::Value(v))
                    })
                    .collect::<Result<Vec<Thunk>, Error>>()?,
            )),
            _ => Ok(val),
        }
    }

    fn hnf(&self, thunk: &Thunk) -> Result<Value, Error> {
        match thunk {
            Thunk::Expr(expr, env) => self.eval(*expr, env),
            Thunk::Value(val) => Ok(val.clone()),
            Thunk::Builtin(b) => Ok(Value::Builtin(b)),
        }
    }

    fn eval(&self, expr_ref: ExprRef, env: &Env) -> Result<Value, Error> {
        let expr = &self.ast.borrow()[expr_ref];
        match expr {
            Expr::Lit(l) => Ok(Value::Lit(l.clone())),
            Expr::Nil => Ok(Value::Nil),

            Expr::Var(v) => {
                let (e, env) = match self.lookup(env, *v)? {
                    Thunk::Expr(e, env) => (e, env),
                    Thunk::Value(v) => return Ok(v),
                    Thunk::Builtin(b) => return Ok(Value::Builtin(b)),
                };
                self.eval(e, &env)
            }

            Expr::Cat(cat) => {
                if cat.len() == 1 {
                    self.eval(cat[0], env)
                } else {
                    let mut buf = String::new();
                    for expr in cat {
                        let val = self.eval(*expr, env)?;
                        let val = self.force(val)?;
                        buf.push_str(&val.to_string());
                    }
                    Ok(Value::Lit(Literal::Str(buf)))
                }
            }

            Expr::Chc(choices) => {
                if choices.len() == 1 {
                    self.eval(choices[0].value, env)
                } else {
                    self.choose(choices, env)
                }
            }

            Expr::Tup(values) => Ok(Value::Tup(
                values
                    .iter()
                    .map(|v| Thunk::Expr(*v, env.clone()))
                    .collect::<Vec<Thunk>>(),
            )),

            Expr::Range(from, to) => {
                let from = self.eval(*from, env)?.as_num()?;
                let to = self.eval(*to, env)?.as_num()?;
                Ok(Value::Lit(Literal::Num(
                    self.rand.borrow_mut().gen_range(from..=to),
                )))
            }

            Expr::Fun(_) => Ok(Value::Closure(Closure {
                func: expr_ref,
                scope: env.clone(),
            })),

            Expr::Ap(func, val) => match self.eval(*func, env)? {
                Value::Closure(c) => {
                    let scrut = Thunk::Expr(*val, env.clone());
                    self.eval_closure(&c, scrut)
                }
                Value::Builtin(builtin) => (builtin.callback)(self, *val, env),
                _ => bail!("Bad function: {:?}", func),
            },

            _ => bail!("unimplemented: {:?}", expr),
        }
    }

    fn eval_closure(&self, closure: &Closure, mut scrut: Thunk) -> Result<Value, Error> {
        let ast = self.ast.borrow();
        let cases = match &ast[closure.func] {
            Expr::Fun(cases) => cases,
            _ => bail!("INVARIANT FAILED"),
        };

        for c in cases {
            let mut bindings = Vec::new();
            if !self.match_pat(&c.pat, &mut scrut, &mut bindings)? {
                continue;
            }

            let mut new_scope = HashMap::new();
            for (name, binding) in bindings {
                new_scope.insert(name, binding);
            }

            let new_scope = Rc::new(Scope {
                vars: new_scope,
                parent: closure.scope.clone(),
            });
            return self.eval(c.expr, &Some(new_scope));
        }

        bail!("No pattern in {:?} matched {:?}", cases, scrut);
    }

    fn match_pat(
        &self,
        pat: &Pat,
        scrut: &mut Thunk,
        bindings: &mut Vec<(Name, Thunk)>,
    ) -> Result<bool, Error> {
        if let Pat::Var(v) = pat {
            bindings.push((*v, scrut.clone()));
            return Ok(true);
        }
        if let Pat::Wildcard = pat {
            return Ok(true);
        }
        // if it's not just a variable, then we'll need to make sure
        // we've evaluated `scrut` at least one level from here
        if let Thunk::Expr(e, env) = scrut {
            *scrut = Thunk::Value(self.eval(*e, env)?)
        };

        // now we can match deeper patterns, at least a little
        match pat {
            Pat::Lit(lhs) => {
                if let Thunk::Value(Value::Lit(rhs)) = scrut {
                    Ok(lhs == rhs)
                } else {
                    Ok(false)
                }
            }
            Pat::Tup(pats) => {
                if let Thunk::Value(Value::Tup(thunks)) = scrut {
                    if pats.len() != thunks.len() {
                        return Ok(false);
                    }

                    for (p, t) in pats.iter().zip(thunks) {
                        if !self.match_pat(p, t, bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            _ => Ok(false),
        }
    }

    fn choose(&self, choices: &[Choice], env: &Env) -> Result<Value, Error> {
        let max = choices.iter().map(Choice::weight).sum();
        let mut choice = self.rand.borrow_mut().gen_range(0..max);
        for ch in choices {
            if choice < ch.weight() {
                return self.eval(ch.value, env);
            }
            choice -= ch.weight();
        }
        bail!("unreachable")
    }
}
