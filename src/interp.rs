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
    Tup(Vec<Value>),
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
                    buf.push_str(&val.to_string());
                }
                buf.push('>');
                f(&buf)
            }
            Value::Builtin(func) => f(&format!("#<builtin {}>", func.name)),
            Value::Closure(_) => f(&format!("#<lambda ...>")),
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
];

impl fmt::Debug for BuiltinFunc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "BuiltinFunc {{ name: {:?}, ... }}", self.name)
    }
}

#[derive(Debug, Clone)]
enum NamedItem {
    Expr(ExprRef),
    Value(Value),
    Builtin(&'static BuiltinFunc),
}

type Env = Option<Rc<Scope>>;
#[derive(Debug)]
pub struct Scope {
    vars: HashMap<Name, NamedItem>,
    parent: Env,
}

#[derive(Debug, Clone)]
pub struct Closure {
    expr: ExprRef,
    scope: Env,
}

pub struct State {
    ast: RefCell<ASTArena>,
    root_scope: RefCell<HashMap<Name, NamedItem>>,
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
                .insert(sym, NamedItem::Builtin(builtin));
        }
        s
    }

    fn lookup(&self, env: &Env, name: Name) -> Result<NamedItem, Error> {
        if let Some(env) = env {
            if let Some(ne) = env.vars.get(&name) {
                Ok(ne.clone())
            } else {
                self.lookup(&env.parent, name)
            }
        } else {
            match self.root_scope.borrow().get(&name) {
                None => bail!("no such thing: {:?}", name),
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
                println!("{}", val.to_string());
            }

            Stmt::Fix(name) => {
                let expr = match self.lookup(&None, *name)? {
                    NamedItem::Expr(e) => e,
                    // if it's not an expr, then our work here is done
                    _ => return Ok(()),
                };
                let val = self.eval(expr, &None)?;
                self.root_scope.borrow_mut().insert(*name, NamedItem::Value(val));
            }

            Stmt::Assn(fixed, name, expr) => {
                if *fixed {
                    let val = self.eval(*expr, &None)?;
                    self.root_scope.borrow_mut().insert(*name, NamedItem::Value(val));
                } else {
                    self.root_scope
                        .borrow_mut()
                        .insert(*name, NamedItem::Expr(*expr));
                }
            }

            Stmt::LitAssn(fixed, name, strs) => {
                if *fixed {
                    let choice = &strs[self.rand.borrow_mut().gen_range(0..strs.len())];
                    self.root_scope.borrow_mut().insert(
                        *name,
                        NamedItem::Value(Value::Lit(Literal::Str(choice.clone()))),
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
                    .insert(*name, NamedItem::Expr(choices));
            }
        }
        Ok(())
    }

    fn eval(&self, expr: ExprRef, env: &Env) -> Result<Value, Error> {
        let expr = &self.ast.borrow()[expr];
        match expr {
            Expr::Lit(l) => Ok(Value::Lit(l.clone())),
            Expr::Nil => Ok(Value::Nil),
            Expr::Var(v) => {
                let e = match self.lookup(env, *v)? {
                    NamedItem::Expr(e) => e,
                    NamedItem::Value(v) => return Ok(v.clone()),
                    NamedItem::Builtin(b) => return Ok(Value::Builtin(b)),
                };
                self.eval(e, env)
            }
            Expr::Cat(cat) => {
                if cat.len() == 1 {
                    self.eval(cat[0], env)
                } else {
                    let mut buf = String::new();
                    for expr in cat {
                        let val = self.eval(*expr, env)?;
                        buf.push_str(&val.to_string());
                    }
                    Ok(Value::Lit(Literal::Str(buf)))
                }
            }
            Expr::Chc(choices) => {
                if choices.len() == 1 {
                    self.eval(choices[0].value, env)
                } else {
                    self.choose(&choices, env)
                }
            }
            Expr::Tup(values) => Ok(Value::Tup(
                values
                    .iter()
                    .map(|v| self.eval(*v, env))
                    .collect::<Result<Vec<Value>, Error>>()?,
            )),
            Expr::Ap(fun, arg) => match self.eval(*fun, env)? {
                Value::Builtin(builtin) => (builtin.callback)(self, *arg, env),
                _ => bail!("bad function: {:?}", fun),
            },
            Expr::Range(from, to) => {
                let from = self.eval(*from, env)?.as_num()?;
                let to = self.eval(*to, env)?.as_num()?;
                Ok(Value::Lit(Literal::Num(
                    self.rand.borrow_mut().gen_range(from..=to),
                )))
            }
            _ => bail!("unimplemented: {:?}", expr),
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
