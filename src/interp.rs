use crate::ast::*;
use rand::Rng;
use std::collections::HashMap;
use std::fmt;

macro_rules! bail {
    ($fmt:expr) => { return Err(Error { message: format!($fmt), }) };
    ($fmt:expr, $($e:expr),*) => { return Err(Error { message: format!($fmt, $($e),*), }) }
}

#[derive(Debug)]
pub enum Value {
    Lit(Literal),
    Tup(Vec<Value>),
    Builtin(&'static BuiltinFunc),
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
            Value::Lit(Literal::Str(s)) => f(s),
            Value::Lit(Literal::Atom(s)) => f(s),
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
    callback: &'static dyn Fn(&mut State, &Expr) -> Result<Value, Error>,
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
        callback: &|state: &mut State, expr: &Expr| -> Result<Value, Error> {
            let args = match expr {
                Expr::Tup(tup) => tup,
                _ => bail!("`rep`: expected tuple"),
            };
            if args.len() != 2 {
                bail!("`rep`: expected two arguments, got {}", args.len())
            }
            let num = state.eval(&args[0])?.as_num()?;
            let rep = (0..num).map(|_| args[1].clone()).collect();
            state.eval(&Expr::Cat(rep))
        },
    },
    BuiltinFunc {
        name: "length",
        callback: &|_state: &mut State, expr: &Expr| -> Result<Value, Error> {
            let args = match expr {
                Expr::Tup(tup) => tup,
                _ => bail!("`length`: expected tuple"),
            };
            Ok(Value::Lit(Literal::Num(args.len() as i64)))
        },
    },
    BuiltinFunc {
        name: "to-upper",
        callback: &|state: &mut State, expr: &Expr| -> Result<Value, Error> {
            let s = state.eval(expr)?;
            Ok(Value::Lit(Literal::Str(s.as_str()?.to_uppercase())))
        },
    },
    BuiltinFunc {
        name: "to-lower",
        callback: &|state: &mut State, expr: &Expr| -> Result<Value, Error> {
            let s = state.eval(expr)?;
            Ok(Value::Lit(Literal::Str(s.as_str()?.to_lowercase())))
        },
    },
];

impl fmt::Debug for BuiltinFunc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "BuiltinFunc {{ name: {:?}, ... }}", self.name)
    }
}

enum NamedItem {
    Expr(Expr),
    Builtin(&'static BuiltinFunc),
}

pub struct State {
    scope: HashMap<String, NamedItem>,
    rand: rand::rngs::ThreadRng,
    parser: crate::grammar::StmtsParser,
}

impl Default for State {
    fn default() -> State {
        Self::new()
    }
}

impl State {
    pub fn new() -> State {
        let mut s = State {
            scope: HashMap::new(),
            rand: rand::thread_rng(),
            parser: crate::grammar::StmtsParser::new(),
        };
        for builtin in BUILTINS {
            s.scope
                .insert(builtin.name.to_string(), NamedItem::Builtin(builtin));
        }
        s
    }

    pub fn run(&mut self, src: &str) -> Result<(), Error> {
        let lexed = crate::lexer::tokens(src);
        let stmts = self.parser.parse(lexed)?;
        for stmt in stmts {
            self.execute(&stmt)?;
        }
        Ok(())
    }

    pub fn run_repl(&mut self, src: &str) -> Result<(), Error> {
        let lexed = crate::lexer::tokens(src);
        let stmts = match self.parser.parse(lexed) {
            Ok(stmts) => stmts,
            Err(err) => {
                let with_puts = format!("puts {}", src);
                let lexed = crate::lexer::tokens(&with_puts);
                if let Ok(stmts) = self.parser.parse(lexed) {
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
        for name in self.scope.keys() {
            if name.starts_with(fragment) {
                possibilities.push(name.clone());
            }
        }
        if at_beginning && "puts".starts_with(fragment) {
            possibilities.push("puts ".to_owned());
        }
        possibilities
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Puts(expr) => {
                let val = self.eval(expr)?;
                println!("{}", val.to_string());
            }
            Stmt::Assn(name, expr) => {
                self.scope
                    .insert(name.to_string(), NamedItem::Expr(expr.clone()));
            }
            Stmt::LitAssn(name, strs) => {
                let choices = strs
                    .iter()
                    .map(|s| Choice {
                        weight: None,
                        value: Expr::Lit(Literal::Str(s.clone())),
                    })
                    .collect();
                self.scope
                    .insert(name.to_string(), NamedItem::Expr(Expr::Chc(choices)));
            }
            _ => bail!("unimplemented"),
        }
        Ok(())
    }

    fn eval(&mut self, expr: &Expr) -> Result<Value, Error> {
        match expr {
            Expr::Lit(l) => Ok(Value::Lit(l.clone())),
            Expr::Var(v) => {
                let e = match self.scope.get(v) {
                    Some(NamedItem::Expr(e)) => e.clone(),
                    Some(NamedItem::Builtin(b)) => return Ok(Value::Builtin(b)),
                    None => bail!("no such thing: {}", v),
                };
                self.eval(&e)
            }
            Expr::Cat(cat) => {
                if cat.len() == 1 {
                    self.eval(&cat[0])
                } else {
                    let mut buf = String::new();
                    for expr in cat {
                        let val = self.eval(expr)?;
                        buf.push_str(&val.to_string());
                    }
                    Ok(Value::Lit(Literal::Str(buf)))
                }
            }
            Expr::Chc(choices) => {
                if choices.len() == 1 {
                    self.eval(&choices[0].value)
                } else {
                    self.choose(choices)
                }
            }
            Expr::Tup(values) => Ok(Value::Tup(
                values
                    .iter()
                    .map(|v| self.eval(v))
                    .collect::<Result<Vec<Value>, Error>>()?,
            )),
            Expr::Ap(fun, arg) => match self.eval(fun)? {
                Value::Builtin(builtin) => (builtin.callback)(self, arg),
                _ => bail!("bad function: {:?}", fun),
            },
            Expr::Range(from, to) => {
                let from = self.eval(from)?.as_num()?;
                let to = self.eval(to)?.as_num()?;
                Ok(Value::Lit(Literal::Num(self.rand.gen_range(from..=to))))
            }
            _ => bail!("unimplemented: {:?}", expr),
        }
    }

    fn choose(&mut self, choices: &[Choice]) -> Result<Value, Error> {
        let max = choices.iter().map(Choice::weight).sum();
        let mut choice = self.rand.gen_range(0..max);
        for ch in choices {
            if choice < ch.weight() {
                return self.eval(&ch.value);
            }
            choice -= ch.weight();
        }
        bail!("unreachable")
    }
}
