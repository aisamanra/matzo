use crate::ast::*;
use rand::Rng;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub enum Value {
    Lit(Literal),
    Tup(Vec<Value>),
    Builtin(&'static BuiltinFunc),
}

pub struct BuiltinFunc {
    name: &'static str,
    callback: &'static dyn Fn(&mut State, &Expr) -> Value,
}

const BUILTINS: &[BuiltinFunc] = &[
    BuiltinFunc {
        name: "rep",
        callback: &|state: &mut State, expr: &Expr| -> Value {
            let args = match expr {
                Expr::Tup(tup) => tup,
                _ => panic!("`rep`: expected tuple"),
            };
            if args.len() != 2 {
                panic!("`rep`: expected two arguments, got {}", args.len())
            }
            let num = match state.eval(&args[0]) {
                Value::Lit(Literal::Num(n)) => n,
                r => panic!("`rep`: expected first arg to be a number, but got {:?}", r),
            };

            let rep = (0..num).map(|_| args[1].clone()).collect();
            state.eval(&Expr::Cat(rep))
        }
    },
];

impl fmt::Debug for BuiltinFunc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(fmt, "BuiltinFunc {{ name: {:?}, ... }}", self.name)
    }
}

impl Value {
    fn to_string(&self) -> String {
        match self {
            Value::Lit(Literal::Str(s)) => s.clone(),
            Value::Lit(Literal::Atom(s)) => s.clone(),
            Value::Lit(Literal::Num(n)) => format!("{}", n),
            Value::Tup(values) => {
                let mut buf = String::new();
                buf.push_str("<");
                for (i, val) in values.iter().enumerate() {
                    if i > 0 {
                        buf.push_str(", ");
                    }
                    buf.push_str(&val.to_string());
                }
                buf.push_str(">");
                buf
            }
            Value::Builtin(func) =>
                format!("#<builtin {}>", func.name),
        }
    }
}

enum NamedItem {
    Expr(Expr),
    Builtin(&'static BuiltinFunc),
}

pub struct State {
    scope: HashMap<String, NamedItem>,
    rand: rand::rngs::ThreadRng,
}

impl State {
    pub fn new() -> State {
        let mut s = State {
            scope: HashMap::new(),
            rand: rand::thread_rng(),
        };
        s.init_builtins();
        s
    }

    fn init_builtins(&mut self) {
        for builtin in BUILTINS {
            self.scope.insert(builtin.name.to_string(), NamedItem::Builtin(builtin));
        }
    }

    pub fn execute(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Puts(expr) => {
                let val = self.eval(expr);
                println!("{}", val.to_string());
            }
            Stmt::Assn(name, expr) => {
                self.scope.insert(name.to_string(), NamedItem::Expr(expr.clone()));
            }
            Stmt::LitAssn(name, strs) => {
                let choices = strs.iter().map(|s| Choice {
                    weight: None,
                    value: Expr::Lit(Literal::Str(s.clone())),
                }).collect();
                self.scope.insert(name.to_string(), NamedItem::Expr(Expr::Chc(choices)));
            }
            _ => panic!("unimplemented"),
        }
    }

    fn eval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Lit(l) => Value::Lit(l.clone()),
            Expr::Var(v) => {
                let e = match self.scope.get(v) {
                    Some(NamedItem::Expr(e)) =>
                        e.clone(),
                    Some(NamedItem::Builtin(b)) =>
                        return Value::Builtin(b),
                    None =>
                        panic!("no such thing: {}", v),
                };
                self.eval(&e)
            }
            Expr::Cat(cat) => {
                if cat.len() == 1 {
                    self.eval(&cat[0])
                } else {
                    let mut buf = String::new();
                    for expr in cat {
                        let val = self.eval(expr);
                        buf.push_str(&val.to_string());
                    }
                    Value::Lit(Literal::Str(buf))
                }
            }
            Expr::Chc(choices) => {
                if choices.len() == 1 {
                    self.eval(&choices[0].value)
                } else {
                    self.choose(choices)
                }
            }
            Expr::Tup(values) =>
                Value::Tup(values.iter().map(|v| self.eval(v)).collect()),
            Expr::Ap(fun, arg) => {
                match self.eval(fun) {
                    Value::Builtin(builtin) => (builtin.callback)(self, arg),
                    _ => panic!("bad function: {:?}", fun),
                }
            }
            Expr::Range(from, to) => {
                let from = match self.eval(from) {
                    Value::Lit(Literal::Num(n)) => n,
                    e => panic!("bad start in range: {}", e.to_string()),
                };
                let to = match self.eval(to) {
                    Value::Lit(Literal::Num(n)) => n,
                    e => panic!("bad end in range: {}", e.to_string()),
                };
                Value::Lit(Literal::Num(self.rand.gen_range(from..=to)))
            }
            _ => panic!("unimplemented: {:?}", expr),
        }
    }

    fn choose(&mut self, choices: &[Choice]) -> Value {
        let max = choices.iter().map(Choice::weight).sum();
        let mut choice = self.rand.gen_range(0..max);
        for ch in choices {
            if choice < ch.weight() {
                return self.eval(&ch.value);
            }
            choice -= ch.weight();
        }
        panic!("unreachable")
    }
}
