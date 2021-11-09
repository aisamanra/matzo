use crate::ast::*;
use rand::Rng;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Value {
    Lit(Literal),
}

impl Value {
    fn to_string(&self) -> String {
        match self {
            Value::Lit(Literal::Str(s)) => s.clone(),
            Value::Lit(Literal::Atom(s)) => s.clone(),
            Value::Lit(Literal::Num(n)) => format!("{}", n),
        }
    }
}

pub struct State {
    scope: HashMap<String, Expr>,
    rand: rand::rngs::ThreadRng,
}

impl State {
    pub fn new() -> State {
        State {
            scope: HashMap::new(),
            rand: rand::thread_rng(),
        }
    }

    pub fn execute(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Puts(expr) => {
                let val = self.eval(expr);
                println!("{}", val.to_string());
            }
            Stmt::Assn(name, expr) => {
                self.scope.insert(name.to_string(), expr.clone());
            }
            _ => panic!("unimplemented"),
        }
    }

    fn eval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Lit(l) => Value::Lit(l.clone()),
            Expr::Var(v) => {
                let e = if let Some(x) = self.scope.get(v) {
                    x.clone()
                } else {
                    panic!("no such thing: {}", v);
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
