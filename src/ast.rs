type Name = String;

#[derive(Debug, Clone)]
pub enum Stmt {
    Puts(Expr),
    Fix(Name),
    Assn(Name, Expr),
    LitAssn(Name, Vec<String>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Name),
    Cat(Vec<Expr>),
    Chc(Vec<Choice>),
    Rep(i64, Box<Expr>),
    Lit(Literal),
    Ap(Box<Expr>, Box<Expr>),
    Tup(Vec<Expr>),
    Let(Name, Box<Expr>, Box<Expr>),
    Fun(Vec<Case>),
    Case(Box<Expr>, Vec<Case>),
    Range(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Case {
    pub pat: Pat,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Pat {
    Var(Name),
    Lit(Literal),
    Tup(Vec<Pat>),
}

#[derive(Debug, Clone)]
pub struct Choice {
    pub weight: Option<i64>,
    pub value: Expr,
}

impl Choice {
    pub fn weight(&self) -> i64 {
        self.weight.unwrap_or(1)
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Str(String),
    Atom(String),
    Num(i64),
}
