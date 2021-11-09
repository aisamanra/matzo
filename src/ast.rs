type Name = String;

#[derive(Debug)]
pub enum Stmt {
    Puts(Expr),
    Fix(Name),
    Assn(Name, Expr),
    LitAssn(Name, Vec<String>),
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct Case {
    pub pat: Pat,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Pat {
    Var(Name),
    Lit(Literal),
    Tup(Vec<Pat>),
}

#[derive(Debug)]
pub struct Choice {
    pub weight: Option<i64>,
    pub value: Expr,
}

#[derive(Debug)]
pub enum Literal {
    Str(String),
    Atom(String),
    Num(i64),
}

impl Literal {
    pub fn from_str_literal(s: &str) -> Literal {
        // strip the outer pieces from the string
        let mut buf = String::new();
        let mut src = s[1..s.len() - 1].chars().into_iter();
        while let Some(c) = src.next() {
            if c == '\\' {
                match src.next() {
                    Some('n') => buf.push('\n'),
                    Some('t') => buf.push('\t'),
                    Some('r') => buf.push('\r'),
                    Some(c) => buf.push(c),
                    None => panic!("bad escape"),
                }
            } else {
                buf.push(c);
            }
        }
        Literal::Str(buf)
    }
}
