use std::fmt;

pub type Name = string_interner::DefaultSymbol;

pub struct ASTArena {
    strings: string_interner::StringInterner,
}

pub struct Debuggable<'a, T> {
    arena: &'a ASTArena,
    value: &'a T,
}

impl<'a> std::fmt::Debug for Debuggable<'a, Stmt> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.arena.show_stmt(self.value, f)
    }
}

impl ASTArena {
    pub fn new() -> ASTArena {
        ASTArena {
            strings: string_interner::StringInterner::new(),
        }
    }

    pub fn add_string(&mut self, s: &str) -> Name {
        self.strings.get_or_intern(s)
    }

    pub fn show_stmt(&self, stmt: &Stmt, f: &mut fmt::Formatter) -> fmt::Result {
        match stmt {
            Stmt::Puts(expr) =>
                writeln!(f, "{:?}", expr),
            Stmt::Fix(name) =>
                writeln!(f, "Fix({})", &self[*name]),
            Stmt::Assn(name, expr) =>
                writeln!(f, "Assn(\n  {},\n  {:?}\n)", &self[*name], expr),
            Stmt::LitAssn(name, strs) => {
                write!(f, "LitAssn({}, [ ", &self[*name])?;
                for str in strs.iter() {
                    write!(f, " {} ", str)?;
                }
                writeln!(f, "]")
            }
        }
    }
}

impl std::ops::Index<string_interner::DefaultSymbol> for ASTArena {
    type Output = str;

    fn index(&self, sf: string_interner::DefaultSymbol) -> &str {
        self.strings.resolve(sf).unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Puts(Expr),
    Fix(Name),
    Assn(Name, Expr),
    LitAssn(Name, Vec<String>),
}

impl Stmt {
    pub fn show<'a>(&'a self, ast: &'a ASTArena) -> Debuggable<'a, Stmt> {
        Debuggable {
            arena: ast,
            value: self,
        }
    }
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
    Atom(Name),
    Num(i64),
}
