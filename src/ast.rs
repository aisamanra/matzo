use std::fmt;

pub type Name = string_interner::DefaultSymbol;

pub struct ASTArena {
    strings: string_interner::StringInterner,
    exprs: Vec<Expr>,
}

impl Default for ASTArena {
    fn default() -> Self {
        Self::new()
    }
}

impl ASTArena {
    pub fn new() -> ASTArena {
        ASTArena {
            strings: string_interner::StringInterner::new(),
            exprs: vec![Expr::Nil],
        }
    }

    pub fn expr_nil(&self) -> ExprRef {
        ExprRef { idx: 0 }
    }

    pub fn add_expr(&mut self, e: Expr) -> ExprRef {
        let idx = self.exprs.len();
        self.exprs.push(e);
        ExprRef { idx }
    }

    pub fn add_string(&mut self, s: &str) -> Name {
        self.strings.get_or_intern(s)
    }

    pub fn show_stmt(&self, stmt: &Stmt, f: &mut fmt::Formatter) -> fmt::Result {
        match stmt {
            Stmt::Puts(expr) => {
                write!(f, "Puts ")?;
                self.show_expr(&self[*expr], f, 0)
            }
            Stmt::Fix(name) => writeln!(f, "Fix {}", &self[*name]),
            Stmt::Assn(fixed, name, expr) => {
                write!(
                    f,
                    "Assn {} {} ",
                    if *fixed { "fixed" } else { "" },
                    &self[*name]
                )?;
                self.show_expr(&self[*expr], f, 0)
            }
            Stmt::LitAssn(fixed, name, strs) => {
                write!(
                    f,
                    "LitAssn {} {}, [ ",
                    if *fixed { "fixed" } else { "" },
                    &self[*name],
                )?;
                for str in strs.iter() {
                    write!(f, " {} ", str)?;
                }
                writeln!(f, "]")
            }
        }
    }

    fn indent(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
        for _ in 0..depth {
            write!(f, " ")?;
        }
        Ok(())
    }

    fn show_pat(&self, pat: &Pat, f: &mut fmt::Formatter) -> fmt::Result {
        match pat {
            Pat::Var(n) => write!(f, "{}", &self[*n]),
            Pat::Lit(Literal::Atom(n)) => write!(f, "{}", &self[*n]),
            Pat::Lit(lit) => write!(f, "{:?}", lit),
            Pat::Tup(tup) => {
                write!(f, "Tup( ")?;
                for t in tup {
                    self.show_pat(t, f)?;
                    write!(f, " ")?;
                }
                write!(f, ")")
            }
        }
    }

    fn show_expr(&self, expr: &Expr, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
        match expr {
            Expr::Nil => writeln!(f, "Nil"),
            Expr::Var(v) => writeln!(f, "Var({})", &self[*v]),
            Expr::Lit(Literal::Atom(n)) => writeln!(f, "Lit(Atom({}))", &self[*n]),
            Expr::Lit(lit) => writeln!(f, "{:?}", lit),
            Expr::Range(from, to) => {
                writeln!(f, "Range(")?;
                self.indent(f, depth + 2)?;
                self.show_expr(&self[*from], f, depth + 2)?;
                self.indent(f, depth + 2)?;
                self.show_expr(&self[*to], f, depth + 2)?;
                self.indent(f, depth)?;
                writeln!(f, ")")
            }

            Expr::Ap(func, arg) => {
                writeln!(f, "Ap(")?;
                self.indent(f, depth + 2)?;
                self.show_expr(&self[*func], f, depth + 2)?;
                self.indent(f, depth + 2)?;
                self.show_expr(&self[*arg], f, depth + 2)?;
                self.indent(f, depth)?;
                writeln!(f, ")")
            }

            Expr::Tup(expr) => {
                writeln!(f, "Tup(")?;
                for e in expr {
                    self.indent(f, depth + 2)?;
                    self.show_expr(&self[*e], f, depth + 2)?;
                }
                self.indent(f, depth)?;
                writeln!(f, ")")
            }

            Expr::Cat(expr) => {
                writeln!(f, "Cat(")?;
                for e in expr {
                    self.indent(f, depth + 2)?;
                    self.show_expr(&self[*e], f, depth + 2)?;
                }
                self.indent(f, depth)?;
                writeln!(f, ")")
            }

            Expr::Chc(expr) => {
                writeln!(f, "Chc(")?;
                for e in expr {
                    if let Some(s) = e.weight {
                        self.indent(f, depth + 2)?;
                        writeln!(f, "{}:", s)?;
                        self.indent(f, depth + 4)?;
                        self.show_expr(&self[e.value], f, depth + 4)?;
                    } else {
                        self.indent(f, depth + 2)?;
                        self.show_expr(&self[e.value], f, depth + 2)?;
                    }
                }
                self.indent(f, depth)?;
                writeln!(f, ")")
            }

            Expr::Let(name, expr, body) => {
                writeln!(f, "Let({}", &self[*name])?;
                self.indent(f, depth + 2)?;
                self.show_expr(&self[*expr], f, depth + 2)?;
                self.indent(f, depth + 2)?;
                self.show_expr(&self[*body], f, depth + 2)?;
                self.indent(f, depth)?;
                writeln!(f, ")")
            }

            Expr::Fun(cases) => {
                writeln!(f, "Fun(")?;
                for case in cases {
                    self.indent(f, depth + 2)?;
                    self.show_pat(&case.pat, f)?;
                    writeln!(f, " =>")?;
                    self.indent(f, depth + 4)?;
                    self.show_expr(&self[case.expr], f, depth + 4)?;
                }
                self.indent(f, depth)?;
                writeln!(f, ")")
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

impl std::ops::Index<ExprRef> for ASTArena {
    type Output = Expr;

    fn index(&self, rf: ExprRef) -> &Self::Output {
        &self.exprs[rf.idx]
    }
}

/// A `Printable` struct is a bundle of another value and an
/// `ASTArena`, which allows us to fetch the various indices and
/// dereference the interned strings.
pub struct Printable<'a, T> {
    arena: &'a ASTArena,
    value: &'a T,
}

impl<'a> std::fmt::Debug for Printable<'a, Stmt> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.arena.show_stmt(self.value, f)
    }
}

impl<'a> std::fmt::Debug for Printable<'a, Expr> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.arena.show_expr(self.value, f, 0)
    }
}

/// A top-level Matzo statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    /// evaluate and print the value of an expression
    Puts(ExprRef),
    /// replace a named item with the forced version of that item
    Fix(Name),
    /// assign a value to a name which may or may not be forced
    Assn(bool, Name, ExprRef),
    /// assign one of a set of strings to a name, which may or may not
    /// be forced
    LitAssn(bool, Name, Vec<String>),
}

impl Stmt {
    pub fn show<'a>(&'a self, ast: &'a ASTArena) -> Printable<'a, Stmt> {
        Printable {
            arena: ast,
            value: self,
        }
    }
}

/// A Matzo expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(Name),
    Cat(Vec<ExprRef>),
    Chc(Vec<Choice>),
    Lit(Literal),
    Ap(ExprRef, ExprRef),
    Tup(Vec<ExprRef>),
    Let(Name, ExprRef, ExprRef),
    Fun(Vec<Case>),
    Range(ExprRef, ExprRef),
    Nil,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprRef {
    idx: usize,
}

/// A single case in an anonymous function or `case` statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Case {
    pub pat: Pat,
    pub expr: ExprRef,
}

/// A pattern, e.g. in an anonymous function or `case` statement
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Var(Name),
    Lit(Literal),
    Tup(Vec<Pat>),
}

/// A single element in a choice, with an optional weight (which
/// defaults to 1) and a value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Choice {
    pub weight: Option<i64>,
    pub value: ExprRef,
}

impl Choice {
    /// Fetch a weight from a `Choice`, defaulting to 1
    pub fn weight(&self) -> i64 {
        self.weight.unwrap_or(1)
    }
}

/// An atomic literal: a string, a number, or an atom
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Str(String),
    Atom(Name),
    Num(i64),
}
