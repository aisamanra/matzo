pub use crate::lexer::{FileRef, Located, Span};
use std::fmt;

// There are two big parts to this file: firstly, there are the
// representations of the relevant data structures, and then there's
// the `ASTArena` that contains them.

/// A `StrRef` is an interned string
pub type StrRef = string_interner::DefaultSymbol;
/// A `Name` is a `StrRef` with a location in the file, for
/// e.g. error-printing purposes
pub type Name = Located<StrRef>;

/// A top-level Matzo statement.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// evaluate and print the value of an expression
    Puts(ExprRef),
    /// replace a named item with the forced version of that item
    Fix(Name),
    /// assign a value to a name which may or may not be forced.
    Assn(Binding),
}

impl Stmt {
    pub fn show<'a>(&'a self, ast: &'a ASTArena) -> Printable<'a, Stmt> {
        Printable {
            arena: ast,
            value: self,
        }
    }
}

/// A Matzo expression. Note that this is not _actually_ a recursive
/// type: the recursive references aren't `Expr` but `ExprRef`. That
/// means that without an `ASTArena` to look up those references, we
/// only have part of the expression.
#[derive(Debug, Clone)]
pub enum Expr {
    /// A variable to look up in the surrounding scope.
    Var(Name),
    /// Concatenation: stringify each expression and jam 'em together
    Cat(Vec<ExprRef>),
    /// Potentially-weighted choice: randomly select an expression
    /// from the provided chocies
    Chc(Vec<Choice>),
    /// A literal: string, integer, or atom
    Lit(Literal),

    /// A tuple of expressions
    Tup(Vec<ExprRef>),
    /// A record with named fields
    Record(Vec<Field>),

    /// Application: applying an expression to one or more arguments.
    Ap(ExprRef, Vec<ExprRef>),
    /// A (possibly strict) let-binding
    Let(Vec<Binding>, ExprRef),
    /// A lambda, defined by cases
    Fun(Vec<Case>),
    /// A range of numbers
    Range(ExprRef, ExprRef),
    /// A case expression with a list of cases
    Case(ExprRef, Vec<Case>),
    /// Accessing the field of a record
    Access(ExprRef, Name),
    /// An empty tree. (We can't write this, but we desugar into it
    /// sometimes)
    Nil,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub fixed: bool,
    pub name: Name,
    pub expr: ExprRef,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Name,
    pub expr: ExprRef,
}

/// An `ExprRef` contains not just an index but also a wrapped
/// location.
pub type ExprRef = Located<ExprId>;

/// A reference to an `Expr` in the `ASTArena`. We should only produce
/// these for expressions that definitely exist, so we shouldn't need
/// to verify that they're valid first. (We never modify or remove
/// elements in an `ASTArena`, after all.)
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprId {
    idx: usize,
}

impl ExprId {
    /// We pre-initialize the `ASTArena` with a `nil` value, so all
    /// nil values can point to that.
    pub fn nil(&self) -> bool {
        self.idx == 0
    }
}

/// A single case in an anonymous function or `case` statement
#[derive(Debug, Clone)]
pub struct Case {
    pub pats: Vec<Pat>,
    pub expr: ExprRef,
}

/// A pattern, e.g. in an anonymous function or `case` statement
#[derive(Debug, Clone)]
pub enum Pat {
    /// A pattern that matches and binds to a specific name
    Var(Name),
    /// A pattern that matches anything
    Wildcard,
    /// A literal (string, number, or atom)
    Lit(Literal),
    /// A tuple of other patterns
    Tup(Vec<Pat>),
}

/// A single element in a choice, with an optional weight (which
/// defaults to 1) and a value
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub enum Literal {
    Str(String),
    Atom(Name),
    Num(i64),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Literal) -> bool {
        match (self, other) {
            (Literal::Str(s1), Literal::Str(s2)) => s1 == s2,
            (Literal::Atom(a1), Literal::Atom(a2)) => a1.item == a2.item,
            (Literal::Num(n1), Literal::Num(n2)) => n1 == n2,
            (_, _) => false,
        }
    }
}

/// An `ASTArena` contains all the information we get from parsing
/// files as well as all the interned strings. This deliberately has
/// private internals so it can be tweaked later while maintaining
/// whatever internal invariants we need.
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
            // why this for `exprs`? See the above note on
            // `ExprRef::nil`.
        }
    }

    pub fn expr_nil(&self) -> ExprId {
        ExprId { idx: 0 }
    }

    /// Add an `Expr` to the arena and return the `ExprId` we can use
    /// to refer to it later
    pub fn add_expr(&mut self, e: Expr) -> ExprId {
        let idx = self.exprs.len();
        self.exprs.push(e);
        ExprId { idx }
    }

    /// Intern a string and add the `StrRef` we can use to refer to it
    /// later
    pub fn add_string(&mut self, s: &str) -> StrRef {
        self.strings.get_or_intern(s)
    }

    /// Print a rich internal representation of a `Stmt`.
    ///
    /// TODO: make this use `Printable` instead.
    pub fn show_stmt(&self, stmt: &Stmt, f: &mut fmt::Formatter) -> fmt::Result {
        match stmt {
            Stmt::Puts(expr) => {
                write!(f, "Puts ")?;
                self.show_expr(&self[expr.item], f, 0)
            }
            Stmt::Fix(name) => writeln!(f, "Fix {}", &self[name.item]),
            Stmt::Assn(Binding { fixed, name, expr }) => {
                write!(
                    f,
                    "Assn {} {} ",
                    if *fixed { "fixed" } else { "" },
                    &self[name.item]
                )?;
                self.show_expr(&self[expr.item], f, 0)
            }
        }
    }

    /// A helper function for printing, write `depth` spaces to the
    /// formatter.
    fn indent(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
        for _ in 0..depth {
            write!(f, " ")?;
        }
        Ok(())
    }

    /// Print a representation of a pattern.
    ///
    /// TODO: convert this to `Printable`
    fn show_pat(&self, pat: &Pat, f: &mut fmt::Formatter) -> fmt::Result {
        match pat {
            Pat::Wildcard => write!(f, "_"),
            Pat::Var(n) => write!(f, "{}", &self[n.item]),
            Pat::Lit(Literal::Atom(n)) => write!(f, "{}", &self[n.item]),
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

    /// Print a representation of an expression.
    ///
    /// TODO: convert this to `Printable`
    fn show_expr(&self, expr: &Expr, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
        match expr {
            Expr::Nil => writeln!(f, "Nil"),
            Expr::Var(v) => writeln!(f, "Var({})", &self[v.item]),
            Expr::Lit(Literal::Atom(n)) => writeln!(f, "Lit(Atom({}))", &self[n.item]),
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

            Expr::Ap(func, args) => {
                writeln!(f, "Ap(")?;
                self.indent(f, depth + 2)?;
                self.show_expr(&self[*func], f, depth + 2)?;
                for arg in args {
                    self.indent(f, depth + 2)?;
                    self.show_expr(&self[*arg], f, depth + 2)?;
                }
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

            Expr::Record(fields) => {
                writeln!(f, "Record(")?;
                for e in fields {
                    self.indent(f, depth + 2)?;
                    writeln!(f, "{}", &self[e.name.item])?;
                    self.show_expr(&self[e.expr], f, depth + 2)?;
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

            Expr::Let(bindings, body) => {
                writeln!(f, "Let(")?;
                for Binding { fixed, name, expr } in bindings {
                    self.indent(f, depth + 2)?;
                    writeln!(
                        f,
                        "{}{} :=",
                        if *fixed { "fixed " } else { "" },
                        &self[name.item]
                    )?;
                    self.show_expr(&self[*expr], f, depth + 2)?;
                }
                self.indent(f, depth + 2)?;
                self.show_expr(&self[*body], f, depth + 2)?;
                self.indent(f, depth)?;
                writeln!(f, ")")
            }

            Expr::Fun(cases) => {
                writeln!(f, "Fun(")?;
                for case in cases {
                    self.indent(f, depth + 2)?;
                    for pat in case.pats.iter() {
                        self.show_pat(pat, f)?;
                        writeln!(f, ", ")?;
                    }
                    writeln!(f, " =>")?;
                    self.indent(f, depth + 4)?;
                    self.show_expr(&self[case.expr], f, depth + 4)?;
                }
                self.indent(f, depth)?;
                writeln!(f, ")")
            }

            Expr::Case(expr, cases) => {
                writeln!(f, "Case(")?;
                self.indent(f, depth)?;
                self.show_expr(&self[*expr], f, depth)?;
                for case in cases {
                    self.indent(f, depth + 2)?;
                    self.show_pat(&case.pats[0], f)?;
                    writeln!(f, " =>")?;
                    self.indent(f, depth + 4)?;
                    self.show_expr(&self[case.expr], f, depth + 4)?;
                }
                self.indent(f, depth)?;
                writeln!(f, ")")
            }

            Expr::Access(expr, field) => {
                writeln!(f, "Access(")?;
                self.indent(f, depth)?;
                self.show_expr(&self[expr.item], f, depth + 2)?;
                self.indent(f, depth)?;
                writeln!(f, "{}", &self[field.item])
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
        &self.exprs[rf.item.idx]
    }
}

impl std::ops::Index<ExprId> for ASTArena {
    type Output = Expr;

    fn index(&self, rf: ExprId) -> &Self::Output {
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
