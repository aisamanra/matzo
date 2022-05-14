use crate::ast::*;
use crate::core::Loc;
use crate::errors::MatzoError;

use std::collections::HashMap;
use std::fmt::Write;
use std::rc::Rc;

/// A `Value` is a representation of the result of evaluation. Note
/// that a `Value` is a representation of something in _weak head
/// normal form_: i.e. for compound expressions (right now just
/// tuples) it might contain other values but it might contain
/// unevaluated expressions as well.
#[derive(Debug, Clone)]
pub enum Value {
    Lit(Literal),
    Tup(Vec<Thunk>),
    Builtin(BuiltinRef),
    Closure(Closure),
    Nil,
}

impl Value {
    /// Convert a `Value` to its string representation.
    pub fn to_string(&self, ast: &ASTArena) -> String {
        self.with_str(ast, |s| s.to_string())
    }

    /// Convert this value to a Rust integer, failing otherwise
    pub fn as_num(&self, ast: &ASTArena, loc: Loc) -> Result<i64, MatzoError> {
        match self {
            Value::Lit(Literal::Num(n)) => Ok(*n),
            _ => self.with_str(ast, |s| {
                return Err(MatzoError::new(loc, format!("Expected number, got {}", s)));
            }),
        }
    }

    /// Convert this value to a Rust string, failing otherwise
    pub fn as_str(&self, ast: &ASTArena, loc: Loc) -> Result<&str, MatzoError> {
        match self {
            Value::Lit(Literal::Str(s)) => Ok(s),
            _ => self.with_str(ast, |s| {
                return Err(MatzoError::new(loc, format!("Expected string, got {}", s)));
            }),
        }
    }

    /// Convert this value to a Rust slice, failing otherwise
    pub fn as_tup(&self, ast: &ASTArena, loc: Loc) -> Result<&[Thunk], MatzoError> {
        match self {
            Value::Tup(vals) => Ok(vals),
            _ => self.with_str(ast, |s| {
                return Err(MatzoError::new(loc, format!("Expected tuple, got {}", s)));
            }),
        }
    }

    /// Convert this value to a closure, failing otherwise
    pub fn as_closure(&self, ast: &ASTArena, loc: Loc) -> Result<&Closure, MatzoError> {
        match self {
            Value::Closure(closure) => Ok(closure),
            _ => self.with_str(ast, |s| {
                return Err(MatzoError::new(loc, format!("Expected closure, got {}", s)));
            }),
        }
    }

    /// Call the provided function with the string representation of
    /// this value. Note that this _will not force the value_ if it's
    /// not completely forced already: indeed, this can't, since it
    /// doesn't have access to the `State`. Unevaluated fragments of
    /// the value will be printed as `...`.
    pub fn with_str<U>(&self, ast: &ASTArena, f: impl FnOnce(&str) -> U) -> U {
        match self {
            Value::Nil => f(""),
            Value::Lit(Literal::Str(s)) => f(s),
            Value::Lit(Literal::Atom(s)) => f(&ast[s.item].to_string()),
            Value::Lit(Literal::Num(n)) => f(&format!("{}", n)),
            Value::Tup(values) => {
                let mut buf = String::new();
                buf.push('<');
                for (i, val) in values.iter().enumerate() {
                    if i > 0 {
                        buf.push_str(", ");
                    }
                    match val {
                        Thunk::Value(v) => buf.push_str(&v.to_string(ast)),
                        Thunk::Expr(..) => buf.push_str("..."),
                        Thunk::Builtin(func) => write!(buf, "#<builtin {}>", func.idx).unwrap(),
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

#[derive(Debug, Clone, Copy)]
pub struct BuiltinRef {
    pub name: &'static str,
    pub idx: usize,
}

/// The name `Thunk` is a bit of a misnomer here: this is
/// _potentially_ a `Thunk`, but represents anything that can be
/// stored in a variable: it might be an unevaluated expression (along
/// with the environment where it should be evaluated), or it might be
/// a partially- or fully-forced value, or it might be a builtin
/// function.
#[derive(Debug, Clone)]
pub enum Thunk {
    Expr(ExprRef, Env),
    Value(Value),
    Builtin(BuiltinRef),
}

impl Thunk {
    pub fn with_str<U>(&self, ast: &ASTArena, f: impl FnOnce(&str) -> U) -> U {
        match self {
            Thunk::Expr(_, _) => f("..."),
            Thunk::Value(v) => v.with_str(ast, f),
            Thunk::Builtin(b) => f(&format!("#<builtin {}", b.name)),
        }
    }
}

/// An environment is either `None` (i.e. in the root scope) or `Some`
/// of some reference-counted scope (since those scopes might be
/// shared in several places, e.g. as pointers in thunks or closures).
pub type Env = Option<Rc<Scope>>;

/// A `Scope` represents a _non-root_ scope (since the root scope is
/// treated in a special way) and contains a map from variables to
/// `Thunk`s, along with a parent pointer.
#[derive(Debug)]
pub struct Scope {
    pub vars: HashMap<StrRef, Thunk>,
    pub parent: Env,
}

/// A `Closure` is a pointer to the expression that represents a
/// function implementation along with the scope in which it was
/// defined.
///
/// IMPORTANT INVARIANT: the `func` here should be an `ExprRef` which
/// references a `Func`. The reason we don't copy the `Func` in is
/// because, well, that'd be copying, and we can bypass that, but we
/// have to maintain that invariant explicitly, otherwise we'll panic.
#[derive(Debug, Clone)]
pub struct Closure {
    pub func: ExprRef,
    pub scope: Env,
}
