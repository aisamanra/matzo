use crate::ast::*;
use crate::lexer::Span;
use crate::rand::*;

use anyhow::{anyhow, bail, Error};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::io::Write;
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

#[derive(Debug, Clone, Copy)]
pub struct BuiltinRef {
    idx: usize,
}

impl Value {
    fn to_string(&self, ast: &ASTArena) -> String {
        self.with_str(ast, |s| s.to_string())
    }
}

impl Value {
    /// Convert this value to a Rust integer, failing otherwise
    pub fn as_num(&self, ast: &ASTArena) -> Result<i64, Error> {
        match self {
            Value::Lit(Literal::Num(n)) => Ok(*n),
            _ => self.with_str(ast, |s| bail!("Expected number, got {}", s)),
        }
    }

    /// Convert this value to a Rust string, failing otherwise
    pub fn as_str(&self, ast: &ASTArena) -> Result<&str, Error> {
        match self {
            Value::Lit(Literal::Str(s)) => Ok(s),
            _ => self.with_str(ast, |s| bail!("Expected string, got {}", s)),
        }
    }

    /// Convert this value to a Rust slice, failing otherwise
    pub fn as_tup(&self, ast: &ASTArena) -> Result<&[Thunk], Error> {
        match self {
            Value::Tup(vals) => Ok(vals),
            _ => self.with_str(ast, |s| bail!("Expected tuple, got {}", s)),
        }
    }

    /// Convert this value to a closure, failing otherwise
    pub fn as_closure(&self, ast: &ASTArena) -> Result<&Closure, Error> {
        match self {
            Value::Closure(closure) => Ok(closure),
            _ => self.with_str(ast, |s| bail!("Expected tuple, got {}", s)),
        }
    }

    /// Call the provided function with the string representation of
    /// this value. Note that this _will not force the value_ if it's
    /// not completely forced already: indeed, this can't, since it
    /// doesn't have access to the `State`. Unevaluated fragments of
    /// the value will be printed as `#<unevaluated>`.
    pub fn with_str<U>(&self, ast: &ASTArena, f: impl FnOnce(&str) -> U) -> U {
        match self {
            Value::Nil => f(""),
            Value::Lit(Literal::Str(s)) => f(s),
            Value::Lit(Literal::Atom(s)) => f(&format!("{}", &ast[s.item])),
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
                        Thunk::Expr(..) => buf.push_str("#<unevaluated>"),
                        Thunk::Builtin(func) => buf.push_str(&format!("#<builtin {}>", func.idx)),
                    }
                }
                buf.push('>');
                f(&buf)
            }
            Value::Builtin(func) => f(&format!("#<builtin {}>", func.idx)),
            Value::Closure(_) => f("#<lambda ...>"),
        }
    }
}

/// A representation of a builtin function implemented in Rust. This
/// will be inserted into the global scope under the name provided as
/// `name`.
pub struct BuiltinFunc {
    /// The name of the builtin: this is used in error messages, in
    /// printing the value (e.g. in the case of `puts some-builtin`),
    /// and as the Matzo identifier used for this function.
    pub name: &'static str,
    /// The callback here is the Rust implementation of the function,
    /// where the provided `ExprRef` is the argument to the function.
    pub callback: Box<dyn Fn(&State, ExprRef, &Env) -> Result<Value, Error>>,
}

impl fmt::Debug for BuiltinFunc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "BuiltinFunc {{ name: {:?}, ... }}", self.name)
    }
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

/// An environment is either `None` (i.e. in the root scope) or `Some`
/// of some reference-counted scope (since those scopes might be
/// shared in several places, e.g. as pointers in thunks or closures).
pub type Env = Option<Rc<Scope>>;

/// A `Scope` represents a _non-root_ scope (since the root scope is
/// treated in a special way) and contains a map from variables to
/// `Thunk`s, along with a parent pointer.
#[derive(Debug)]
pub struct Scope {
    vars: HashMap<StrRef, Thunk>,
    parent: Env,
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
    func: ExprRef,
    scope: Env,
}

/// A `State` contains all the interpreter state needed to run a
/// `Matzo` program.
pub struct State {
    /// An `ASTArena` that contains all the packed information that
    /// results from parsing a program.
    pub ast: RefCell<ASTArena>,
    /// The root scope of the program, which contains all the
    /// top-level definitions and builtins.
    root_scope: RefCell<HashMap<StrRef, Thunk>>,
    /// The set of builtin (i.e. implemented-in-Rust) functions
    builtins: Vec<BuiltinFunc>,
    /// The thread-local RNG.
    rand: RefCell<Box<dyn MatzoRand>>,
    /// The instantiated parser used to parse Matzo programs
    parser: crate::grammar::StmtsParser,
    /// The instantiated parser used to parse Matzo programs
    expr_parser: crate::grammar::ExprRefParser,
}

impl Default for State {
    fn default() -> State {
        Self::new()
    }
}

impl State {
    /// This initializes a new `State` and adds all the builtin
    /// functions to the root scope
    pub fn new() -> State {
        let mut s = State {
            root_scope: RefCell::new(HashMap::new()),
            rand: RefCell::new(Box::new(DefaultRNG::new())),
            parser: crate::grammar::StmtsParser::new(),
            expr_parser: crate::grammar::ExprRefParser::new(),
            ast: RefCell::new(ASTArena::new()),
            builtins: Vec::new(),
        };
        for builtin in crate::builtins::builtins() {
            let idx = s.builtins.len();
            let sym = s.ast.borrow_mut().add_string(&builtin.name);
            s.root_scope
                .borrow_mut()
                .insert(sym, Thunk::Builtin(BuiltinRef { idx }));
            s.builtins.push(builtin);
        }
        s
    }

    /// This initializes a new `State` and adds all the builtin
    /// functions to the root scope
    pub fn new_from_seed(seed: u64) -> State {
        let mut s = State {
            root_scope: RefCell::new(HashMap::new()),
            rand: RefCell::new(Box::new(SeededRNG::from_seed(seed))),
            parser: crate::grammar::StmtsParser::new(),
            expr_parser: crate::grammar::ExprRefParser::new(),
            ast: RefCell::new(ASTArena::new()),
            builtins: Vec::new(),
        };
        for builtin in crate::builtins::builtins() {
            let idx = s.builtins.len();
            let sym = s.ast.borrow_mut().add_string(&builtin.name);
            s.root_scope
                .borrow_mut()
                .insert(sym, Thunk::Builtin(BuiltinRef { idx }));
            s.builtins.push(builtin);
        }
        s
    }

    /// Get the underlying AST. (This is mostly useful for testing
    /// purposes, where we don't want to have a function do the
    /// parsing and evaluating for us at the same time.)
    pub fn get_ast(&self) -> &RefCell<ASTArena> {
        &self.ast
    }

    /// Look up a `Name` in the provided `Env`. This will result in
    /// either a `Thunk` (i.e. the named value) or an error that
    /// indicates the missing name.
    fn lookup(&self, env: &Env, name: Name) -> Result<Thunk, Error> {
        if let Some(env) = env {
            if let Some(ne) = env.vars.get(&name.item) {
                Ok(ne.clone())
            } else {
                self.lookup(&env.parent, name)
            }
        } else {
            match self.root_scope.borrow().get(&name.item) {
                None => {
                    let span = self.ast.borrow().get_line(name.file, name.span);
                    bail!("no such thing: {}\n{}", &self.ast.borrow()[name.item], span)
                }
                Some(ne) => Ok(ne.clone()),
            }
        }
    }

    /// Evaluate this string as a standalone program, writing the
    /// results to stdout.
    pub fn run(&self, src: &str) -> Result<(), Error> {
        let lexed = crate::lexer::tokens(src);
        let file = self.ast.borrow_mut().add_file(src.to_string());
        let stmts = self
            .parser
            .parse(&mut self.ast.borrow_mut(), file, lexed)
            .map_err(|err| anyhow!("Got {:?}", err))?;
        let mut stdout = io::stdout();
        for stmt in stmts {
            self.execute(&stmt, &mut stdout)?;
        }
        Ok(())
    }

    /// Evaluate this string as a standalone program, writing the
    /// results to the provided writer.
    pub fn run_with_writer(&self, src: &str, w: &mut impl std::io::Write) -> Result<(), Error> {
        let lexed = crate::lexer::tokens(src);
        let file = self.ast.borrow_mut().add_file(src.to_string());
        let stmts = self
            .parser
            .parse(&mut self.ast.borrow_mut(), file, lexed)
            .map_err(|err| anyhow!("Got {:?}", err))?;
        for stmt in stmts {
            self.execute(&stmt, &mut *w)?;
        }
        Ok(())
    }

    /// Evaluate this string as a fragment in a REPL, writing the
    /// results to stdout. One way this differs from the standalone
    /// program is that it actually tries parsing twice: first it
    /// tries parsing the fragment normally, and then if that doesn't
    /// work it tries adding a `puts` ahead of it: this is hacky, but
    /// it allows the REPL to respond by printing values when someone
    /// simply types an expression.
    pub fn run_repl(&self, src: &str) -> Result<(), Error> {
        let lexed = crate::lexer::tokens(src);
        let file = self.ast.borrow_mut().add_file(src.to_string());
        let stmts = {
            let mut ast = self.ast.borrow_mut();
            self.parser.parse(&mut ast, file, lexed)
        };
        match stmts {
            Ok(stmts) => {
                for stmt in stmts {
                    self.execute(&stmt, io::stdout())?;
                }
            }
            Err(err) => {
                let lexed = crate::lexer::tokens(src);
                let expr = {
                    let mut ast = self.ast.borrow_mut();
                    self.expr_parser.parse(&mut ast, file, lexed)
                };
                if let Ok(expr) = expr {
                    let val = self.eval(expr, &None)?;
                    let val = self.force(val)?;
                    writeln!(io::stdout(), "{}", val.to_string(&self.ast.borrow()))?;
                } else {
                    bail!("{:?}", err);
                }
            }
        };
        Ok(())
    }

    /// Autocomplete this name. This doesn't make use of any
    /// contextual information (e.g. like function arguments or
    /// `let`-bound names) but instead tries to complete based
    /// entirely on the things in root scope.
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

    /// Execute this statement, writing any output to the provided
    /// output writer. Right now, this will always start in root
    /// scope: there are no statements within functions.
    pub fn execute(&self, stmt: &Stmt, mut output: impl io::Write) -> Result<(), Error> {
        match stmt {
            // Evaluate the provided expression _all the way_
            // (i.e. recurisvely, not to WHNF) and write its
            // representation to the output.
            Stmt::Puts(expr) => {
                let val = self.eval(*expr, &None)?;
                let val = self.force(val)?;
                writeln!(output, "{}", val.to_string(&self.ast.borrow())).unwrap();
            }

            // Look up the provided name, and if it's not already
            // forced completely, then force it completely and
            // re-insert this name with the forced version.
            Stmt::Fix(name) => {
                let val = match self.lookup(&None, *name)? {
                    Thunk::Expr(e, env) => self.eval(e, &env)?,
                    // we need to handle this case in case it's
                    // already in WHNF (e.g. a tuple whose elements
                    // are not yet values)
                    Thunk::Value(v) => v,
                    // if it's not an expr or val, then our work here
                    // is done
                    _ => return Ok(()),
                };
                let val = self.force(val)?;
                self.root_scope
                    .borrow_mut()
                    .insert(name.item, Thunk::Value(val));
            }

            // assign a given expression to a name, forcing it to a
            // value if the assignment is `fixed`.
            Stmt::Assn(fixed, name, expr) => {
                let thunk = if *fixed {
                    let val = self.eval(*expr, &None)?;
                    let val = self.force(val)?;
                    Thunk::Value(val)
                } else {
                    Thunk::Expr(*expr, None)
                };
                self.root_scope.borrow_mut().insert(name.item, thunk);
            }

            // assign a simple disjunction of strings to a name,
            // forcing it to a value if the assignment is `fixed`.
            Stmt::LitAssn(fixed, name, strs) => {
                if *fixed {
                    let choice = &strs[self.rand.borrow_mut().gen_range_usize(0, strs.len())];
                    let str = self.ast.borrow()[choice.item].to_string();
                    self.root_scope
                        .borrow_mut()
                        .insert(name.item, Thunk::Value(Value::Lit(Literal::Str(str))));
                    return Ok(());
                }

                let choices: Vec<Choice> = strs
                    .iter()
                    .map(|s| {
                        let str = self.ast.borrow()[s.item].to_string();
                        Choice {
                            weight: None,
                            value: Located {
                                file: s.file,
                                span: s.span,
                                item: self.ast.borrow_mut().add_expr(Expr::Lit(Literal::Str(str))),
                            },
                        }
                    })
                    .collect();
                let choices = Located {
                    file: choices.first().unwrap().value.file,
                    span: Span {
                        start: choices.first().unwrap().value.span.start,
                        end: choices.last().unwrap().value.span.end,
                    },
                    item: self.ast.borrow_mut().add_expr(Expr::Chc(choices)),
                };
                self.root_scope
                    .borrow_mut()
                    .insert(name.item, Thunk::Expr(choices, None));
            }
        }
        Ok(())
    }

    /// Given a value, force it recursively.
    fn force(&self, val: Value) -> Result<Value, Error> {
        match val {
            Value::Tup(values) => Ok(Value::Tup(
                values
                    .into_iter()
                    .map(|t| {
                        let v = self.hnf(&t)?;
                        let v = self.force(v)?;
                        Ok(Thunk::Value(v))
                    })
                    .collect::<Result<Vec<Thunk>, Error>>()?,
            )),
            _ => Ok(val),
        }
    }

    /// Given a thunk, force it to WHNF.
    pub fn hnf(&self, thunk: &Thunk) -> Result<Value, Error> {
        match thunk {
            Thunk::Expr(expr, env) => self.eval(*expr, env),
            Thunk::Value(val) => Ok(val.clone()),
            Thunk::Builtin(b) => Ok(Value::Builtin(*b)),
        }
    }

    /// Given an `ExprRef` and an environment, fetch that expression
    /// and then evalute it in that environment
    pub fn eval(&self, expr_ref: ExprRef, env: &Env) -> Result<Value, Error> {
        let expr = &self.ast.borrow()[expr_ref.item];
        match expr {
            // literals should be mostly cheap-ish to copy, so a
            // literal evaluates to a `Value` that's a copy of the
            // literal
            Expr::Lit(l) => Ok(Value::Lit(l.clone())),
            // `Nil` evalutes to `Nil`
            Expr::Nil => Ok(Value::Nil),

            // When a variable is used, we should look it up and
            // evaluate it to WHNF
            Expr::Var(v) => self.hnf(&self.lookup(env, *v)?),

            // for a catenation, we should fully evaluate all the
            // expressions, convert them to strings, and concatenate
            // them all.
            Expr::Cat(cat) => {
                // if we ever have a catentation of one, then don't
                // bother with the string: just evaluate the
                // expression.
                if cat.len() == 1 {
                    self.eval(cat[0], env)
                } else {
                    let mut buf = String::new();
                    for expr in cat {
                        let val = self.eval(*expr, env)?;
                        let val = self.force(val)?;
                        buf.push_str(&val.to_string(&self.ast.borrow()));
                    }
                    Ok(Value::Lit(Literal::Str(buf)))
                }
            }

            // for choices, we should choose one with the appropriate
            // frequency and then evaluate it
            Expr::Chc(choices) => {
                // if we ever have only one choice, well, choose it:
                if choices.len() == 1 {
                    self.eval(choices[0].value, env)
                } else {
                    self.choose(choices, env)
                }
            }

            // for a tuple, we return a tuple of thunks to begin with,
            // to make sure that the values contained within are
            // appropriately lazy
            Expr::Tup(values) => Ok(Value::Tup(
                values
                    .iter()
                    .map(|v| Thunk::Expr(*v, env.clone()))
                    .collect::<Vec<Thunk>>(),
            )),

            // for a range, choose randomly between the start and end
            // expressions
            Expr::Range(from, to) => {
                let from = self.eval(*from, env)?.as_num(&self.ast.borrow())?;
                let to = self.eval(*to, env)?.as_num(&self.ast.borrow())?;
                Ok(Value::Lit(Literal::Num(
                    self.rand.borrow_mut().gen_range_i64(from, to + 1),
                )))
            }

            // for a function, return a closure (i.e. the function
            // body paired with the current environment)
            Expr::Fun(_) => Ok(Value::Closure(Closure {
                func: expr_ref,
                scope: env.clone(),
            })),

            // for application, make sure the thing we're applying is
            // either a closure (i.e. the result of evaluating a
            // function) or a builtin, and then handle it
            // appropriately
            Expr::Ap(func, val) => match self.eval(*func, env)? {
                Value::Closure(c) => {
                    let scrut = Thunk::Expr(*val, env.clone());
                    self.eval_closure(&c, scrut)
                }
                Value::Builtin(b) => {
                    let builtin = &self.builtins[b.idx];
                    (builtin.callback)(self, *val, env)
                }
                _ => bail!("Bad function: {:?}", func),
            },

            // for a let-expression, create a new scope, add the new
            // name to it (optionally forcing it if `fixed`) and then
            // evaluate the body within that scope.
            Expr::Let(fixed, name, val, body) => {
                let mut new_scope = HashMap::new();
                if *fixed {
                    let val = self.eval(*val, env)?;
                    let val = self.force(val)?;
                    new_scope.insert(name.item, Thunk::Value(val));
                } else {
                    new_scope.insert(name.item, Thunk::Expr(*val, env.clone()));
                };
                let new_scope = Rc::new(Scope {
                    vars: new_scope,
                    parent: env.clone(),
                });
                self.eval(*body, &Some(new_scope))
            }

            Expr::Case(scrut, _) => {
                let closure = Closure {
                    func: expr_ref,
                    scope: env.clone(),
                };
                self.eval_closure(&closure, Thunk::Expr(*scrut, env.clone()))
            }
        }
    }

    /// Evaluate a closure as applied to a given argument.
    ///
    /// There's a very subtle thing going on here: when we apply a
    /// closure to an expression, we should evaluate that expression
    /// _as far as we need to and no further_. That's why the `scrut`
    /// argument here is mutable: to start with, it'll be a
    /// `Thunk::Expr`. If the function uses a wildcard or variable
    /// match, it'll stay that way, but if we start matching against
    /// it, we'll evaluate it at least to WHNF to find out whether it
    /// maches, and _sometimes_ a little further.
    ///
    /// Here's where it gets tricky: we need to maintain that
    /// evaluation between branches so that we don't get Schrödinger's
    /// patterns. An example where that might work poorly if we're not
    /// careful is here:
    ///
    /// ```ignore
    /// {Foo => "1"; Foo => "2"; _ => "..."}.(Foo | Bar)
    /// ```
    ///
    /// It should be impossible to get `"2"` in this case. That means
    /// that we need to force the argument _and keep branching against
    /// the forced argument_. But we also want the following to still
    /// contain non-determinism:
    ///
    /// ```ignore
    /// {<Foo, x> => x x "!"; <Bar, x> => x x "?"}.<Foo | Bar, "a" | "b">
    /// ```
    ///
    /// The above program should print one of "aa!", "bb!", "aa?", or
    /// "bb?". That means it needs to
    ///  1. force the argument first to `<_, _>`, to make sure it's a
    /// two-element tuple
    /// 2. force the first element of the tuple to `Foo` or `Bar` to
    /// discriminate on it, but
    /// 3. _not_ force the second element of the tuple, because we
    /// want it to vary from invocation to invocation.
    ///
    /// So the way we do this is, we start by representing the
    /// argument as a `Thunk::Expr`, but allow the pattern-matching
    /// function to mutably replace it with progressively more
    /// evaluated versions of the same expression, and then that's the
    /// thing we put into scope in the body of the function.
    pub fn eval_closure(&self, closure: &Closure, mut scrut: Thunk) -> Result<Value, Error> {
        let ast = self.ast.borrow();
        let cases = match &ast[closure.func] {
            Expr::Fun(cases) => cases,
            Expr::Case(_, cases) => cases,
            // see the note attached to the definition of `Closure`
            _ => bail!("INVARIANT FAILED"),
        };

        // for each case
        for c in cases {
            // build a set of potential bindings, which `match_pat`
            // will update if it finds matching variables
            let mut bindings = Vec::new();
            if !self.match_pat(&c.pat, &mut scrut, &mut bindings)? {
                // if we didn't match, we don't care about any
                // bindings we've found: simply skip it
                continue;
            }

            // build a new scope from the bindings discovered
            let mut new_scope = HashMap::new();
            for (name, binding) in bindings {
                new_scope.insert(name.item, binding);
            }

            let new_scope = Rc::new(Scope {
                vars: new_scope,
                parent: closure.scope.clone(),
            });
            // and now evaluate the chosen branch body in the
            // newly-created scope
            return self.eval(c.expr, &Some(new_scope));
        }

        // we couldn't find a matching pattern, so throw an error
        bail!("No pattern in {:?} matched {:?}", cases, scrut);
    }

    /// attempt to match the thunk `scrut` against the pattern
    /// `pat`. If it matched, then it'll return `Ok(true)`, if it
    /// didn't, it'll return `Ok(false)`, and (because it might need
    /// to do incremental evaluation to check if the pattern matches)
    /// it'll return an error if forcing parts of the expression
    /// returns an error. The `bindings` vector will be filled with
    /// name-thunk pairs based on the pattern: if this returns
    /// `Ok(true)`, then those are the thunks that should be bound to
    /// names in the context, but otherwise those bindings can be
    /// safely ignored.
    fn match_pat(
        &self,
        pat: &Pat,
        scrut: &mut Thunk,
        bindings: &mut Vec<(Name, Thunk)>,
    ) -> Result<bool, Error> {
        if let Pat::Var(v) = pat {
            bindings.push((*v, scrut.clone()));
            return Ok(true);
        }
        if let Pat::Wildcard = pat {
            return Ok(true);
        }
        // if it's not just a variable, then we'll need to make sure
        // we've evaluated `scrut` at least one level from here
        if let Thunk::Expr(e, env) = scrut {
            *scrut = Thunk::Value(self.eval(*e, env)?)
        };

        // now we can match deeper patterns, at least a little
        match pat {
            // literals match if the thunk is an identical literal
            Pat::Lit(lhs) => {
                if let Thunk::Value(Value::Lit(rhs)) = scrut {
                    Ok(lhs == rhs)
                } else {
                    Ok(false)
                }
            }

            // tuples match if the thunk evaluates to a tuple of the
            // same size, and if all the patterns in the tuple match
            // the thunks in the expression
            Pat::Tup(pats) => {
                if let Thunk::Value(Value::Tup(thunks)) = scrut {
                    if pats.len() != thunks.len() {
                        return Ok(false);
                    }

                    for (p, t) in pats.iter().zip(thunks) {
                        if !self.match_pat(p, t, bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }

            // otherwise, Does Not Match
            _ => Ok(false),
        }
    }

    // this chooses an expressino from a choice, taking into account
    // the weights
    fn choose(&self, choices: &[Choice], env: &Env) -> Result<Value, Error> {
        let max = choices.iter().map(Choice::weight).sum();
        let mut choice = self.rand.borrow_mut().gen_range_i64(0, max);
        for ch in choices {
            if choice < ch.weight() {
                return self.eval(ch.value, env);
            }
            choice -= ch.weight();
        }

        // if we got here, it means our math was wrong
        bail!("unreachable")
    }
}
