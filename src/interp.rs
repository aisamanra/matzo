use crate::ast::*;
use crate::builtins::builtins;
use crate::core::FileTable;
use crate::errors::MatzoError;
use crate::rand::*;
pub use crate::value::*;
use crate::{grammar, lexer};

use anyhow::{bail, Error};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::rc::Rc;

/// A `Callback` here is what we use for builtin functions: they take
/// the state and the env along with a (non-empty) list of
/// arguments. (We don't even parse empty argument lists right now,
/// although that's subject to change.)
type Callback = Box<dyn Fn(&State, &[ExprRef], &Env) -> Result<Value, MatzoError>>;

/// A representation of a builtin function implemented in Rust. This
/// will be inserted into the global scope under the name provided as
/// `name`.
pub struct BuiltinFunc {
    /// The name of the builtin: this is used in error messages, in
    /// printing the value (e.g. in the case of `puts some-builtin`),
    /// and as the Matzo identifier used for this function.
    pub name: &'static str,
    /// The callback here is the Rust implementation of the function,
    /// where the provided `&[ExprRef]` is the arguments to the
    /// function. They should be unevaluated. (call by name, baybee)
    pub callback: Callback,
}

impl fmt::Debug for BuiltinFunc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "BuiltinFunc {{ name: {:?}, ... }}", self.name)
    }
}

/// A `State` contains all the interpreter state needed to run a
/// `Matzo` program. Note that a bunch of these are separate `RefCell`
/// things because that makes it easier to do some internal mutability
/// (e.g. adding stuff to the `ASTArena` even when we don't have
/// mutable state access.)
pub struct State {
    /// An `ASTArena` that contains all the packed information that
    /// results from parsing a program.
    pub ast: RefCell<ASTArena>,
    /// The root scope of the program, which contains all the
    /// top-level definitions and builtins.
    root_scope: RefCell<HashMap<StrRef, Thunk>>,
    /// The file table which contains the raw sources
    pub file_table: RefCell<FileTable>,
    /// The set of builtin (i.e. implemented-in-Rust) functions
    builtins: Vec<BuiltinFunc>,
    /// The thread-local RNG.
    rand: RefCell<Box<dyn MatzoRand>>,
    /// The instantiated parser used to parse Matzo programs
    parser: grammar::StmtsParser,
    /// The instantiated parser used to parse Matzo expressions
    expr_parser: grammar::ExprRefParser,
}

impl Default for State {
    fn default() -> State {
        Self::new()
    }
}

impl State {
    /// This initializes a new `State` and adds all the builtin
    /// functions to the root scope
    fn new_with_rand(rand: Box<dyn MatzoRand>) -> State {
        let mut s = State {
            root_scope: RefCell::new(HashMap::new()),
            file_table: RefCell::new(FileTable::new()),
            rand: RefCell::new(rand),
            parser: grammar::StmtsParser::new(),
            expr_parser: grammar::ExprRefParser::new(),
            ast: RefCell::new(ASTArena::new()),
            builtins: Vec::new(),
        };
        for builtin in builtins() {
            let idx = s.builtins.len();
            let sym = s.ast.borrow_mut().add_string(builtin.name);
            s.root_scope.borrow_mut().insert(
                sym,
                Thunk::Builtin(BuiltinRef {
                    idx,
                    name: builtin.name,
                }),
            );
            s.builtins.push(builtin);
        }
        s
    }

    /// This initializes a new `State` and adds all the builtin
    /// functions to the root scope
    pub fn new() -> State {
        State::new_with_rand(Box::new(DefaultRNG::new()))
    }

    /// This initializes a new `State` and adds all the builtin
    /// functions to the root scope
    pub fn new_from_seed(seed: u64) -> State {
        State::new_with_rand(Box::new(SeededRNG::from_seed(seed)))
    }

    /// Get the underlying AST. (This is mostly useful for testing
    /// purposes.)
    pub fn get_ast(&self) -> &RefCell<ASTArena> {
        &self.ast
    }

    /// Look up a `Name` in the provided `Env`. This will result in
    /// either a `Thunk` (i.e. the named value) or an error that
    /// indicates the missing name.
    fn lookup(&self, env: &Env, name: Name) -> Result<Thunk, MatzoError> {
        if let Some(env) = env {
            if let Some(ne) = env.vars.get(&name.item) {
                Ok(ne.clone())
            } else {
                self.lookup(&env.parent, name)
            }
        } else {
            match self.root_scope.borrow().get(&name.item) {
                None => Err(MatzoError::new(
                    name.loc,
                    format!("Undefined name {}", &self.ast.borrow()[name.item]),
                )),
                Some(ne) => Ok(ne.clone()),
            }
        }
    }

    /// Evaluate this string as a standalone program, writing the
    /// results to stdout.
    pub fn run(&self, src: &str) -> Result<(), Error> {
        self.run_with_writer(src, &mut io::stdout())
    }

    fn print_error(&self, mtz: MatzoError) -> String {
        let mut buf = String::new();
        buf.push_str(&mtz.message);
        buf.push('\n');
        buf.push_str(&self.file_table.borrow().get_line(mtz.loc));
        for ctx in mtz.context {
            buf.push('\n');
            buf.push_str(&ctx.message);
            buf.push_str(&self.file_table.borrow().get_line(ctx.loc));
        }
        buf
    }

    /// Evaluate this string as a standalone program, writing the
    /// results to the provided writer.
    pub fn run_with_writer(&self, src: &str, w: &mut impl std::io::Write) -> Result<(), Error> {
        let file = self
            .file_table
            .borrow_mut()
            .add_file("???".to_owned(), src.to_string());
        if let Err(mtz) = self.run_file(src, file, w) {
            bail!("{}", self.print_error(mtz));
        }
        Ok(())
    }

    fn run_file(
        &self,
        src: &str,
        file: FileRef,
        mut w: &mut impl std::io::Write,
    ) -> Result<(), MatzoError> {
        let lexed = lexer::tokens(src);
        let stmts = self.parser.parse(&mut self.ast.borrow_mut(), file, lexed);
        let stmts = stmts.map_err(|e| MatzoError::from_parse_error(file, e))?;
        for stmt in stmts {
            self.execute(&stmt, &mut w)?;
        }
        Ok(())
    }

    fn repl_parse(&self, src: &str) -> Result<Vec<Stmt>, MatzoError> {
        let lexed = lexer::tokens(src);
        let file = self.file_table.borrow_mut().add_repl_line(src.to_string());
        let stmts = {
            let mut ast = self.ast.borrow_mut();
            self.parser.parse(&mut ast, file, lexed)
        };
        match stmts {
            Ok(stmts) => Ok(stmts),
            Err(err) => {
                // this might have just been an expression instead, so
                // try parsing a single expression to see if that
                // works
                let lexed = lexer::tokens(src);
                let expr = {
                    let mut ast = self.ast.borrow_mut();
                    self.expr_parser.parse(&mut ast, file, lexed)
                };
                if let Ok(expr) = expr {
                    Ok(vec![Stmt::Puts(expr)])
                } else {
                    Err(MatzoError::from_parse_error(file, err))
                }
            }
        }
    }

    /// Evaluate this string as a fragment in a REPL, writing the
    /// results to stdout. One way this differs from the standalone
    /// program is that it actually tries parsing twice: first it
    /// tries parsing the fragment normally, and then if that doesn't
    /// work it tries parsing it as an expression instead of a
    /// statement and then printing the result.
    pub fn run_repl(&self, src: &str) -> Result<(), Error> {
        if let Err(mtz) = self.repl_main(src) {
            bail!("{}", self.print_error(mtz));
        }
        Ok(())
    }

    fn repl_main(&self, src: &str) -> Result<(), MatzoError> {
        let stmts = self.repl_parse(src)?;
        for stmt in stmts {
            self.execute(&stmt, io::stdout())?;
        }
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
    /// output writer. Right now, this will always run in the global
    /// root scope: there are no statements within functions, so
    /// statements are necessarily "top-level".
    pub fn execute(&self, stmt: &Stmt, mut output: impl io::Write) -> Result<(), MatzoError> {
        match stmt {
            // For a `puts`, we want to evaluate the provided
            // expression _all the way_ (i.e. recursively, not just to
            // WHNF) and write its representation to the output.
            Stmt::Puts(expr) => {
                let val = self.eval(*expr, &None)?;
                let val = self.force(val)?;
                writeln!(output, "{}", val.to_string(&self.ast.borrow())).unwrap();
            }

            // Look up the provided name, and if it's not already
            // forced completely, then force it completely and
            // re-insert this name with the forced version. (I don't
            // love this feature but it was in the original
            // implementation.)
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
                                loc: s.loc,
                                item: self.ast.borrow_mut().add_expr(Expr::Lit(Literal::Str(str))),
                            },
                        }
                    })
                    .collect();
                let choices = Located {
                    loc: choices.first().unwrap().value.loc,
                    item: self.ast.borrow_mut().add_expr(Expr::Chc(choices)),
                };
                self.root_scope
                    .borrow_mut()
                    .insert(name.item, Thunk::Expr(choices, None));
            }
        }
        Ok(())
    }

    /// Given a value, force it recursively (i.e. all the way to head
    /// normal form.) This really only affects tuples recursively.
    fn force(&self, val: Value) -> Result<Value, MatzoError> {
        match val {
            Value::Tup(values) => Ok(Value::Tup(
                values
                    .into_iter()
                    .map(|t| {
                        let v = self.hnf(&t)?;
                        let v = self.force(v)?;
                        Ok(Thunk::Value(v))
                    })
                    .collect::<Result<Vec<Thunk>, MatzoError>>()?,
            )),
            _ => Ok(val),
        }
    }

    /// Given a thunk, force it to WHNF: i.e. evaluate the outermost
    /// layer but not recursively.
    pub fn hnf(&self, thunk: &Thunk) -> Result<Value, MatzoError> {
        match thunk {
            Thunk::Expr(expr, env) => self.eval(*expr, env),
            // note: if this ever needs to be efficient, this would be
            // a great place to add a proper GC.
            Thunk::Value(val) => Ok(val.clone()),
            Thunk::Builtin(b) => Ok(Value::Builtin(*b)),
        }
    }

    /// Given an `ExprRef` and an environment, fetch that expression
    /// and then evalute it in that environment
    pub fn eval(&self, expr_ref: ExprRef, env: &Env) -> Result<Value, MatzoError> {
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
                let from = self
                    .eval(*from, env)?
                    .as_num(&self.ast.borrow(), from.loc)?;
                let to = self.eval(*to, env)?.as_num(&self.ast.borrow(), to.loc)?;
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
            Expr::Ap(func, vals) => match self.eval(*func, env)? {
                Value::Closure(c) => {
                    let scruts = vals.iter().map(|v| Thunk::Expr(*v, env.clone())).collect();
                    self.eval_closure(&c, scruts)
                }
                Value::Builtin(b) => {
                    let builtin = &self.builtins[b.idx];
                    (builtin.callback)(self, vals, env)
                }
                _ => Err(MatzoError::new(
                    expr_ref.loc,
                    "Trying to call a non-function".to_string(),
                )),
            },

            // for a let-expression, create a new scope, add the new
            // name to it (optionally forcing it if `fixed`) and then
            // evaluate the body within that scope.
            Expr::Let(binding, body) => {
                let mut new_scope = HashMap::new();
                self.extend_scope(binding, env, &mut new_scope)?;
                let new_scope = Rc::new(Scope {
                    vars: new_scope,
                    parent: env.clone(),
                });
                self.eval(*body, &Some(new_scope))
            }

            // For a `case`, we actually kind of cheat: we treat it
            // like we're creating and immediately applying a
            // function. A `case` also only takes one argument, while
            // a function takes more than one, so this is 'punning' in
            // a kind of gross way, but... it works and it doesn't
            // repeat code, so, uh, yay, I guess?
            Expr::Case(scrut, _) => {
                let closure = Closure {
                    func: expr_ref,
                    scope: env.clone(),
                };
                self.eval_closure(&closure, vec![Thunk::Expr(*scrut, env.clone())])
            }
        }
    }

    fn extend_scope(
        &self,
        binding: &Binding,
        env: &Env,
        scope: &mut HashMap<StrRef, Thunk>,
    ) -> Result<(), MatzoError> {
        if binding.fixed {
            let val = self.eval(binding.expr, env)?;
            let val = self.force(val)?;
            scope.insert(binding.name.item, Thunk::Value(val));
        } else {
            scope.insert(binding.name.item, Thunk::Expr(binding.expr, env.clone()));
        }
        Ok(())
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
    /// {[Foo] => "1"; [Foo] => "2"; _ => "..."}[Foo | Bar]
    /// ```
    ///
    /// It should be impossible to get `"2"` in this case. That means
    /// that we need to force the argument _and keep branching against
    /// the already-forced argument_. But we also want the following
    /// to still contain non-determinism:
    ///
    /// ```ignore
    /// {[<Foo, x>] => x x "!"; [<Bar, x>] => x x "?"}[<Foo | Bar, "a" | "b">]
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
    pub fn eval_closure(
        &self,
        closure: &Closure,
        mut scruts: Vec<Thunk>,
    ) -> Result<Value, MatzoError> {
        let ast = self.ast.borrow();
        let cases = match &ast[closure.func] {
            Expr::Fun(cases) => cases,
            Expr::Case(_, cases) => cases,
            // see the note attached to the definition of `Closure`
            other => panic!("Expected a `Fun` or `Case` in a closure, found {:?}", other),
        };

        // for each case
        'cases: for c in cases {
            // build a set of potential bindings, which `match_pat`
            // will update if it finds matching variables
            let mut bindings = Vec::new();
            if scruts.len() != c.pats.len() {
                continue;
            }
            for (scrut, pat) in scruts.iter_mut().zip(c.pats.iter()) {
                if !self.match_pat(pat, scrut, &mut bindings)? {
                    // if we didn't match, we don't care about any
                    // bindings we've found: simply skip it
                    continue 'cases;
                }
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
        let mut buf = String::new();
        if scruts.len() != 1 {
            let arena = self.ast.borrow();
            buf.push('[');
            for (i, scrut) in scruts.iter().enumerate() {
                if i != 0 {
                    buf.push_str(", ");
                }
                scrut.with_str(&arena, |s| buf.push_str(s));
            }
            buf.push(']');
        } else {
            scruts[0].with_str(&self.ast.borrow(), |s| buf.push_str(s));
        }
        Err(MatzoError::new(
            closure.func.loc,
            format!("No pattern matched {}", buf),
        ))
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
    ) -> Result<bool, MatzoError> {
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

    // this chooses an expression from a choice, taking into account
    // the weights
    fn choose(&self, choices: &[Choice], env: &Env) -> Result<Value, MatzoError> {
        let max = choices.iter().map(Choice::weight).sum();
        let mut choice = self.rand.borrow_mut().gen_range_i64(0, max);
        for ch in choices {
            if choice < ch.weight() {
                return self.eval(ch.value, env);
            }
            choice -= ch.weight();
        }

        // if we got here, it means our math was wrong
        panic!("unreachable (bad math in `choose`)")
    }
}
