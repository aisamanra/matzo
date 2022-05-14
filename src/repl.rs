use rustyline::{
    completion::Completer, highlight::Highlighter, hint::Hinter, validate::Validator, Helper,
};
use std::cell::RefCell;
use std::rc::Rc;

pub struct Repl {
    state: Rc<RefCell<crate::interp::State>>,
}

impl Repl {
    pub fn new(state: Rc<RefCell<crate::interp::State>>) -> Repl {
        Repl { state }
    }
}

impl Completer for Repl {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<String>)> {
        if pos == 0 {
            return Ok((pos, Vec::new()));
        }

        if let Some(c) = line.chars().nth(pos - 1) {
            if c.is_alphabetic() {
                // this means we're looking at maybe something
                // alphabetic; let's see what the current typed thing
                // is
                let mut str_start = 0;
                for (idx, ch) in line.chars().enumerate() {
                    if ch.is_whitespace() {
                        str_start = idx + 1;
                    }
                    if idx == pos {
                        break;
                    }
                }
                // we've now found the current fragment
                let so_far = &line[str_start..pos];
                return Ok((
                    str_start,
                    self.state.borrow().autocomplete(so_far, str_start == 0),
                ));
            }
        }
        Ok((pos, Vec::new()))
    }
}

impl Hinter for Repl {
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, _ctx: &rustyline::Context<'_>) -> Option<String> {
        if pos == 0 {
            return None;
        }
        if let Some(c) = line.chars().nth(pos - 1) {
            if c.is_alphabetic() {
                // this means we're looking at maybe something
                // alphabetic; let's see what the current typed thing
                // is
                let mut str_start = 0;
                for (idx, ch) in line.chars().enumerate() {
                    if ch.is_whitespace() {
                        str_start = idx + 1;
                    }
                    if idx == pos {
                        break;
                    }
                }
                // don't suggest for stuff that's too short
                if pos - str_start < 2 {
                    return None;
                }
                // we've now found the current fragment
                let so_far = &line[str_start..pos];
                let autocompletes = self.state.borrow().autocomplete(so_far, str_start == 0);
                if autocompletes.len() == 1 {
                    let known = autocompletes.first().unwrap();
                    return known
                        .strip_prefix(so_far)
                        .map(|s| ansi_term::Colour::Blue.dimmed().paint(s).to_string());
                } else {
                    return None;
                }
            }
        }
        None
    }
}

impl Highlighter for Repl {}

impl Validator for Repl {}

impl Helper for Repl {}
