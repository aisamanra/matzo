use crate::ast::{ExprRef, Span};
use crate::core::Loc;
use crate::errors::MatzoError;
use crate::value::Value;

pub fn arity_error(func: &str, expected: usize, actual: &[ExprRef]) -> Result<Value, MatzoError> {
    let msg = format!(
        "`{}`: expected {} argument{}, got {}",
        func,
        expected,
        if expected == 1 { "" } else { "s" },
        actual.len()
    );
    if actual.is_empty() {
        panic!("should not be possible to express")
        // Err(MatzoError::new(Span::empty(), msg))
    } else {
        let span = Span {
            start: actual[0].loc.span.start,
            end: actual[actual.len() - 1].loc.span.end,
        };
        let file = actual[0].loc.file;
        Err(MatzoError::new(Loc { span, file }, msg))
    }
}
