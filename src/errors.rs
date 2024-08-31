use crate::core::{FileRef, Loc, Span};
use crate::lexer;

#[derive(Debug)]
pub struct MatzoError {
    pub message: String,
    pub loc: Loc,
    pub context: Vec<ContextLine>,
}

#[derive(Debug)]
pub struct ContextLine {
    pub message: String,
    pub loc: Loc,
}

impl MatzoError {
    #[must_use]
    pub fn new(loc: Loc, message: String) -> MatzoError {
        MatzoError {
            message,
            loc,
            context: Vec::new(),
        }
    }

    #[must_use]
    pub fn reposition(mut self, loc: Loc) -> MatzoError {
        self.loc = loc;
        self
    }

    fn format_expected_list(expected: &[String]) -> String {
        if expected.len() == 1 {
            expected[0].to_string()
        } else {
            let mut expected_list = String::new();
            let num = expected.len();
            for (i, exp) in expected.iter().enumerate() {
                if i > 0 {
                    expected_list.push_str(", ");
                }
                if i + 1 == num {
                    expected_list.push_str("or ");
                }
                expected_list.push_str(exp);
            }
            expected_list
        }
    }

    #[must_use]
    pub fn from_parse_error(
        file: FileRef,
        err: lalrpop_util::ParseError<usize, lexer::Wrap, lexer::LexerError>,
    ) -> Self {
        match err {
            lalrpop_util::ParseError::User { error } => MatzoError::new(
                Loc {
                    span: error.range,
                    file,
                },
                "Unrecognized token".to_string(),
            ),
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (start, tok, end),
                expected,
            } => {
                let span = Span {
                    start: start,
                    end: end,
                };

                let expected = MatzoError::format_expected_list(&expected);
                MatzoError::new(
                    Loc { span, file },
                    format!("Unexpected {}. Expected {expected}", tok.token_name()),
                )
            }
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                let span = Span {
                    start: location - 1,
                    end: location,
                };
                let expected = MatzoError::format_expected_list(&expected);
                MatzoError::new(
                    Loc { span, file },
                    format!("Unexpected end-of-file; expected {expected}"),
                )
            }
            lalrpop_util::ParseError::InvalidToken { .. } => {
                panic!("Unexpected `InvalidToken`")
            }
            lalrpop_util::ParseError::ExtraToken {
                token: (start, tok, end),
            } => {
                let span = Span {
                    start: start,
                    end: end,
                };
                MatzoError::new(
                    Loc { span, file },
                    format!("Extra token {}", tok.token_name()),
                )
            }
        }
    }
}

impl std::fmt::Display for MatzoError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "MatzoError {{ .. }}")
    }
}

impl std::error::Error for MatzoError {}
