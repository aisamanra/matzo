use crate::core::Span;
use crate::lexer;

#[derive(Debug)]
pub struct MatzoError {
    pub message: String,
    pub span: Span,
    pub context: Vec<ContextLine>,
}

#[derive(Debug)]
pub struct ContextLine {
    pub message: String,
    pub span: Span,
}

impl MatzoError {
    pub fn new(span: Span, message: String) -> MatzoError {
        MatzoError {
            message,
            span,
            context: Vec::new(),
        }
    }

    pub fn no_loc(message: String) -> MatzoError {
        MatzoError {
            message,
            span: Span::empty(),
            context: Vec::new(),
        }
    }

    pub fn reposition(mut self, span: Span) -> MatzoError {
        self.span = span;
        self
    }

    fn format_expected_list(expected: Vec<String>) -> String {
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

    pub fn from_parse_error(
        err: lalrpop_util::ParseError<usize, lexer::Token, lexer::LexerError>,
    ) -> Self {
        match err {
            lalrpop_util::ParseError::User { error } => {
                MatzoError::new(error.range, "Unrecognized token".to_string())
            }
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (start, tok, end),
                expected,
            } => {
                let span = Span {
                    start: start as u32,
                    end: end as u32,
                };

                let expected = MatzoError::format_expected_list(expected);
                MatzoError::new(
                    span,
                    format!("Unexpected {}. Expected {}", tok.token_name(), expected),
                )
            }
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => {
                let span = Span {
                    start: location as u32 - 1,
                    end: location as u32,
                };
                let expected = MatzoError::format_expected_list(expected);
                MatzoError::new(
                    span,
                    format!("Unexpected end-of-file; expected {}", expected),
                )
            }
            lalrpop_util::ParseError::InvalidToken { .. } => {
                panic!("Unexpected `InvalidToken`")
            }
            lalrpop_util::ParseError::ExtraToken {
                token: (start, tok, end),
            } => {
                let span = Span {
                    start: start as u32,
                    end: end as u32,
                };
                MatzoError::new(span, format!("Extra token {}", tok.token_name()))
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
