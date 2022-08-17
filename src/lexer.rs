pub use crate::core::{FileRef, Loc, Span};
use logos::{Lexer, Logos};

#[derive(Debug, Clone, Copy)]
pub struct Located<T> {
    pub item: T,
    pub loc: Loc,
}

impl<T> Located<T> {
    pub fn new(item: T, file: FileRef, span: Span) -> Located<T> {
        Located {
            loc: Loc { file, span },
            item,
        }
    }
}

impl<T: Clone> Located<T> {
    pub fn map<R>(&self, func: impl FnOnce(T) -> R) -> Located<R> {
        Located {
            item: func(self.item.clone()),
            loc: self.loc,
        }
    }
}

fn parse_num<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<i64> {
    let slice = lex.slice();
    slice.parse().ok()
}

fn parse_str<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<String> {
    let mut buf = String::new();
    let s = lex.slice();
    let mut src = s[1..s.len() - 1].chars();
    while let Some(c) = src.next() {
        if c == '\\' {
            match src.next() {
                Some('n') => buf.push('\n'),
                Some('t') => buf.push('\t'),
                Some('r') => buf.push('\r'),
                Some(c) => buf.push(c),
                None => return None,
            }
        } else {
            buf.push(c);
        }
    }
    Some(buf)
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token<'a> {
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,

    #[token("(")]
    LPar,
    #[token(")")]
    RPar,

    #[token("{")]
    LCurl,
    #[token("}")]
    RCurl,

    #[token("[")]
    LBrac,
    #[token("]")]
    RBrac,

    #[token("|")]
    Pipe,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(";")]
    Semi,

    #[token(".")]
    Dot,

    #[token("_")]
    Underscore,

    #[token("..")]
    DotDot,

    #[token("=>")]
    Arrow,

    #[token(":=")]
    Assn,

    #[token("::=")]
    LitAssn,

    #[token("puts")]
    Puts,

    #[token("case")]
    Case,

    #[token("let")]
    Let,

    #[token("in")]
    In,

    #[token("fix")]
    Fix,

    #[token("fn")]
    Fn,

    #[token("record")]
    Record,

    #[regex(r"\p{Ll}(\pL|[0-9_/-])*")]
    Var(&'a str),

    #[regex(r"\p{Lu}(\pL|[0-9_/-])*")]
    Atom(&'a str),

    #[regex(r"[0-9]+", parse_num)]
    Num(i64),

    #[regex(r"'([^'\\]|\\.)*'", parse_str)]
    #[regex("\"([^\"\\\\]|\\\\.)*\"", parse_str)]
    Str(String),

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"\(\*([^*]|\*[^)])*\*\)", logos::skip)]
    Error,
}

impl<'a> Token<'a> {
    pub fn token_name(&self) -> String {
        match self {
            Token::Var(v) => format!("variable `{}`", v),
            Token::Atom(a) => format!("atom `{}`", a),
            Token::Num(n) => format!("number `{}`", n),
            Token::Str(s) => format!("string `{}`", s),

            Token::LAngle => "`<`".to_string(),
            Token::RAngle => "`>`".to_string(),

            Token::LPar => "`(`".to_string(),
            Token::RPar => "`)`".to_string(),

            Token::LCurl => "`{`".to_string(),
            Token::RCurl => "`}`".to_string(),

            Token::LBrac => "`[`".to_string(),
            Token::RBrac => "`]`".to_string(),

            Token::Pipe => "`|`".to_string(),

            Token::Colon => "`:`".to_string(),
            Token::Comma => "`,`".to_string(),
            Token::Semi => "`;`".to_string(),

            Token::Dot => "`.`".to_string(),
            Token::Underscore => "`_`".to_string(),
            Token::DotDot => "`..`".to_string(),
            Token::Arrow => "`=>`".to_string(),
            Token::Assn => "`:=`".to_string(),

            Token::LitAssn => "`::=`".to_string(),
            Token::Puts => "`puts`".to_string(),
            Token::Case => "`case`".to_string(),
            Token::Let => "`let`".to_string(),
            Token::In => "`in`".to_string(),
            Token::Fix => "`fix`".to_string(),
            Token::Fn => "`fn`".to_string(),
            Token::Record => "`record`".to_string(),

            Token::Error => "error".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct LexerError {
    pub range: Span,
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "LexerError({}..{})", self.range.start, self.range.end)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub fn tokens(source: &str) -> impl Iterator<Item = Spanned<Token<'_>, usize, LexerError>> {
    Token::lexer(source)
        .spanned()
        .map(move |(token, range)| match token {
            Token::Error => Err(LexerError {
                range: Span {
                    start: range.start as u32,
                    end: range.end as u32,
                },
            }),
            token => Ok((range.start, token, range.end)),
        })
}

#[cfg(test)]
mod test {
    use logos::Logos;

    use super::Token;

    #[test]
    fn simple_lexer_test() {
        let mut lex = Token::lexer("x := Foo (* ignore *) | \"bar\";");
        assert_eq!(lex.next(), Some(Token::Var("x")));
        assert_eq!(lex.next(), Some(Token::Assn));
        assert_eq!(lex.next(), Some(Token::Atom("Foo")));
        assert_eq!(lex.next(), Some(Token::Pipe));
        assert_eq!(lex.next(), Some(Token::Str("bar".to_owned())));
        assert_eq!(lex.next(), Some(Token::Semi));
        assert_eq!(lex.next(), None)
    }
}
