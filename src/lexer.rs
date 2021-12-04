use logos::{Lexer, Logos};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FileRef {
    pub idx: usize,
}

/// A location in a source file
#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn empty() -> Span {
        Span {
            start: u32::MAX,
            end: u32::MAX,
        }
    }

    pub fn exists(&self) -> bool {
        self.start != u32::MAX && self.end != u32::MAX
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Located<T> {
    pub item: T,
    pub span: Span,
    pub file: FileRef,
}

impl<T> Located<T> {
    pub fn new(item: T, file: FileRef, span: Span) -> Located<T> {
        Located { span, file, item }
    }
}

impl <T: Clone> Located<T> {
    pub fn map<R>(&self, func: impl FnOnce(T) -> R) -> Located<R> {
        Located {
            item: func(self.item.clone()),
            span: self.span,
            file: self.file,
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

    #[regex(r"\p{Ll}(\pL|[0-9_-])*")]
    Var(&'a str),

    #[regex(r"\p{Lu}(\pL|[0-9_-])*")]
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

#[derive(Debug)]
pub struct LexerError;

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "LexerError")
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub fn tokens(source: &str) -> impl Iterator<Item = Spanned<Token<'_>, usize, LexerError>> {
    Token::lexer(source)
        .spanned()
        .map(move |(token, range)| match token {
            Token::Error => Err(LexerError),
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
