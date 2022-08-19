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

fn parse_escapes<'a>(src: &'a str) -> Option<String> {
    let mut buf = String::new();
    let mut src = src.chars();
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

fn parse_str<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<String> {
    let s = lex.slice();
    parse_escapes(&s[1..s.len() - 1])
}

fn parse_fragment<'a>(lex: &mut Lexer<'a, FStringToken<'a>>) -> Option<String> {
    parse_escapes(lex.slice())
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

    #[token("with")]
    With,

    #[regex(r"\p{Ll}(\pL|[0-9_/-])*")]
    Var(&'a str),

    #[regex(r"\p{Lu}(\pL|[0-9_/-])*")]
    Atom(&'a str),

    #[regex(r"[0-9]+", parse_num)]
    Num(i64),

    #[regex(r"'([^'\\]|\\.)*'", parse_str)]
    #[regex("\"([^\"\\\\]|\\\\.)*\"", parse_str)]
    Str(String),

    #[token("`")]
    FStringStart,

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
            Token::With => "`with`".to_string(),

            Token::FStringStart => "`".to_string(),

            Token::Error => "error".to_string(),
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum FStringToken<'a> {
    #[regex(r"([^`,\\]|\\.)*", parse_fragment)]
    Text(String),

    #[token("`")]
    FStringEnd,

    #[regex(r",\p{Ll}(\pL|[0-9_/-])*", |s| &s.slice()[1..])]
    Var(&'a str),

    #[error]
    Error,
}

impl<'a> FStringToken<'a> {
    fn token_name(&self) -> String {
        match self {
            FStringToken::Text(s) => format!("fragment `{}`", s),
            FStringToken::Var(v) => format!("variable `{}`", v),
            FStringToken::FStringEnd => "`".to_string(),
            FStringToken::Error => "error".to_string(),
        }
    }
}

pub enum WrappedLexer<'a> {
    Regular(Lexer<'a, Token<'a>>),
    FString(Lexer<'a, FStringToken<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Wrap<'a> {
    R(Token<'a>),
    F(FStringToken<'a>),
}

impl<'a> Wrap<'a> {
    pub fn token_name(&self) -> String {
        match self {
            Wrap::R(tok) => tok.token_name(),
            Wrap::F(tok) => tok.token_name(),
        }
    }
}

struct MatzoLexer<'a> {
    mode: WrappedLexer<'a>,
}

impl<'a> MatzoLexer<'a> {
    pub fn lex(src: &'a str) -> MatzoLexer<'a> {
        MatzoLexer {
            mode: WrappedLexer::Regular(Token::lexer(src)),
        }
    }
}

impl<'a> Iterator for MatzoLexer<'a> {
    type Item = (Wrap<'a>, logos::Span);

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.mode {
            WrappedLexer::Regular(inner) => {
                let result = inner.next();
                let span = inner.span();
                if let Some(Token::FStringStart) = result {
                    self.mode = WrappedLexer::FString(inner.to_owned().morph());
                }
                result.map(|tok| (Wrap::R(tok), span))
            }
            WrappedLexer::FString(inner) => {
                let result = inner.next();
                let span = inner.span();
                if let Some(FStringToken::FStringEnd) = result {
                    self.mode = WrappedLexer::Regular(inner.to_owned().morph());
                }
                result.map(|tok| (Wrap::F(tok), span))
            }
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

pub fn tokens(source: &str) -> impl Iterator<Item = Spanned<Wrap<'_>, usize, LexerError>> {
    MatzoLexer::lex(source).map(move |(token, range)| match token {
        Wrap::R(Token::Error) | Wrap::F(FStringToken::Error) => Err(LexerError {
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

    use super::{FStringToken, MatzoLexer, Token, Wrap};

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

    #[test]
    fn fstring_test() {
        let mut lex = MatzoLexer::lex("puts `foo ,bar baz ,quux whatever`;");
        assert_eq!(lex.next().map(|r| r.0), Some(Wrap::R(Token::Puts)));
        assert_eq!(lex.next().map(|r| r.0), Some(Wrap::R(Token::FStringStart)));
        assert_eq!(
            lex.next().map(|r| r.0),
            Some(Wrap::F(FStringToken::Text("foo ".to_owned())))
        );
        assert_eq!(
            lex.next().map(|r| r.0),
            Some(Wrap::F(FStringToken::Var("bar")))
        );
        assert_eq!(
            lex.next().map(|r| r.0),
            Some(Wrap::F(FStringToken::Text(" baz ".to_owned())))
        );
        assert_eq!(
            lex.next().map(|r| r.0),
            Some(Wrap::F(FStringToken::Var("quux")))
        );
        assert_eq!(
            lex.next().map(|r| r.0),
            Some(Wrap::F(FStringToken::Text(" whatever".to_owned())))
        );
        assert_eq!(
            lex.next().map(|r| r.0),
            Some(Wrap::F(FStringToken::FStringEnd))
        );
        assert_eq!(lex.next().map(|r| r.0), Some(Wrap::R(Token::Semi)));
        assert_eq!(lex.next().map(|r| r.0), None)
    }
}
