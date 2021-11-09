use logos::{Lexer, Logos};

fn parse_num<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<i64> {
    let slice = lex.slice();
    Some(slice.parse().ok()?)
}

fn parse_str<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<String> {
    let mut buf = String::new();
    let s = lex.slice();
    let mut src = s[1..s.len() - 1].chars().into_iter();
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

    #[token("..")]
    DotDot,

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
