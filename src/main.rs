use matzo::grammar::StmtsParser;
use matzo::interp::State;
use matzo::lexer::tokens;
use logos::Logos;

use std::io::Write;

fn run(src: &str) {
    let lexed = tokens(&src);
    let stmts = StmtsParser::new().parse(lexed).unwrap();
    let mut state = State::new();
    for stmt in stmts {
        if let Err(err) = state.execute(&stmt) {
            eprintln!("error: {}", err);
        }
    }
}

fn run_repl() -> std::io::Result<()> {
    let mut state = State::new();
    let parser = StmtsParser::new();
    let mut stdout = std::io::stdout();
    let stdin = std::io::stdin();
    let mut buf = String::new();

    loop {
        stdout.write(b">>> ")?;
        stdout.flush()?;
        buf.clear();
        stdin.read_line(&mut buf)?;
        let lexed = tokens(&buf);

        let stmts = match parser.parse(lexed) {
            Ok(stmts) => stmts,
            Err(err) => {
                eprintln!("{:?}", err);
                continue;
            }
        };
        for stmt in stmts {
            if let Err(err) = state.execute(&stmt) {
                eprintln!("error: {}", err);
            }
        }
    }
}

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<String>>();
    if args.is_empty() {
        run_repl().unwrap();
        return;
    }

    for arg in args {
        let buf = std::fs::read_to_string(arg).unwrap();
        run(&buf);
    }
}
