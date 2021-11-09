use matzo::grammar::StmtsParser;
use matzo::interp::State;
use matzo::lexer::tokens;

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

fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::Editor::<()>::new();
    let mut state = State::new();
    let parser = StmtsParser::new();
    println!("matzo interpreter");
    println!("(work-in-progress)");

    loop {
        let line = match rl.readline(">>> ") {
            Ok(ln) => ln,
            Err(rustyline::error::ReadlineError::Eof) |
            Err(rustyline::error::ReadlineError::Interrupted) =>
                return Ok(()),
            err => err?,
        };
        let lexed = tokens(&line);

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
