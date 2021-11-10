use matzo::interp::State;

fn matzo_version() -> String {
    format!("matzo (git {})", env!("VERGEN_GIT_SHA"))
}

fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::Editor::<matzo::repl::Repl>::new();
    let state = std::rc::Rc::new(std::cell::RefCell::new(State::new()));
    rl.set_helper(Some(matzo::repl::Repl::new(state.clone())));
    println!("{}", ansi_term::Colour::Blue.bold().paint(matzo_version()),);
    println!("{}", ansi_term::Colour::Blue.paint("(work-in-progress)"),);

    loop {
        let line = match rl.readline(">>> ") {
            Ok(ln) => ln,
            Err(rustyline::error::ReadlineError::Eof)
            | Err(rustyline::error::ReadlineError::Interrupted) => return Ok(()),
            err => err?,
        };

        if let Err(err) = state.borrow_mut().run_repl(&line) {
            eprintln!(
                "{} {}",
                ansi_term::Colour::Red.bold().paint("error:"),
                ansi_term::Colour::Red.paint(format!("{}", err)),
            );
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
        if let Err(err) = State::new().run(&buf) {
            eprintln!(
                "{} {}",
                ansi_term::Colour::Red.bold().paint("error:"),
                ansi_term::Colour::Red.paint(format!("{}", err)),
            );
        }
    }
}
