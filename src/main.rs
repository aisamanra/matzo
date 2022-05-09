use matzo::interp::State;

use clap::{App, Arg};

#[derive(Debug)]
struct Opts {
    seed: Option<u64>,
    files: Vec<String>,
}

impl Opts {
    fn parse() -> Opts {
        let matches = App::new("Matzo")
            .setting(clap::AppSettings::TrailingVarArg)
            .version(format!("git {}", env!("VERGEN_GIT_SHA")).as_ref())
            .author("Getty Ritter <matzo@infinitenegativeutility.com>")
            .about("A language for random text")
            .arg(
                Arg::with_name("seed")
                    .short("s")
                    .long("seed")
                    .value_name("NUMBER")
                    .help("Sets a custom RNG seed")
                    .takes_value(true),
            )
            .arg(
                Arg::with_name("input")
                    .value_name("FILES")
                    .help("Files to evaluate")
                    .multiple(true)
                    .takes_value(true),
            )
            .get_matches();

        let seed = matches.value_of("seed").map(|s| s.parse().unwrap());
        let mut files = Vec::new();
        if let Some(fs) = matches.values_of("input") {
            files.extend(fs.map(|x| x.to_string()));
        }
        Opts { seed, files }
    }
}

fn matzo_version() -> String {
    format!("matzo (git {})", env!("VERGEN_GIT_SHA"))
}

fn run_repl(seed: Option<u64>) -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = rustyline::Editor::<matzo::repl::Repl>::new();
    let state = if let Some(s) = seed {
        State::new_from_seed(s)
    } else {
        State::new()
    };
    let state = std::rc::Rc::new(std::cell::RefCell::new(state));
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
    let opts = Opts::parse();

    if opts.files.is_empty() {
        run_repl(opts.seed).unwrap();
        return;
    }

    let state = if let Some(s) = opts.seed {
        State::new_from_seed(s)
    } else {
        State::new()
    };
    for arg in opts.files {
        let buf = std::fs::read_to_string(arg).unwrap();
        if let Err(err) = state.run(&buf) {
            eprintln!(
                "{} {}",
                ansi_term::Colour::Red.bold().paint("error:"),
                ansi_term::Colour::Red.paint(format!("{}", err)),
            );
        }
    }
}
