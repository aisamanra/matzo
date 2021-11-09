use matzo::grammar;

use std::io::Write;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    for exp in std::fs::read_dir("tests")? {
        let exp = exp?.path().canonicalize()?;
        let fname = exp.file_name().ok_or("bad")?.to_string_lossy();
        if let Some(prefix) = fname.strip_suffix(".matzo") {
            println!("regenerating {}.matzo", prefix);
            let exp_filename = |new_suffix| {
                let mut f = exp.clone();
                f.pop();
                f.push(format!("{}.{}", prefix, new_suffix));
                f
            };

            let src = std::fs::read_to_string(&exp)?;
            if let Ok(ast) = grammar::StmtsParser::new().parse(&src) {
                let mut f = std::fs::File::create(exp_filename("parsed"))?;
                writeln!(f, "{:#?}", ast)?;
            }
        }
    }

    Ok(())
}
