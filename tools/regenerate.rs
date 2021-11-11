use matzo::grammar;
use matzo::lexer;

use std::io::Write;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    for exp in std::fs::read_dir("tests")? {
        let exp = exp?.path().canonicalize()?;
        let fname = exp.file_name().ok_or("bad file name")?.to_string_lossy();
        if let Some(prefix) = fname.strip_suffix(".matzo") {
            println!("regenerating {}.matzo", prefix);
            let exp_filename = |new_suffix| {
                let mut f = exp.clone();
                f.pop();
                f.push(format!("{}.{}", prefix, new_suffix));
                f
            };

            let mut ast = matzo::ast::ASTArena::new();
            let src = std::fs::read_to_string(&exp)?;
            let tokens = lexer::tokens(&src);
            if let Ok(stmts) = grammar::StmtsParser::new().parse(&mut ast, tokens) {
                let mut f = std::fs::File::create(exp_filename("parsed"))?;
                for stmt in stmts {
                    writeln!(f, "{:#?}", stmt.show(&ast))?;
                }
            }
        }
    }

    Ok(())
}
