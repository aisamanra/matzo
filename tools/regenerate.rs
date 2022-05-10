use matzo::grammar;
use matzo::interp;
use matzo::lexer;

use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::io::Write;

const MAX_RUN_EXPECTATIONS: u64 = 64;

fn generate_runs(source: &str) -> Result<BTreeMap<String, String>, Box<dyn std::error::Error>> {
    let mut found_results: BTreeMap<String, u64> = BTreeMap::new();
    for seed in 0..MAX_RUN_EXPECTATIONS {
        let state = interp::State::new_from_seed(seed);
        let mut out = Vec::new();
        state.run_with_writer(source, &mut out)?;
        let out = std::str::from_utf8(&out).unwrap().trim().to_string();
        if let Entry::Vacant(e) = found_results.entry(out) {
            e.insert(seed);
        }
    }
    let output = found_results
        .into_iter()
        .map(|(k, v)| (format!("{}", v), k))
        .collect();
    Ok(output)
}

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
            let file = ast.add_file(src.to_string());
            if let Ok(stmts) = grammar::StmtsParser::new().parse(&mut ast, file, tokens) {
                let mut f = std::fs::File::create(exp_filename("parsed"))?;
                for stmt in stmts {
                    writeln!(f, "{:#?}", stmt.show(&ast))?;
                }
            }

            if let Ok(existing) = std::fs::read_to_string(exp_filename("output")) {
                let existing: BTreeMap<String, String> = serde_yaml::from_str(&existing)?;
                print!("  generating output for {}: ", fname);
                let map = generate_runs(&src)?;
                if map != existing {
                    let mut f = std::fs::File::create(exp_filename("output"))?;
                    writeln!(f, "# generated for {}", env!("VERGEN_GIT_SHA"))?;
                    writeln!(f, "{}", serde_yaml::to_string(&map)?)?;
                    println!("generated {} cases", map.len());
                } else {
                    println!("no changes needed");
                }
            }
        }
    }

    Ok(())
}
