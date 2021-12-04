use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

const TEST_PREFIX: &str = "
#[cfg(test)]
use pretty_assertions::assert_eq;
use crate::{grammar,lexer};
use std::collections::BTreeMap;
use std::io::Write;

// to let us use pretty_assertions with strings, we write a newtype
// that delegates Debug to Display
#[derive(PartialEq, Eq)]
struct StringWrapper<'a> {
  wrapped: &'a str,
}

impl<'a> core::fmt::Debug for StringWrapper<'a> {
  fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    core::fmt::Display::fmt(self.wrapped, f)
  }
}

fn assert_eq(x: &str, y: &str) {
  assert_eq!(StringWrapper {wrapped: x}, StringWrapper {wrapped: y});
}

fn get_expectations(contents: &str) -> BTreeMap<u64, String> {
  let map: BTreeMap<String, String> = serde_yaml::from_str(contents).unwrap();
  let mut result = BTreeMap::new();
  for (k, v) in map.into_iter() {
    result.insert(k.parse().unwrap(), v);
  }
  result
}
";

const TEST_TEMPLATE: &str = "
#[test]
fn test_%PREFIX%() {
  let state = crate::interp::State::new();
  let source = include_str!(\"%ROOT%/tests/%PREFIX%.matzo\");
  let lexer = lexer::tokens(source);
  let file = state.get_ast().borrow_mut().add_file(source.to_string());
  let stmts = grammar::StmtsParser::new().parse(&mut state.get_ast().borrow_mut(), file, lexer);
  assert!(stmts.is_ok());
  let stmts = stmts.unwrap();

  if let Ok(expected) = std::fs::read_to_string(\"%ROOT%/tests/%PREFIX%.parsed\") {
    let mut buf = Vec::new();
    for s in stmts.iter() {
      writeln!(buf, \"{:?}\", s.show(&state.get_ast().borrow())).unwrap();
    }
    assert_eq(
      std::str::from_utf8(&buf).unwrap().trim(),
      expected.trim(),
    );
  }

  if let Ok(contents) = std::fs::read_to_string(\"%ROOT%/tests/%PREFIX%.output\") {
    let expectations = get_expectations(&contents);
    for (seed, expectation) in expectations {
      let st = crate::interp::State::new_from_seed(seed);
      let mut out = Vec::new();
      st.run_with_writer(source, &mut out).unwrap();
      assert_eq(
        std::str::from_utf8(&out).unwrap().trim(),
        expectation.trim(),
      );
    }
  }
}
";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    lalrpop::process_root()?;

    for file in ["build.rs", "src/grammar.lalrpop"] {
        println!("cargo:rerun-if-changed={}", file);
    }

    vergen::vergen(vergen::Config::default())?;

    let out_dir = env::var("OUT_DIR")?;
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let dest = Path::new(&out_dir).join("exp_tests.rs");
    let mut test_file = File::create(&dest)?;
    writeln!(test_file, "{}", TEST_PREFIX)?;

    for exp in std::fs::read_dir("tests")? {
        let exp = exp?.path().canonicalize()?;
        let fname = exp.file_name().ok_or("bad file name")?.to_string_lossy();
        if let Some(prefix) = fname.strip_suffix(".matzo") {
            let test = TEST_TEMPLATE
                .replace("%FILE%", &fname)
                .replace("%PREFIX%", prefix)
                .replace("%ROOT%", &manifest_dir);
            writeln!(test_file, "{}", test)?;
        }
    }

    Ok(())
}
