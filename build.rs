use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

const TEST_PREFIX: &str = "
#[cfg(test)]
use pretty_assertions::assert_eq;
use crate::grammar;
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
";

const TEST_TEMPLATE: &str = "
#[test]
fn test_%PREFIX%() {
  let source = include_str!(\"%ROOT%/tests/%PREFIX%.matzo\");
  let ast = grammar::StmtsParser::new().parse(source);
  assert!(ast.is_ok());
  let ast = ast.unwrap();

  let mut buf = Vec::new();
  writeln!(buf, \"{:#?}\", ast).unwrap();
  assert_eq(
    std::str::from_utf8(&buf).unwrap().trim(),
    include_str!(\"%ROOT%/tests/%PREFIX%.parsed\").trim(),
  );
}
";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    lalrpop::process_root()?;

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
