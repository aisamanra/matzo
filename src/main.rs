fn main() {
    println!("{:?}", matzo::grammar::StmtsParser::new().parse("puts Foo Bar | Baz"));
}
