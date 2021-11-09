fn main() {
    println!("{:?}", matzo::grammar::StmtsParser::new().parse("puts 55"));
}
