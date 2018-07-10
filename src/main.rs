extern crate parserlib;

extern crate stdio;
use std::io::prelude::*;

fn main() {
    let mut text_to_parse = String::new();
    let name: String = std::env::args()
        .skip(1)
        .next()
        .expect("must have filename")
        .to_string();
    let mut f = std::fs::File::open(name).expect("file not found");
    f.read_to_string(&mut text_to_parse)
        .expect("Something went wrong");

    let mut tokens = parserlib::lex::get_tokens(text_to_parse);
    tokens.reverse();
    let ast = parserlib::parse::parse(tokens).expect("should give program");
    let code = parserlib::gencode::gencode_program(ast);
    for instruction in code.iter() {
        println!(
            "{}",
            parserlib::amd64_asm::translate_instruction(instruction)
        );
    }
}
