extern crate parserlib;

extern crate stdio;
use stdio::scanf;

fn main() {
//    println!("Text to parse:");
    let text_to_parse = scanf().expect("Please enter something");
    let mut tokens = parserlib::lex::get_tokens(text_to_parse);
//    println!("{:#?}", tokens);
    tokens.reverse();
    let ast = parserlib::parse::parse(tokens).expect("should give program");
//    println!("{:#?}", ast);
    let code = parserlib::gencode::gencode_program(ast);
//    println!("{:#?}", code);
    for instruction in code.iter() {
        println!("{}", parserlib::asmgen::translate_instruction(instruction));
    }
}
