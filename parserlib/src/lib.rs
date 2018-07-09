pub mod amd64_asm;
pub mod gencode;
pub mod lex;
pub mod parse;
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
