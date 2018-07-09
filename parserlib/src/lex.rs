#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    KWInt,
    KWReturn,
    Identifier(String),
    Integer(String),
    LParen,
    RParen,
    LSquare,
    RSquare,
    LBracket,
    RBracket,
    SemiColon,
    FullColon,
    Ampersand,
    AtSign,
    Exclamation,
    BackSlash,
    Slash,
    Lt,
    LtE,
    Gt,
    GtE,
    Eq,
    EqEq,
    NotEq,
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
    Exponent,
    Dollar,
    Hash,
    Complement,
    LogNegate,
    BoolAnd,
    BoolOr,
    Or,
    RShift,
    LShift,
}

pub fn get_tokens(code: String) -> Vec<Token> {
    get_tokens_vec(code.chars().collect())
}

pub fn get_tokens_vec(mut code: Vec<char>) -> Vec<Token> {
    let mut tokens = Vec::<Token>::new();
    while code.len() > 0 {
        code = code[0..]
            .iter()
            .collect::<String>()
            .trim()
            .chars()
            .collect();
        match get_token(&code) {
            Ok((tok, cod)) => {
                tokens.push(tok);
                code = cod;
            }
            Err(string) => {
                println!("ERROR: {}", string);
                panic!("{}", string);
            }
        }
    }
    tokens
}

fn get_token(code: &Vec<char>) -> Result<(Token, Vec<char>), String> {
    match code[0] {
        'a'...'z' | 'A'...'Z' => get_identifier(code),
        '0'...'9' => get_number(code),
        '{' => make_sc_token(Token::LBracket, code),
        '}' => make_sc_token(Token::RBracket, code),
        '(' => make_sc_token(Token::LParen, code),
        ')' => make_sc_token(Token::RParen, code),
        '[' => make_sc_token(Token::LSquare, code),
        ']' => make_sc_token(Token::RSquare, code),
        '-' => make_sc_token(Token::Minus, code),
        '~' => make_sc_token(Token::Complement, code),
        ';' => make_sc_token(Token::SemiColon, code),
        '+' => make_sc_token(Token::Plus, code),
        '*' => make_sc_token(Token::Times, code),
        '/' => make_sc_token(Token::Divide, code),
        '&' | '|' | '!' | '<' | '>' | '=' => make_boolean_token(code),
        _ => Err("Incorrect Character encountered in parsing sequence".to_string()),
    }
}

fn make_boolean_token(code: &Vec<char>) -> Result<(Token, Vec<char>), String>{
    match code[0] {
        '&' => {
            return if code.len() > 1 && '&' == code[1] {
                Ok((Token::BoolAnd, code[2..].to_vec()))
            } else  {
                Ok((Token::Ampersand, code[1..].to_vec()))
            }
        } 
        '|' => {
            return if code.len() > 1 && '|' == code[1] {
                Ok((Token::BoolOr, code[2..].to_vec()))
            } else {
                Ok((Token::Or, code[1..].to_vec()))
            }
        }
        '!' => {
            return if code.len() > 1  && '=' == code[1] {
                Ok((Token::NotEq, code[2..].to_vec()))
            } else {
                Ok((Token::LogNegate, code[1..].to_vec()))
            }
        }
        '<' => {
            if code.len() > 1 {
                return match code[1] {
                    '<' => Ok((Token::LShift, code[2..].to_vec())),
                    '=' => Ok((Token::LtE, code[2..].to_vec())),
                    _ => Ok((Token::Lt, code[1..].to_vec())),
                }
            } else {
                return Ok((Token::Lt, code[1..].to_vec()))
            }
        }
        '>' => {
            if code.len() > 1 {
                return match code[1] {
                    '>' => Ok((Token::RShift, code[2..].to_vec())),
                    '=' => Ok((Token::GtE, code[2..].to_vec())),
                    _ => Ok((Token::Lt, code[1..].to_vec())),
                }
            } else {
                return Ok((Token::Lt, code[1..].to_vec()))
            }
        }
        '=' => {
            if code.len() > 1 {
                return match code[1] {
                    '=' => Ok((Token::EqEq, code[2..].to_vec())),
                    _ => Ok((Token::Eq, code[1..].to_vec())),
                }
            } else {
                return Ok((Token::Eq, code[1..].to_vec()))
            }
        }
        _ =>  Err("Somehow a random character got past the function guards".to_string())
    }
}

fn make_sc_token(tok: Token, code: &Vec<char>) -> Result<(Token, Vec<char>), String> {
    Ok((tok, code[1..].to_vec()))
}

fn get_identifier(code: &Vec<char>) -> Result<(Token, Vec<char>), String> {
    let mut length = 0;
    while length < code.len() {
        match code[length] {
            'a'...'z' | 'A'...'Z' | '_' | '0'...'9' => {
                length = length + 1;
            }
            _ => {
                break;
            }
        }
    }
    let ident: String = code[0..length].iter().collect();
    match ident.as_ref() {
        "int" => Ok((Token::KWInt, code[length..].to_vec())),
        "return" => Ok((Token::KWReturn, code[length..].to_vec())),
        _ => Ok((Token::Identifier(ident), code[length..].to_vec())),
    }
}

fn get_number(code: &Vec<char>) -> Result<(Token, Vec<char>), String> {
    let mut length = 0;
    while length < code.len() {
        match code[length] {
            '0'...'9' | '_' => {
                length = length + 1;
            }
            _ => {
                break;
            }
        }
    }
    Ok((
        Token::Integer(code[0..length].iter().collect()),
        code[length..].to_vec(),
    ))
}
