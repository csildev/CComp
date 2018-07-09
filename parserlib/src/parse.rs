use lex;

#[derive(Debug)]
pub struct Program {
    funcs: Vec<Function>,
}

impl Program {
    pub fn new() -> Program {
        Program { funcs: Vec::new() }
    }
    pub fn get_funcs(&self) -> &Vec<Function> {
        &self.funcs
    }
}

#[derive(Debug)]
pub struct Function {
    name: Option<String>,
    stmts: Vec<Statement>,
}

impl Function {
    pub fn new() -> Function {
        Function {
            name: None,
            stmts: Vec::new(),
        }
    }

    pub fn get_name(&self) -> String {
        match self.name.clone() {
            Some(x) => x,
            None => panic!("Must have name"),
        }
    }

    pub fn get_stmts(&self) -> &Vec<Statement> {
        &self.stmts
    }
}

#[derive(Debug)]
pub enum Statement {
    NoStmt,
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    LogNegate(Box<Expression>),
    Complement(Box<Expression>),
    Negate(Box<Expression>),
    Int(String),
}

use lex::Token;
pub fn parse(mut tokens: Vec<lex::Token>) -> Result<Program, String> {
    let mut prog = Program::new();
    while let Some(Token::KWInt) = tokens.pop() {
        tokens.push(Token::KWInt);
        let (func, toks) = parse_func(tokens);
        tokens = toks;
        prog.funcs.push(func);
    }
    Ok(prog)
}

pub fn parse_func(mut tokens: Vec<lex::Token>) -> (Function, Vec<lex::Token>) {
    let mut func = Function::new();
    if let Some(Token::KWInt) = tokens.pop() {
    } else {
        panic!("Expected keyword 'int'");
    }
    if let Some(Token::Identifier(x)) = tokens.pop() {
        func.name = Some(x);
    } else {
        panic!("Expected function name");
    }

    if let Some(Token::LParen) = tokens.pop() {

    } else {
        panic!("Expected open parenthesis");
    }

    if let Some(Token::RParen) = tokens.pop() {
    } else {
        panic!("Expected close parenthesis");
    }

    if let Some(Token::LBracket) = tokens.pop() {
    } else {
        panic!("Expected open bracket");
    }

    loop {
        match tokens.pop() {
            Some(Token::RBracket) => {
                break;
            }
            Some(x) => {
                tokens.push(x);
                let (stmt, mut toks) = parse_stmt(tokens);
                tokens = toks;
                func.stmts.push(stmt);
            }
            None => {
                panic!("Expected token");
            }
        }
    }

    (func, tokens)
}

pub fn parse_stmt(mut tokens: Vec<lex::Token>) -> (Statement, Vec<lex::Token>) {
    match tokens.pop() {
        Some(Token::KWReturn) => {
            let (exp, mut tokens) = parse_exp(tokens);
            if let Some(Token::SemiColon) = tokens.pop() {
            } else {
                panic!("Expected semicolon to end statement");
            }
            (Statement::Return(exp), tokens)
        }
        Some(_) => {
            panic!("Unexpected token encountered");
        }
        None => {
            panic!("Expected keyword 'return'");
        }
    }
}

pub fn parse_exp(mut tokens: Vec<lex::Token>) -> (Expression, Vec<lex::Token>) {
    match tokens.pop() {
        Some(Token::Integer(s)) => (Expression::Int(s), tokens),
        Some(Token::Minus) => {
            let (exp, toks) = parse_exp(tokens);
            (Expression::Negate(Box::new(exp)), toks)
        }
        Some(Token::LogNegate) => {
            let (exp, toks) = parse_exp(tokens);
            (Expression::LogNegate(Box::new(exp)), toks)
        }
        Some(Token::Complement) => {
            let (exp, toks) = parse_exp(tokens);
            (Expression::Complement(Box::new(exp)), toks)
        }
        Some(_) => {
            panic!("Unexpected token");
        }
        None => {
            panic!("Expected token");
        }
    }
}
