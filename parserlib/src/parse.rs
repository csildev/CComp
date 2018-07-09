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
    Binary(BinOp, Box<Expression>, Box<Term>),
    Term(Term),
}

#[derive(Debug)]
pub enum Term {
    Binary(BinOp, Box<Term>, Box<Factor>),
    Factor(Factor),
}

#[derive(Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
}

#[derive(Debug)]
pub enum Factor {
    Unary(UnOp, Box<Factor>),
    Wrapped(Box<Expression>),
    Int(String),
}

#[derive(Debug)]
pub enum UnOp {
    Negate,
    LogNegate,
    Complement,
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

pub fn parse_exp(tokens: Vec<lex::Token>) -> (Expression, Vec<lex::Token>) {
    let (tm, mut tokens) = parse_term(tokens);
    let mut term = Expression::Term(tm);
    let mut next = tokens
        .pop()
        .expect("Unexpected end of file in binary expression");
    while next == Token::Plus || next == Token::Minus {
        tokens.push(next.clone());
        let (op, toks) = parse_op(tokens);
        let (next_term, toks) = parse_term(toks);
        tokens = toks;
        term = Expression::Binary(op, Box::new(term), Box::new(next_term));
        next = tokens
            .pop()
            .expect("Unexpected end of file when parsing expression");
    }
    tokens.push(next);
    (term, tokens)
}

pub fn parse_term(tokens: Vec<lex::Token>) -> (Term, Vec<lex::Token>) {
    let (fc, mut tokens) = parse_factor(tokens);
    let mut factor = Term::Factor(fc);
    let mut next = tokens
        .pop()
        .expect("Unexpected end of file in binary expression");
    while next == Token::Times || next == Token::Divide {
        tokens.push(next.clone());
        let (op, toks) = parse_op(tokens);
        let (next_term, toks) = parse_factor(toks);
        tokens = toks;
        factor = Term::Binary(op, Box::new(factor), Box::new(next_term));
        next = tokens
            .pop()
            .expect("Unexpected end of file when parsing term");
    }
    tokens.push(next);
    (factor, tokens)
}

pub fn parse_factor(mut tokens: Vec<lex::Token>) -> (Factor, Vec<lex::Token>) {
    match tokens.pop() {
        Some(Token::Integer(s)) => (Factor::Int(s), tokens),
        Some(Token::Minus) => {
            let (fact, toks) = parse_factor(tokens);

            (Factor::Unary(UnOp::Negate, Box::new(fact)), toks)
        }
        Some(Token::LogNegate) => {
            let (fact, toks) = parse_factor(tokens);
            (Factor::Unary(UnOp::LogNegate, Box::new(fact)), toks)
        }
        Some(Token::Complement) => {
            let (fact, toks) = parse_factor(tokens);
            (Factor::Unary(UnOp::Complement, Box::new(fact)), toks)
        }
        Some(Token::LParen) => {
            let (expr, mut toks) = parse_exp(tokens);
            if let Some(Token::RParen) = toks.pop(){
            } else {
                panic!("Expected closing parenthesis");
            }
            (Factor::Wrapped(Box::new(expr)), toks)
        }
        Some(_) => {
            panic!("Unexpected token");
        }
        None => {
            panic!("Expected token");
        }
    }
}

pub fn parse_op(mut tokens: Vec<lex::Token>) -> (BinOp, Vec<lex::Token>) {
    match tokens
        .pop()
        .expect("Unexpected end of file in parsing sequence")
    {
        Token::Plus => (BinOp::Plus, tokens),
        Token::Minus => (BinOp::Minus, tokens),
        Token::Times => (BinOp::Times, tokens),
        Token::Divide => (BinOp::Divide, tokens),
        _ => panic!("unexpected token encountered, operator expected"),
    }
}
