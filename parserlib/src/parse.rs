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
    Declare(String,Option<Expression>),
    Expression(Expression)
}

#[derive(Debug)]
pub enum Expression {
    Assign(String,Box<Expression>),
    LogOrExpression(LogOrExpression)
}

#[derive(Debug)]
pub enum LogOrExpression {
    Binary(BinOp, Box<LogOrExpression>, Box<LogAndExpression>),
    LogAndExpression(LogAndExpression),
}

#[derive(Debug)]
pub enum LogAndExpression {
    Binary(BinOp, Box<LogAndExpression>, Box<EqualityExpression>),
    EqualityExpression(EqualityExpression)
}

#[derive(Debug)]
pub enum EqualityExpression {
    Binary(BinOp, Box<EqualityExpression>, Box<RelationalExpression>),
    RelationalExpression(RelationalExpression),
}

#[derive(Debug)]
pub enum RelationalExpression {
    Binary(BinOp, Box<RelationalExpression>, Box<AdditiveExpression>),
    AdditiveExpression(AdditiveExpression),
}

#[derive(Debug)]
pub enum AdditiveExpression {
    Binary(BinOp, Box<AdditiveExpression>, Box<Term>),
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
    BoolOr,
    BoolAnd,
    RShift,
    LShift,
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
}

#[derive(Debug)]
pub enum Factor {
    Unary(UnOp, Box<Factor>),
    Wrapped(Box<Expression>),
    Int(String),
    Var(String)
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
    let stmt;
    match tokens.pop() {
        Some(Token::KWReturn) => {
            let (exp, mut toks) = parse_exp(tokens);
            tokens = toks;
            stmt = Statement::Return(exp)
        }
        Some(Token::KWInt) => {
            match tokens.pop() {
                Some(Token::Identifier(var)) => {
                    let next_tok = tokens.pop();
                    match next_tok{
                        Some(Token::Eq) => {
                            let (exp, mut toks) = parse_exp(tokens);
                            tokens = toks;
                            stmt = Statement::Declare(var, Some(exp))
                        }
                        _ =>  {
                            tokens.push(next_tok.expect("must have").clone());
                           stmt = Statement::Declare(var, None) 
                        }
                    }
                }
                _ => panic!("Expected Identifier")
            }
        }
        Some(Token::SemiColon) => {
            tokens.push(Token::SemiColon);
            stmt = Statement::NoStmt;
        }
        Some(x) => {
            tokens.push(x);
            let (expr, mut toks) = parse_exp(tokens);
            tokens = toks;
            stmt = Statement::Expression(expr);
        }
        None => {
            panic!("Expected keyword 'return'");
        }
    }
    
    if let Some(Token::SemiColon) = tokens.pop() {
    } else {
        panic!("Expected semicolon to end statement");
    }
    return (stmt, tokens)
}

pub fn parse_exp(mut tokens: Vec<lex::Token>) -> (Expression, Vec<lex::Token>){
    match tokens.pop() {
        Some(Token::Identifier(x)) => {
            let next = tokens.pop();
            if let Some(Token::Eq) = next {
                let(expr, mut toks) = parse_exp(tokens);
                return (Expression::Assign(x, Box::new(expr)), toks);
            } else {
               tokens.push(next.expect("Must have token, we couldn't be here without one"));
               tokens.push(Token::Identifier(x));
               let (expr, mut toks) = parse_logorexp(tokens);
               (Expression::LogOrExpression(expr), toks)
            }
        }
        None => { panic!("Reached end of tokens inside expresssion"); }
        Some(x) => {
            tokens.push(x);
            let (expr, mut toks) = parse_logorexp(tokens);
            (Expression::LogOrExpression(expr), toks)
        }
    }
}

pub fn parse_logorexp(tokens: Vec<lex::Token>) -> (LogOrExpression, Vec<lex::Token>) {
    let (tm, mut tokens) = parse_logandexp(tokens);
    let mut term = LogOrExpression::LogAndExpression(tm);
    let mut next = tokens
        .pop()
        .expect("Unexpected end of file in binary expression");
    while next == Token::BoolOr {
        tokens.push(next.clone());
        let (op, toks) = parse_op(tokens);
        let (next_term, toks) = parse_logandexp(toks);
        tokens = toks;
        term = LogOrExpression::Binary(op, Box::new(term), Box::new(next_term));
        next = tokens
            .pop()
            .expect("Unexpected end of file when parsing expression");
    }
    tokens.push(next);
    (term, tokens)
}

pub fn parse_logandexp(tokens: Vec<lex::Token>) -> (LogAndExpression, Vec<lex::Token>) {
    let (eqe, mut tokens) = parse_equality_expression(tokens);
    let mut term = LogAndExpression::EqualityExpression(eqe);
    let mut next = tokens
        .pop()
        .expect("Unexpected end of file in binary expression");
    while next == Token::BoolAnd {
        tokens.push(next.clone());
        let (op, toks) = parse_op(tokens);
        let (next_term, toks) = parse_equality_expression(toks);
        tokens = toks;
        term = LogAndExpression::Binary(op, Box::new(term), Box::new(next_term));
        next = tokens
            .pop()
            .expect("Unexpected end of file when parsing expression");
    }
    tokens.push(next);
    (term, tokens)
}

pub fn parse_equality_expression(tokens: Vec<lex::Token>) -> (EqualityExpression, Vec<lex::Token>) {
    let (relexp, mut tokens) = parse_relational_expression(tokens);
    let mut term = EqualityExpression::RelationalExpression(relexp);
    let mut next = tokens
        .pop()
        .expect("Unexpected end of file in binary expression");
    while next == Token::NotEq || next == Token::EqEq {
        tokens.push(next.clone());
        let (op, toks) = parse_op(tokens);
        let (next_term, toks) = parse_relational_expression(toks);
        tokens = toks;
        term = EqualityExpression::Binary(op, Box::new(term), Box::new(next_term));
        next = tokens
            .pop()
            .expect("Unexpected end of file when parsing expression");
    }
    tokens.push(next);
    (term, tokens)
}

pub fn parse_relational_expression(tokens: Vec<lex::Token>) -> (RelationalExpression, Vec<lex::Token>) {
    let (tm, mut tokens) = parse_additive_expression(tokens);
    let mut term = RelationalExpression::AdditiveExpression(tm);
    let mut next = tokens
        .pop()
        .expect("Unexpected end of file in binary expression");
    while next == Token::Lt || next == Token::LtE || next == Token::Gt || next == Token::GtE {
        tokens.push(next.clone());
        let (op, toks) = parse_op(tokens);
        let (next_term, toks) = parse_additive_expression(toks);
        tokens = toks;
        term = RelationalExpression::Binary(op, Box::new(term), Box::new(next_term));
        next = tokens
            .pop()
            .expect("Unexpected end of file when parsing expression");
    }
    tokens.push(next);
    (term, tokens)
}

pub fn parse_additive_expression(tokens: Vec<lex::Token>) -> (AdditiveExpression, Vec<lex::Token>) {
    let (tm, mut tokens) = parse_term(tokens);
    let mut term = AdditiveExpression::Term(tm);
    let mut next = tokens
        .pop()
        .expect("Unexpected end of file in binary expression");
    while next == Token::Plus || next == Token::Minus {
        tokens.push(next.clone());
        let (op, toks) = parse_op(tokens);
        let (next_term, toks) = parse_term(toks);
        tokens = toks;
        term = AdditiveExpression::Binary(op, Box::new(term), Box::new(next_term));
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
    let next = tokens.pop();
    match next {
        Some(Token::Identifier(s)) => {
            (Factor::Var(s), tokens)
        },
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
        Token::Lt => (BinOp::Lt, tokens),
        Token::Gt => (BinOp::Gt, tokens),
        Token::GtE => (BinOp::GtE, tokens),
        Token::LtE => (BinOp::LtE, tokens),
        Token::EqEq => (BinOp::Eq, tokens),
        Token::NotEq => (BinOp::NotEq, tokens),
        Token::BoolOr => (BinOp::BoolOr, tokens),
        Token::BoolAnd => (BinOp::BoolAnd, tokens),
        _ => panic!("unexpected token encountered, operator expected"),
    }
}
