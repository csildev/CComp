use lex;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Function {
    name: Option<String>,
    stmts: Vec<BlockItem>,
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

    pub fn get_stmts(&self) -> &Vec<BlockItem> {
        &self.stmts
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    For(
        Option<Expression>,
        Box<Expression>,
        Option<Box<Expression>>,
        Box<Statement>,
    ),
    ForDecl(
        Box<BlockItem>,
        Box<Expression>,
        Option<Box<Expression>>,
        Box<Statement>,
    ),
    While(Expression, Box<Statement>),
    Do(Box<Statement>, Expression),
    Break,
    Continue,
    Expression(Option<Expression>),
    Compound(Vec<BlockItem>),
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(String, Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Assign(String, Box<Expression>),
    ConditionalExpression(ConditionalExpression),
}

#[derive(Debug, Clone)]
pub enum ConditionalExpression {
    LogicalOrExpression(LogOrExpression),
    Conditional(Box<LogOrExpression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum LogOrExpression {
    Binary(BinOp, Box<LogOrExpression>, Box<LogAndExpression>),
    LogAndExpression(LogAndExpression),
}

#[derive(Debug, Clone)]
pub enum LogAndExpression {
    Binary(BinOp, Box<LogAndExpression>, Box<EqualityExpression>),
    EqualityExpression(EqualityExpression),
}

#[derive(Debug, Clone)]
pub enum EqualityExpression {
    Binary(BinOp, Box<EqualityExpression>, Box<RelationalExpression>),
    RelationalExpression(RelationalExpression),
}

#[derive(Debug, Clone)]
pub enum RelationalExpression {
    Binary(BinOp, Box<RelationalExpression>, Box<AdditiveExpression>),
    AdditiveExpression(AdditiveExpression),
}

#[derive(Debug, Clone)]
pub enum AdditiveExpression {
    Binary(BinOp, Box<AdditiveExpression>, Box<Term>),
    Term(Term),
}

#[derive(Debug, Clone)]
pub enum Term {
    Binary(BinOp, Box<Term>, Box<Factor>),
    Factor(Factor),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Factor {
    Unary(UnOp, Box<Factor>),
    Wrapped(Box<Expression>),
    Int(String),
    Var(String),
}

#[derive(Debug, Clone)]
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
                let (stmt, mut toks) = parse_blockitem(tokens);
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

pub fn parse_blockitem(mut tokens: Vec<lex::Token>) -> (BlockItem, Vec<lex::Token>) {
    let stmt;
    match tokens.pop() {
        Some(Token::KWInt) => match tokens.pop() {
            Some(Token::Identifier(var)) => {
                let next_tok = tokens.pop();
                match next_tok {
                    Some(Token::Eq) => {
                        let (exp, mut toks) = parse_exp(tokens);
                        tokens = toks;
                        stmt = BlockItem::Declaration(var, Some(exp));
                        (stmt, tokens)
                    }
                    _ => {
                        tokens.push(next_tok.expect("must have").clone());
                        stmt = BlockItem::Declaration(var, None);
                        (stmt, tokens)
                    }
                }
            }
            _ => panic!("Expected Identifier"),
        },
        Some(x) => {
            tokens.push(x);
            let (bkitem, toks) = parse_stmt(tokens);
            (BlockItem::Statement(bkitem), toks)
        }
        _ => {
            panic!("Error, must have token");
        }
    }
}

pub fn parse_stmt(mut tokens: Vec<lex::Token>) -> (Statement, Vec<lex::Token>) {
    let stmt;
    match tokens.pop() {
        Some(Token::KWIf) => {
            if let Some(Token::LParen) = tokens.pop() {
            } else {
                panic!("Expected parenthesis to begin condition");
            }
            let (rel_exp, mut toks) = parse_exp(tokens);
            tokens = toks;
            if let Some(Token::RParen) = tokens.pop() {
            } else {
                panic!("Expected parenthesis to end condition");
            }
            let (smt, mut toks) = parse_stmt(tokens);
            tokens = toks;
            let else_stmt = match tokens.pop() {
                Some(Token::KWElse) => None,
                Some(x) => {
                    tokens.push(x);
                    None
                }
                None => {
                    panic!("Expected token found EOF");
                }
            };
            stmt = Statement::If(rel_exp, Box::new(smt), else_stmt);
            return (stmt, tokens);
        }
        Some(Token::KWReturn) => {
            let (exp, mut toks) = parse_exp(tokens);
            tokens = toks;
            stmt = Statement::Return(exp)
        }
        Some(Token::SemiColon) => {
            tokens.push(Token::SemiColon);
            stmt = Statement::Expression(None);
        }
        Some(Token::LBracket) => {
            let mut blockitems = Vec::new();
            loop {
                match tokens.pop() {
                    Some(Token::RBracket) => break,
                    Some(x) => {
                        tokens.push(x);
                        let (mut blkitem, mut toks) = parse_blockitem(tokens);
                        tokens = toks;
                        blockitems.push(blkitem);
                    }
                    None => panic!("Expected end of block, not end of file"),
                }
            }
            return (Statement::Compound(blockitems), tokens);
        }
        Some(Token::KWFor) => {
            if let Some(Token::LParen) = tokens.pop() {
            } else {
                panic!("Expected open parenthesis to begin for")
            }
            let nt = tokens.pop();
            if let Some(Token::KWInt) = nt {
                tokens.push(nt.expect("Must have token"));
                let (ex, toks) = parse_blockitem(tokens);
                tokens = toks;
                let exp = ex;
                if let Some(Token::SemiColon) = tokens.pop() {
                } else {
                    panic!("Expected semicolon between parses")
                }
                let (s_exp, toks) = parse_exp(tokens);
                tokens = toks;
                if let Some(Token::SemiColon) = tokens.pop() {
                } else {
                    panic!("expected semicolon")
                }
                let (t_exp, toks) = parse_exp(tokens);
                tokens = toks;
                if let Some(Token::RParen) = tokens.pop() {
                } else {
                    panic!("Expected closing parenthesis");
                }
                let (stmt, toks) = parse_stmt(tokens);
                tokens = toks;
                return (
                    Statement::ForDecl(
                        Box::new(exp),
                        Box::new(s_exp),
                        Some(Box::new(t_exp)),
                        Box::new(stmt),
                    ),
                    tokens,
                );

            } else {
                tokens.push(nt.expect("Must have token"));
                let (ex, toks) = parse_exp(tokens);
                tokens = toks;
                let exp = ex;
                if let Some(Token::SemiColon) = tokens.pop() {
                } else {
                    panic!("Expected semicolon between parses")
                }
                let (s_exp, toks) = parse_exp(tokens);
                tokens = toks;
                if let Some(Token::SemiColon) = tokens.pop() {
                } else {
                    panic!("expected semicolon")
                }
                let (t_exp, toks) = parse_exp(tokens);
                tokens = toks;
                if let Some(Token::RParen) = tokens.pop() {
                } else {
                    panic!("Expected closing parenthesis");
                }
                let (stmt, toks) = parse_stmt(tokens);
                tokens = toks;
                return (
                    Statement::For(
                        Some(exp),
                        Box::new(s_exp),
                        Some(Box::new(t_exp)),
                        Box::new(stmt),
                    ),
                    tokens,
                );
            }
        }
        Some(x) => {
            tokens.push(x);
            let (expr, mut toks) = parse_exp(tokens);
            tokens = toks;
            stmt = Statement::Expression(Some(expr));
        }
        None => {
            panic!("Expected keyword 'return'");
        }
    }

    if let Some(Token::SemiColon) = tokens.pop() {
    } else {
        panic!("Expected semicolon to end statement");
    }
    return (stmt, tokens);
}

pub fn parse_exp(mut tokens: Vec<lex::Token>) -> (Expression, Vec<lex::Token>) {
    match tokens.pop() {
        Some(Token::Identifier(x)) => {
            let next = tokens.pop();
            if let Some(Token::Eq) = next {
                let (expr, mut toks) = parse_exp(tokens);
                return (Expression::Assign(x, Box::new(expr)), toks);
            } else {
                tokens.push(next.expect("Must have token, we couldn't be here without one"));
                tokens.push(Token::Identifier(x));
                let (expr, mut toks) = parse_conditionalexp(tokens);
                (Expression::ConditionalExpression(expr), toks)
            }
        }
        None => {
            panic!("Reached end of tokens inside expresssion");
        }
        Some(x) => {
            tokens.push(x);
            let (expr, mut toks) = parse_conditionalexp(tokens);
            (Expression::ConditionalExpression(expr), toks)
        }
    }
}

pub fn parse_conditionalexp(
    mut tokens: Vec<lex::Token>,
) -> (ConditionalExpression, Vec<lex::Token>) {
    match tokens.pop() {
        Some(x) => {
            tokens.push(x);
            let (l_exp, mut toks) = parse_logorexp(tokens);
            tokens = toks;
            match tokens.pop() {
                Some(Token::Question) => {
                    let (s_exp, mut toks) = parse_exp(tokens);
                    tokens = toks;
                    if let Some(Token::FullColon) = tokens.pop() {
                    } else {
                        panic!("Expected token, found eof");
                    }
                    let (e_exp, mut toks) = parse_exp(tokens);
                    tokens = toks;
                    (
                        ConditionalExpression::Conditional(
                            Box::new(l_exp),
                            Box::new(s_exp),
                            Box::new(e_exp),
                        ),
                        tokens,
                    )
                }
                Some(x) => {
                    tokens.push(x);
                    (ConditionalExpression::LogicalOrExpression(l_exp), tokens)
                }
                None => {
                    panic!("unexpected end of file");
                }
            }
        }
        None => panic!("Unexpected end of file when matching token"),
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

pub fn parse_relational_expression(
    tokens: Vec<lex::Token>,
) -> (RelationalExpression, Vec<lex::Token>) {
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
        Some(Token::Identifier(s)) => (Factor::Var(s), tokens),
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
            if let Some(Token::RParen) = toks.pop() {
            } else {
                panic!("Expected closing parenthesis");
            }
            (Factor::Wrapped(Box::new(expr)), toks)
        }
        Some(_) => {
            panic!("Unknown token");
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
