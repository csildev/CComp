use parse;

#[derive(Debug)]
pub enum Instruction {
    Ret,
    Call(String),
    Jmp(String),
    Label(String),
    ExtLabel(String),
    PushR(Register),
    MovRC(Register, String),
    Neg(Register),
    LogNeg(Register),
    Not(Register),
    AddRR(Register,Register),
    SubRR(Register,Register),
    MulRR(Register,Register),
    DivRR(Register,Register),
    PopR(Register),
    LtEqRR(Register,Register),
    GtEqRR(Register,Register),
    GtRR(Register,Register),
    LtRR(Register,Register),
    EqRR(Register,Register),
    BoolOr(Register,Register),
    BoolAnd(Register, Register),
}

#[derive(Debug)]
pub enum Register {
    EAX,
    EBX,
    ECX,
    EDX,
}

pub fn gencode_program(program: parse::Program) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();

    for func in program.get_funcs().iter() {
        instrs.extend(gencode_func(func));
    }
    instrs
}

pub fn gencode_func(func: &parse::Function) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    instrs.push(Instruction::ExtLabel(func.get_name()));
    for stmt in func.get_stmts().iter() {
        instrs.extend(gencode_stmt(stmt));
    }
    instrs
}

pub fn gencode_stmt(stmt: &parse::Statement) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match stmt {
        parse::Statement::NoStmt => {}
        parse::Statement::Return(expr) => {
            instrs.extend(gencode_expr(expr));
            instrs.push(Instruction::PopR(Register::EAX));
            instrs.push(Instruction::Ret);
        }
        _ => {}
    }
    instrs
}

pub fn gencode_expr(expr: &parse::Expression) -> Vec<Instruction> {
    panic!("Expected something")
}

pub fn gencode_logorexpr(expr: &parse::LogOrExpression) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::LogOrExpression::Binary(x, y, z) => {
            instrs.extend(gencode_logorexpr(y));
            instrs.extend(gencode_logandexp(z));
            instrs.push(Instruction::PopR(Register::ECX));
            instrs.push(Instruction::PopR(Register::EAX)); 
            match x {
                parse::BinOp::BoolOr=> {
                   instrs.push(Instruction::BoolOr(Register::EAX, Register::ECX));
                }
                _ => panic!("Expected orop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        },
        parse::LogOrExpression::LogAndExpression(x) => {
            instrs.extend(gencode_logandexp(x));
        }
    }
    instrs
}

pub fn gencode_logandexp(expr: &parse::LogAndExpression) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::LogAndExpression::Binary(x, y, z) => {
            instrs.extend(gencode_logandexp(y));
            instrs.extend(gencode_equalityexp(z));
            instrs.push(Instruction::PopR(Register::ECX));
            instrs.push(Instruction::PopR(Register::EAX)); 
            match x {
                parse::BinOp::BoolAnd => {
                    instrs.push(Instruction::BoolAnd(Register::EAX, Register::ECX));
                }
                _ => panic!("Expected andop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        },
        parse::LogAndExpression::EqualityExpression(x) => {
            instrs.extend(gencode_equalityexp(x));
        }
    }
    instrs
}


pub fn gencode_equalityexp(expr: &parse::EqualityExpression) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::EqualityExpression::Binary(x, y, z) => {
            instrs.extend(gencode_equalityexp(y));
            instrs.extend(gencode_relationalexp(z));
            instrs.push(Instruction::PopR(Register::ECX));
            instrs.push(Instruction::PopR(Register::EAX)); 
            match x {
                parse::BinOp::Eq=> {
                   instrs.push(Instruction::EqRR(Register::EAX, Register::ECX));
                }
                _ => panic!("Expected relop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        },
        parse::EqualityExpression::RelationalExpression(x) => {
            instrs.extend(gencode_relationalexp(x));
        }
    }
    instrs
}


pub fn gencode_relationalexp(expr: &parse::RelationalExpression) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::RelationalExpression::Binary(x, y, z) => {
            instrs.extend(gencode_relationalexp(y));
            instrs.extend(gencode_additiveexp(z));
            instrs.push(Instruction::PopR(Register::ECX));
            instrs.push(Instruction::PopR(Register::EAX)); 
            match x {
                parse::BinOp::LtE=> {
                   instrs.push(Instruction::LtEqRR(Register::EAX, Register::ECX));
                }
                parse::BinOp::GtE => {
                    instrs.push(Instruction::GtEqRR(Register::EAX, Register::ECX));
                }
                parse::BinOp::Gt => {
                    instrs.push(Instruction::GtRR(Register::EAX, Register::ECX))
                }
                parse::BinOp::Lt => {
                    instrs.push(Instruction::LtRR(Register::EAX, Register::ECX))
                }

                _ => panic!("Expected relop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        },
        parse::RelationalExpression::AdditiveExpression(x) => {
            instrs.extend(gencode_additiveexp(x));
        }
    }
    instrs
}


pub fn gencode_additiveexp(expr: &parse::AdditiveExpression) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::AdditiveExpression::Binary(x, y, z) => {
            instrs.extend(gencode_additiveexp(y));
            instrs.extend(gencode_term(z));
            instrs.push(Instruction::PopR(Register::ECX));
            instrs.push(Instruction::PopR(Register::EAX)); 
            match x {
                parse::BinOp::Plus => {
                   instrs.push(Instruction::AddRR(Register::EAX, Register::ECX));
                }
                parse::BinOp::Minus => {
                    instrs.push(Instruction::SubRR(Register::EAX, Register::ECX));
                }
                _ => panic!("Expected addop not mulop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        },
        parse::AdditiveExpression::Term(x) => {
            instrs.extend(gencode_term(x));
        }
    }
    instrs
}


pub fn gencode_term(term: &parse::Term) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match term {
        parse::Term::Binary(x, y, z) => {
            instrs.extend(gencode_term(y));
            instrs.extend(gencode_factor(z));
            instrs.push(Instruction::PopR(Register::ECX));
            instrs.push(Instruction::PopR(Register::EAX));

            match x{
                parse::BinOp::Times => {
                    instrs.push(Instruction::MulRR(Register::EAX, Register::ECX));
                }
                 parse::BinOp::Divide => {

                    instrs.push(Instruction::DivRR(Register::EAX, Register::ECX));
                }
                _ => panic!("Expected mulop not addop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        },
        parse::Term::Factor(x) => {
            instrs.extend(gencode_factor(x));
        }
    }
    instrs
}

pub fn gencode_factor(factor: &parse::Factor) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match factor {
        parse::Factor::Unary(x, y) => {
            instrs.extend(gencode_factor(y));
            instrs.push(Instruction::PopR(Register::EAX));
            match x {
                parse::UnOp::Complement => {
                    instrs.push(Instruction::Not(Register::EAX));
                }
                parse::UnOp::LogNegate => {
                    instrs.push(Instruction::LogNeg(Register::EAX));
                }
                parse::UnOp::Negate => {
                    instrs.push(Instruction::Neg(Register::EAX));
                }
            }
        }
        parse::Factor::Wrapped(x) => {
            instrs.extend(gencode_expr(x));
        }
        parse::Factor::Int(x) => {
            instrs.push(Instruction::MovRC(Register::EAX, x.clone()));
            instrs.push(Instruction::PushR(Register::EAX));
        }
        _ => {
            panic!("Somehow we got somewhere we shouldnt")
        }
    }
    instrs
}
