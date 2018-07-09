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
    }
    instrs
}

pub fn gencode_expr(expr: &parse::Expression) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::Expression::Binary(x, y, z) => {
            instrs.extend(gencode_expr(y));
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
        parse::Expression::Term(x) => {
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
    }
    instrs
}
