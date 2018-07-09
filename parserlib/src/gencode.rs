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
            instrs.push(Instruction::Ret);
        }
    }
    instrs
}

pub fn gencode_expr(expr: &parse::Expression) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::Expression::Int(x) => {
            instrs.push(Instruction::MovRC(Register::EAX, x.to_string()));
        }
        parse::Expression::Complement(x) => {
            instrs.extend(gencode_expr(x));
            instrs.push(Instruction::Not(Register::EAX));
        }
        parse::Expression::LogNegate(x) => {
            instrs.extend(gencode_expr(x));
            instrs.push(Instruction::LogNeg(Register::EAX));
        }
        parse::Expression::Negate(x) => {
            instrs.extend(gencode_expr(x));
            instrs.push(Instruction::Neg(Register::EAX));
        }
    }
    instrs
}
