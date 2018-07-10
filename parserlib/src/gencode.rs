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
    MovRR(Register, Register),
    MovMR(Register, u64),
    MovRS(Register, u64),
    MovSR(u64, Register),
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
    ESP,
    EBP,
}

use std::collections::HashMap;
pub fn gencode_program(program: parse::Program) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();

    for func in program.get_funcs().iter() {
        instrs.extend(gencode_func(func));
    }
    instrs
}

pub fn gencode_func(func: &parse::Function) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    let mut vmap = HashMap::new();
    let mut sindex = 0;
    instrs.push(Instruction::ExtLabel(func.get_name()));
    instrs.push(Instruction::PushR(Register::EBP));
    instrs.push(Instruction::MovRR(Register::EBP, Register::ESP));
    for stmt in func.get_stmts().iter() {
        let (stmt, map, idx) = gencode_stmt(stmt, vmap, sindex);
        instrs.extend(stmt);
        vmap = map;
        sindex = idx;
    }
   instrs
}

pub fn gencode_stmt(stmt: &parse::Statement, mut vmap: HashMap<String,u64>, stack_index: u64) -> (Vec<Instruction>, HashMap<String, u64>, u64) {
    let mut instrs = Vec::<Instruction>::new();
    match stmt {
        parse::Statement::NoStmt => {}
        parse::Statement::Return(expr) => {
            instrs.extend(gencode_expr(expr,&vmap));
            instrs.push(Instruction::PopR(Register::EAX));
            instrs.push(Instruction::MovRR(Register::ESP, Register::EBP));
            instrs.push(Instruction::PopR(Register::EBP));
            instrs.push(Instruction::Ret);
        }
        parse::Statement::Declare(x, Some(y)) => {
            if vmap.contains_key(x){
                panic!("variable was redefined");
            }
            instrs.extend(gencode_expr(y,&vmap));
            vmap.insert(x.to_string(),stack_index);
            instrs.push(Instruction::PopR(Register::EAX));
            instrs.push(Instruction::PushR(Register::EAX));
        }
        parse::Statement::Declare(x, None) => {
            if vmap.contains_key(x){
                panic!("variable redefined");
            }
            vmap.insert(x.to_string(), stack_index);
            instrs.push(Instruction::MovRC(Register::EAX,"0".to_string()));
            instrs.push(Instruction::MovRS(Register::EAX, stack_index));
        }
        parse::Statement::Expression(x) => {
            instrs.extend(gencode_expr(x,&vmap));
        }
    }
    (instrs, vmap, stack_index+4)
}

pub fn gencode_expr(expr: &parse::Expression, vmap: &HashMap<String, u64>) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::Expression::Assign(x,y) => {
            instrs.extend(gencode_expr(y, vmap));
            let var_offset = vmap.get(x).expect("Undefined variable");
            instrs.push(Instruction::PopR(Register::EAX));
            instrs.push(Instruction::MovRS(Register::EAX,var_offset.clone()));
        }
        parse::Expression::LogOrExpression(x) => {
            instrs.extend(gencode_logorexpr(x, vmap));
        }
    }
    instrs
}

pub fn gencode_logorexpr(expr: &parse::LogOrExpression, vmap: &HashMap<String, u64>) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::LogOrExpression::Binary(x, y, z) => {
            instrs.extend(gencode_logorexpr(y, vmap));
            instrs.extend(gencode_logandexp(z, vmap));
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
            instrs.extend(gencode_logandexp(x, vmap));
        }
    }
    instrs
}

pub fn gencode_logandexp(expr: &parse::LogAndExpression, vmap: &HashMap<String, u64>) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::LogAndExpression::Binary(x, y, z) => {
            instrs.extend(gencode_logandexp(y, vmap));
            instrs.extend(gencode_equalityexp(z, vmap));
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
            instrs.extend(gencode_equalityexp(x, vmap));
        }
    }
    instrs
}


pub fn gencode_equalityexp(expr: &parse::EqualityExpression, vmap: &HashMap<String, u64>) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::EqualityExpression::Binary(x, y, z) => {
            instrs.extend(gencode_equalityexp(y, vmap));
            instrs.extend(gencode_relationalexp(z, vmap));
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
            instrs.extend(gencode_relationalexp(x, vmap));
        }
    }
    instrs
}


pub fn gencode_relationalexp(expr: &parse::RelationalExpression, vmap : &HashMap<String, u64>) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::RelationalExpression::Binary(x, y, z) => {
            instrs.extend(gencode_relationalexp(y, vmap));
            instrs.extend(gencode_additiveexp(z, vmap));
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
            instrs.extend(gencode_additiveexp(x, vmap));
        }
    }
    instrs
}


pub fn gencode_additiveexp(expr: &parse::AdditiveExpression, vmap: &HashMap<String, u64>) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::AdditiveExpression::Binary(x, y, z) => {
            instrs.extend(gencode_additiveexp(y, vmap));
            instrs.extend(gencode_term(z, vmap));
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
            instrs.extend(gencode_term(x, vmap));
        }
    }
    instrs
}


pub fn gencode_term(term: &parse::Term, vmap: &HashMap<String, u64>) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match term {
        parse::Term::Binary(x, y, z) => {
            instrs.extend(gencode_term(y, vmap));
            instrs.extend(gencode_factor(z, vmap));
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
            instrs.extend(gencode_factor(x, vmap));
        }
    }
    instrs
}

pub fn gencode_factor(factor: &parse::Factor, vmap: &HashMap<String, u64>) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match factor {
        parse::Factor::Unary(x, y) => {
            instrs.extend(gencode_factor(y, vmap));
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
            instrs.extend(gencode_expr(x, vmap));
        }
        parse::Factor::Int(x) => {
            instrs.push(Instruction::MovRC(Register::EAX, x.clone()));
            instrs.push(Instruction::PushR(Register::EAX));
        }
        parse::Factor::Var(x) => {
            let offset = vmap.get(x).expect("Variable not defined");
            instrs.push(Instruction::MovSR(offset.clone(),Register::EAX));
            instrs.push(Instruction::PushR(Register::EAX));
        }
        _ => {
            panic!("Somehow we got somewhere we shouldnt")
        }
    }
    instrs
}
