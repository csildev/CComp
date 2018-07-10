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
    AddRR(Register, Register),
    AddRC(Register, u64),
    SubRR(Register, Register),
    MulRR(Register, Register),
    DivRR(Register, Register),
    PopR(Register),
    LtEqRR(Register, Register),
    GtEqRR(Register, Register),
    GtRR(Register, Register),
    LtRR(Register, Register),
    EqRR(Register, Register),
    BoolOr(Register, Register),
    BoolAnd(Register, Register),
    If(Register, String),
    NIF(Register, String),
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
    let vmap = HashMap::new();
    let sindex = 0;
    instrs.push(Instruction::ExtLabel(func.get_name()));
    instrs.push(Instruction::PushR(Register::EBP));
    instrs.push(Instruction::MovRR(Register::EBP, Register::ESP));
    instrs.extend(gencode_block(func.get_stmts(), &vmap, sindex));
    instrs
}

pub fn gencode_block(
    block: &Vec<parse::BlockItem>,
    vmap: &HashMap<String, u64>,
    mut stack_index: u64,
) -> Vec<Instruction> {
    let mut vmap = vmap.clone();
    let mut instrs = Vec::<Instruction>::new();
    let mut current_scope = HashMap::new();
    for blockitem in block.iter() {
        match blockitem {
            parse::BlockItem::Declaration(x, y) => {
                let y = y.clone();
                let decl = parse::BlockItem::Declaration(x.clone(), y);
                let (intr, vmp, stack_ind, current_scop) =
                    gencode_declaration(&(decl.clone()), vmap, stack_index, current_scope);
                instrs.extend(intr);
                vmap = vmp;
                stack_index = stack_ind;
                current_scope = current_scop;
            }
            parse::BlockItem::Statement(x) => {
                instrs.extend(gencode_stmt(x, &vmap, stack_index));
            }
        }
    }
    instrs.push(Instruction::AddRC(Register::ESP, stack_index));
    instrs
}

pub fn gencode_declaration(
    declaration: &parse::BlockItem,
    mut vmap: HashMap<String, u64>,
    mut stack_index: u64,
    mut current_scope: HashMap<String, u64>,
) -> (
    Vec<Instruction>,
    HashMap<String, u64>,
    u64,
    HashMap<String, u64>,
) {
    let mut instrs = Vec::<Instruction>::new();
    if let parse::BlockItem::Declaration(x, y) = declaration {
        if current_scope.contains_key(x) {
            panic!("variable cannot be defined twice");
        }
        stack_index += 8;
        current_scope.insert(x.clone(), stack_index);
        vmap.insert(x.clone(), stack_index);
        match y {
            Some(y) => {
                instrs.extend(gencode_expr(y, &vmap));
                instrs.push(Instruction::MovRS(Register::EAX, stack_index));
            }
            None => {
                instrs.push(Instruction::MovRC(Register::EAX, "0".to_string()));
                instrs.push(Instruction::MovRS(Register::EAX, stack_index));
            }
        }
    } else {
        panic!("Unexpected statement")
    }
    (instrs, vmap, stack_index, current_scope)
}

pub fn gencode_stmt(
    stmt: &parse::Statement,
    vmap: &HashMap<String, u64>,
    sindex: u64,
) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match stmt {
        parse::Statement::Return(expr) => {
            instrs.extend(gencode_expr(expr, &vmap));
            instrs.push(Instruction::PopR(Register::EAX));
            instrs.push(Instruction::MovRR(Register::ESP, Register::EBP));
            instrs.push(Instruction::PopR(Register::EBP));
            instrs.push(Instruction::Ret);
        }
        parse::Statement::If(x, y, Some(z)) => {
            instrs.extend(gencode_expr(x, &vmap));
            let iflabel = get_unique_label(".iflocallabel".to_string());
            instrs.push(Instruction::If(Register::EAX, iflabel.clone()));
            instrs.extend(gencode_stmt(y, &vmap, sindex));
            instrs.push(Instruction::Label(iflabel));
            instrs.extend(gencode_stmt(z, &vmap, sindex));
        }
        parse::Statement::If(x, y, None) => {
            instrs.extend(gencode_expr(x, &vmap));
            let iflabel = get_unique_label("iflocallabel".to_string());
            instrs.push(Instruction::If(Register::EAX, iflabel.clone()));
            instrs.extend(gencode_stmt(y, &vmap, sindex));
            instrs.push(Instruction::Label(iflabel));
        }
        parse::Statement::Expression(Some(x)) => {
            instrs.extend(gencode_expr(x, &vmap));
        }
        parse::Statement::Expression(None) => {
        }
        parse::Statement::Compound(x) => {
            instrs.extend(gencode_block(x, &vmap, sindex));
            instrs.push(Instruction::AddRC(Register::ESP, sindex));
        }
        parse::Statement::For(Some(init),condition,Some(post),state) => {
            instrs.extend(gencode_expr(init, &vmap));            
            let two = get_unique_label("two".to_string());
            instrs.push(Instruction::Label(two.clone()));
            instrs.extend(gencode_expr(condition, &vmap));
            let end = get_unique_label("endlabel".to_string());
            instrs.push(Instruction::NIF(Register::EAX,end.clone())); 
            instrs.extend(gencode_stmt(state, &vmap, sindex));
            instrs.extend(gencode_expr(post, &vmap));
            instrs.push(Instruction::Jmp(two));
            instrs.push(Instruction::Label(end.clone()));
        }
        _ => {
        }
    }
    instrs
}

pub fn gencode_expr(expr: &parse::Expression, vmap: &HashMap<String, u64>) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::Expression::Assign(x, y) => {
            instrs.extend(gencode_expr(y, vmap));
            let var_offset = vmap.get(x).expect("Undefined variable");
            instrs.push(Instruction::PopR(Register::EAX));
            instrs.push(Instruction::MovRS(Register::EAX, var_offset.clone()));
        }
        parse::Expression::ConditionalExpression(x) => {
            instrs.extend(gencode_condexpr(x, vmap));
        }
    }
    instrs
}

pub fn gencode_condexpr(
    expr: &parse::ConditionalExpression,
    vmap: &HashMap<String, u64>,
) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::ConditionalExpression::LogicalOrExpression(x) => {
            instrs.extend(gencode_logorexpr(x, vmap));
        }
        parse::ConditionalExpression::Conditional(x, y, z) => {
            instrs.extend(gencode_logorexpr(x, vmap));
            let locallabel = get_unique_label(".localcond".to_string());
            let post_label = get_unique_label(".postcond".to_string());
            instrs.push(Instruction::If(Register::EAX, locallabel.clone()));
            instrs.extend(gencode_expr(y, vmap));
            instrs.push(Instruction::Jmp(post_label.clone()));
            instrs.push(Instruction::Label(locallabel));
            instrs.extend(gencode_expr(z, vmap));
            instrs.push(Instruction::Label(post_label));
        }
    }
    instrs
}

pub fn gencode_logorexpr(
    expr: &parse::LogOrExpression,
    vmap: &HashMap<String, u64>,
) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::LogOrExpression::Binary(x, y, z) => {
            instrs.extend(gencode_logorexpr(y, vmap));
            instrs.extend(gencode_logandexp(z, vmap));
            instrs.push(Instruction::PopR(Register::ECX));
            instrs.push(Instruction::PopR(Register::EAX));
            match x {
                parse::BinOp::BoolOr => {
                    instrs.push(Instruction::BoolOr(Register::EAX, Register::ECX));
                }
                _ => panic!("Expected orop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        }
        parse::LogOrExpression::LogAndExpression(x) => {
            instrs.extend(gencode_logandexp(x, vmap));
        }
    }
    instrs
}

pub fn gencode_logandexp(
    expr: &parse::LogAndExpression,
    vmap: &HashMap<String, u64>,
) -> Vec<Instruction> {
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
        }
        parse::LogAndExpression::EqualityExpression(x) => {
            instrs.extend(gencode_equalityexp(x, vmap));
        }
    }
    instrs
}

pub fn gencode_equalityexp(
    expr: &parse::EqualityExpression,
    vmap: &HashMap<String, u64>,
) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::EqualityExpression::Binary(x, y, z) => {
            instrs.extend(gencode_equalityexp(y, vmap));
            instrs.extend(gencode_relationalexp(z, vmap));
            instrs.push(Instruction::PopR(Register::ECX));
            instrs.push(Instruction::PopR(Register::EAX));
            match x {
                parse::BinOp::Eq => {
                    instrs.push(Instruction::EqRR(Register::EAX, Register::ECX));
                }
                _ => panic!("Expected relop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        }
        parse::EqualityExpression::RelationalExpression(x) => {
            instrs.extend(gencode_relationalexp(x, vmap));
        }
    }
    instrs
}

pub fn gencode_relationalexp(
    expr: &parse::RelationalExpression,
    vmap: &HashMap<String, u64>,
) -> Vec<Instruction> {
    let mut instrs = Vec::<Instruction>::new();
    match expr {
        parse::RelationalExpression::Binary(x, y, z) => {
            instrs.extend(gencode_relationalexp(y, vmap));
            instrs.extend(gencode_additiveexp(z, vmap));
            instrs.push(Instruction::PopR(Register::ECX));
            instrs.push(Instruction::PopR(Register::EAX));
            match x {
                parse::BinOp::LtE => {
                    instrs.push(Instruction::LtEqRR(Register::EAX, Register::ECX));
                }
                parse::BinOp::GtE => {
                    instrs.push(Instruction::GtEqRR(Register::EAX, Register::ECX));
                }
                parse::BinOp::Gt => instrs.push(Instruction::GtRR(Register::EAX, Register::ECX)),
                parse::BinOp::Lt => instrs.push(Instruction::LtRR(Register::EAX, Register::ECX)),

                _ => panic!("Expected relop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        }
        parse::RelationalExpression::AdditiveExpression(x) => {
            instrs.extend(gencode_additiveexp(x, vmap));
        }
    }
    instrs
}

pub fn gencode_additiveexp(
    expr: &parse::AdditiveExpression,
    vmap: &HashMap<String, u64>,
) -> Vec<Instruction> {
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
        }
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

            match x {
                parse::BinOp::Times => {
                    instrs.push(Instruction::MulRR(Register::EAX, Register::ECX));
                }
                parse::BinOp::Divide => {
                    instrs.push(Instruction::DivRR(Register::EAX, Register::ECX));
                }
                _ => panic!("Expected mulop not addop"),
            }
            instrs.push(Instruction::PushR(Register::EAX));
        }
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
            instrs.push(Instruction::MovSR(offset.clone(), Register::EAX));
            instrs.push(Instruction::PushR(Register::EAX));
        }
    }
    instrs
}

static mut LABELEND: u64 = 0;
pub fn get_unique_label(ins: String) -> String {
    unsafe {
        LABELEND = LABELEND + 1;
        let mut iflabel: String = ins;
        let second: String = LABELEND.to_string();
        iflabel.push_str(&second);
        iflabel
    }
}
