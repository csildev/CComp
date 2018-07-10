use gencode;

pub fn translate_instruction(i: &gencode::Instruction) -> String {
    match i {
        gencode::Instruction::If(r1, s) => format!(
            "cmp {}, 0\nje {}",
            translate_register(r1),
            s),
        gencode::Instruction::MovSR(r1, r2) => format!(
            "mov {}, [rbp - {}]",
            translate_register(r2),r1),
        gencode::Instruction::MovRS(r1, r2) => format!(
            "mov [rbp - {}], {}",
            r2, translate_register(r1),
            ),
        gencode::Instruction::MovRR(r1, r2) => format!(
            "mov {}, {}",
            translate_register(r1),
            translate_register(r2),
            ),
        gencode::Instruction::EqRR(r1, r2) => format!(
            "cmp {}, {}\n mov {}, 0\n sete {}",
            translate_register(r1),
            translate_register(r2),
            translate_register(r1),
            translate_small_register(r1)),
        gencode::Instruction::LtEqRR(r1, r2) => format!(
            "cmp {}, {}\n mov {}, 0\n setle {}",
            translate_register(r1),
            translate_register(r2),
            translate_register(r1),
            translate_small_register(r1)),
        gencode::Instruction::GtEqRR(r1, r2) => format!(
            "cmp {}, {}\n mov {}, 0\n setge {}",
            translate_register(r1),
            translate_register(r2),
            translate_register(r1),
            translate_small_register(r1)),
        gencode::Instruction::GtRR(r1, r2) => format!(
            "cmp {}, {}\n mov {}, 0\n setg {}",
            translate_register(r2),
            translate_register(r1),
            translate_register(r1),
            translate_small_register(r1)),
        gencode::Instruction::LtRR(r1, r2) => format!(
            "cmp {}, {}\n mov {}, 0\n setl {}",
            translate_register(r2),
            translate_register(r1),
            translate_register(r1),
            translate_small_register(r1)),
        gencode::Instruction::DivRR(r1, r2) => format!(
            "push {}\npush {}\nmov rdx, 0\npop {}\npop rax\nidiv {}",
            translate_register(r1),
            translate_register(r2),
            translate_register(r2),
            translate_small_register(r2)),
        gencode::Instruction::MulRR(r1, r2) => format!(
            "imul {}, {}", translate_register(r1), translate_register(r2)),
        gencode::Instruction::PopR(r) => format!(
            "pop {}", translate_register(r)),
        gencode::Instruction::AddRR(r1, r2) => format!(
            "add {}, {}", translate_register(r1), translate_register(r2)),
        gencode::Instruction::SubRR(r1, r2) => format!(
            "sub {}, {}", translate_register(r1), translate_register(r2)),
        gencode::Instruction::LogNeg(r) => format!(
            "cmp 0 {}\nmov {}, 0\n,sete {}",
            translate_register(r),
            translate_register(r),
            translate_register(r)
        ),
        gencode::Instruction::Not(r) => format!("not {}", translate_register(r)),
        gencode::Instruction::Neg(r) => format!("neg {}", translate_register(r)),
        gencode::Instruction::Ret => "ret".to_string(),
        gencode::Instruction::Call(s) => format!("call {}", s),
        gencode::Instruction::Jmp(s) => format!("jmp {}", s),
        gencode::Instruction::Label(s) => format!("{}:", s),
        gencode::Instruction::ExtLabel(s) => format!("global {}\n{}:", s, s),
        gencode::Instruction::PushR(r) => format!("push {}", translate_register(r)),
        gencode::Instruction::MovRC(r, s) => format!("mov {},{}", translate_register(r), s),
        gencode::Instruction::BoolOr(r1, r2) => format!("or {}, {}\nmov {}, 0\nsetnz {}", 
            translate_register(r1),
            translate_register(r2),
            translate_register(r1),
            translate_small_register(r1)),
        gencode::Instruction::BoolAnd(r1, r2) => format!("cmp {}, 0\nsetne {}\ncmp {},0\nsetne {}\nand {}, {}",
            translate_register(r2),
            translate_small_register(r2),
            translate_register(r1),
            translate_small_register(r1),
            translate_register(r1),
            translate_register(r2)),
            _ => panic!("unknown register"),
        
    }
}

pub fn translate_register(r: &gencode::Register) -> String {
    let r = match r {
        gencode::Register::EAX => "rax",
        gencode::Register::EBX => "rbx",
        gencode::Register::ECX => "rcx",
        gencode::Register::EDX => "rdx",
        gencode::Register::ESP => "rsp",
        gencode::Register::EBP => "rbp",
    };
    r.to_string()
}
pub fn translate_small_register(r: &gencode::Register) -> String {
    let r = match r {
        gencode::Register::EAX => "al",
        gencode::Register::EBX => "bl",
        gencode::Register::ECX => "cl",
        gencode::Register::EDX => "dl",
        _ => panic!("non-8bit register")
    };
    r.to_string()
}
