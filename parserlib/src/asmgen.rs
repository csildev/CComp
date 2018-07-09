use gencode;

pub fn translate_instruction(i: &gencode::Instruction) -> String {
    match i {
        gencode::Instruction::LogNeg(r) => format!("cmp 0 {}\nmov {}, 0\n,sete {}", translate_register(r), translate_register(r), translate_register(r)),
        gencode::Instruction::Not(r) => format!("not {}", translate_register(r)),
        gencode::Instruction::Neg(r) => format!("neg {}", translate_register(r)),
        gencode::Instruction::Ret => "ret".to_string(),
        gencode::Instruction::Call(s) => format!("call {}", s),
        gencode::Instruction::Jmp(s) => format!("jmp {}", s),
        gencode::Instruction::Label(s) => format!("{}:", s),
        gencode::Instruction::ExtLabel(s) => format!("global {}\n{}:", s, s),
        gencode::Instruction::PushR(r) => format!("push {}", translate_register(r)),
        gencode::Instruction::MovRC(r, s) => format!("mov {},{}",translate_register(r),s),
    }
}

pub fn translate_register(r: &gencode::Register) -> String {
        let r = match r {
                gencode::Register::EAX => "rax",
                gencode::Register::EBX => "rbx",
                gencode::Register::ECX => "rcx",
                gencode::Register::EDX => "rdx",
            };
        r.to_string()
}
