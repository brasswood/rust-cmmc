use std::fs;
use std::io::Write;

use crate::ir::*;
use crate::name::symbol::SymbolType;

pub fn write_x64(program: &IRProgram, outfile: &mut fs::File) {
    let mut s = String::with_capacity(13);
    program.x64_codegen(&mut s, 0);
    write!(outfile, "{}", s).unwrap();
}

trait X64Codegen {
    fn x64_codegen(&self, out: &mut String, current_offset: usize);
}

impl<'a> X64Codegen for IRProgram<'a> {
    fn x64_codegen(&self, out: &mut String, current_offset: usize) {
        out.push_str(".globl main\n.data\n");
        for global in &self.globals {
            let val_str = match global.symbol.typ {
                SymbolType::Int | SymbolType::Ptr(_) | SymbolType::Str => ".quad 0",
                SymbolType::Short | SymbolType::Bool => ".byte 0",
                _ => unreachable!(),
            };
            out.push_str(&format!("gbl_{}: {}\n", global.symbol.name, val_str));
        }
        for &StringOperandStruct { id, value } in &self.strings {
            out.push_str(&format!("str_{}: .asciz {}", id, value));
        }
        out.push_str("\n.text\n");
        for proc in &self.procedures {
            proc.x64_codegen(out, current_offset);
        }
    }
}

impl<'a> X64Codegen for IRProcedure<'a> {
    fn x64_codegen(&self, out: &mut String, current_offset: usize) {
        let current_offset = 0;

    }
}