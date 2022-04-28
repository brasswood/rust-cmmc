use std::collections::HashMap;
use std::fs;
use std::io::Write;

use enum_dispatch::enum_dispatch;

use crate::ir::*;
use crate::name::symbol::{SymbolType, Symbol};

enum OperandScope {
    Global,
    Local(usize),
}
struct OperandMap<'a, 'b> {
    symbol_map: HashMap<&'b Symbol<'a>, OperandScope>,
    temp_map: HashMap<&'b TempOperandStruct, OperandScope>,
    current_offset: usize,
}

impl<'a, 'b> OperandMap<'a, 'b> {
    fn new() -> OperandMap<'a, 'b> {
        OperandMap { symbol_map: HashMap::new(), temp_map: HashMap::new(), current_offset: 16 }
    }

    fn insert_global_opd(&mut self, opd: &'b SymbolOperandStruct<'a>) {
        let sym = opd.symbol.as_ref();
        self.symbol_map.insert(sym, OperandScope::Global);
    }

    fn insert_sym_opd(&mut self, opd: &'b SymbolOperandStruct<'a>) {
        let sym = opd.symbol.as_ref();
        self.current_offset += sym.typ.size();
        self.symbol_map.insert(sym, OperandScope::Local(self.current_offset));
    }
    
    fn insert_temp_opd(&mut self, opd: &'b TempOperandStruct) {
        self.current_offset += opd.width;
        self.temp_map.insert(opd, OperandScope::Local(self.current_offset));
    }

    fn get_opd(&self, opd: &'b Operand) -> &OperandScope {
        match opd {
            Operand::SymbolOperand(SymbolOperandStruct { symbol })
            | Operand::AddrOperand(AddrOperandStruct { symbol })
            | Operand::DerefOperand(DerefOperandStruct { symbol }) => self.symbol_map.get(symbol.as_ref()).expect(&format!("Location unknown for symbol {}", symbol.name)),
            Operand::TempOperand(t) => self.temp_map.get(t).expect(&format!("Location unknown for operand tmp_{}", t.id)),
            Operand::LitOperand(_) | Operand::StringOperand(_) => panic!("Attempt to get location of a literal or string operand"),
        }
    }
}

impl<'a> Operand<'a> {
    fn x64_opd(&self, map: &OperandMap) -> String {
        match self {
            Operand::LitOperand(l) => l.value.to_string(),
            Operand::SymbolOperand(_) 
            | Operand::AddrOperand(_) 
            | Operand::DerefOperand(_) 
            | Operand::TempOperand(_) => format!("-{}(%rbp)", map.get_offset(self)),
            Operand::StringOperand(s) => format!("str_{}", s.id),
        }
    }
}

pub fn write_x64(program: &IRProgram, outfile: &mut fs::File) {
    let mut s = String::with_capacity(13);
    let mut map = OperandMap::new();
    program.x64_codegen(&mut s, &mut map);
    write!(outfile, "{}", s).unwrap();
}

#[enum_dispatch(Quad)]
trait X64Codegen<'a> {
    fn x64_codegen<'b>(&'b self, out: &mut String, offset_table: &mut OperandMap<'a, 'b>);
}

impl<'a> X64Codegen<'a> for IRProgram<'a> {
    fn x64_codegen<'b>(&'b self, out: &mut String, offset_table: &mut OperandMap<'a, 'b>) {
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
            proc.x64_codegen(out, offset_table);
        }
    }
}

impl<'a> X64Codegen<'a> for IRProcedure<'a> {
    fn x64_codegen<'b>(&'b self, out: &mut String, offset_table: &mut OperandMap<'a, 'b>) {
        // First, generate the offset of all the variables we need
        for sym_opd in self.formals.iter().chain(&self.locals) {
            offset_table.insert_sym_opd(sym_opd);
        }
        for temp_opd in &self.temps {
            offset_table.insert_temp_opd(temp_opd);
        }
        // Finally, generate the x64 for all of the quads.
        for quad in &self.quads {
            quad.x64_codegen(out, offset_table);
        }
    }
}

impl<'a> X64Codegen<'a> for LabeledQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        out.push_str(&self.label.0);
        self.quad.x64_codegen(out, offset_table);
    }
}

impl<'a> X64Codegen<'a> for AssignQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        out.push_str(&format!("movq "))
    }

}
