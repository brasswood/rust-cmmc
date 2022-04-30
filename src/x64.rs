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
        self.current_offset += opd.size();
        self.symbol_map.insert(sym, OperandScope::Local(self.current_offset));
    }
    
    fn insert_temp_opd(&mut self, opd: &'b TempOperandStruct) {
        self.current_offset += opd.size();
        self.temp_map.insert(opd, OperandScope::Local(self.current_offset));
    }

    fn get_opd(&self, opd: &'b Operand) -> &OperandScope {
        match opd {
            Operand::SymbolOperand(SymbolOperandStruct { symbol }) => self.symbol_map.get(symbol.as_ref()).expect(&format!("Location unknown for symbol {}", symbol.name)),
            Operand::TempOperand(t) => self.temp_map.get(t).expect(&format!("Location unknown for operand tmp_{}", t.id)),
            Operand::LitOperand(_) | Operand::StringOperand(_) => panic!("Attempt to get location of a literal or string operand"),
            Operand::AddrOperand(a) => self.get_opd_from_symbol(a.symbol.as_ref()),
            Operand::DerefOperand(d) => self.get_opd_from_symbol(d.symbol.as_ref()),
        }
    }

    fn get_opd_from_symbol(&self, sym: &'b Symbol) -> &OperandScope {
        self.symbol_map.get(sym).expect(&format!("Location of symbol {} not found", sym.name))
    }
}

impl<'a> Operand<'a> {
    fn load<'b>(&self, available_reg: &str, out: &mut String, offset_table: &mut OperandMap<'a, 'b>) -> String {
        let mov_inst = match self.size() {
            8 => "mov",
            16 => "movq",
            _ => unreachable!(),
        };
        match self {
            Operand::LitOperand(l) => format!("${}", l.value.to_string()),
            Operand::SymbolOperand(s) => {
                let opd_scope = offset_table.get_opd(self);
                let opd_str = match opd_scope {
                    OperandScope::Global => format!("(gbl_{})", s.symbol.name),
                    OperandScope::Local(offset) => format!("-{}(%rbp)", offset),
                };
                out.push_str(&format!("{} {}, {}\n", mov_inst, opd_str, available_reg));
                available_reg.to_string()
            }
            Operand::AddrOperand(a) => {
                let opd_scope = offset_table.get_opd(self);
                match opd_scope {
                    OperandScope::Global => format!("$gbl_{}", a.symbol.name),
                    OperandScope::Local(offset) => {
                        out.push_str(&format!("movq %rbp, {}\nsubq ${}, {}\n", available_reg, offset, available_reg));
                        available_reg.to_string()
                    }
                }
            }
            Operand::DerefOperand(d) => {
                let opd_scope = offset_table.get_opd(self);
                match opd_scope {
                    OperandScope::Global => {
                        out.push_str(&format!("movq (gbl_{}), %rcx\n{} (%rcx), {}\n", d.symbol.name, mov_inst, available_reg));
                        available_reg.to_string()
                    }
                    OperandScope::Local(offset) => {
                        out.push_str(&format!("movq -{}(%rbp), %rcx\n{} (%rcx), {}\n", offset, mov_inst, available_reg));
                        available_reg.to_string()
                    }
                }
            }
            Operand::TempOperand(t) => {
                let opd_scope = offset_table.get_opd(self);
                match opd_scope {
                    // temps are always locals
                    OperandScope::Global => panic!("tmp_{} was found in the global scope", t.id),
                    OperandScope::Local(offset) => {
                        out.push_str(&format!("{} -{}(%rbp), {}\n", mov_inst, offset, available_reg));
                        available_reg.to_string()
                    }
                }
            }
            Operand::StringOperand(s) => format!("$str_{}", s.id), // string lits will be pointers
        }
    }

    fn store<'b>(&self, val: &str, out: &mut String, offset_table: &mut OperandMap<'a, 'b>) {
        let mov_inst = match self.size() {
            64 => "movq",
            8 => "mov",
            _ => unreachable!(),
        };
        
        match self {
            Operand::LitOperand(l) => panic!("Attempt to store to literal {}", l.value.to_string()),
            Operand::SymbolOperand(s) => {
                let opd_scope = offset_table.get_opd(self);
                match opd_scope {
                    OperandScope::Global => out.push_str(&format!("{} {}, (gbl_{})\n", mov_inst, val, s.symbol.name)),
                    OperandScope::Local(offset) => out.push_str(&format!("{} {}, -{}(%rbp)\n", mov_inst, val, offset)),
                }
            }
            Operand::AddrOperand(a) => panic!("Attempt to store to the address of {}", a.symbol.name),
            Operand::DerefOperand(d) => {
                let opd_scope = offset_table.get_opd(self);
                match opd_scope {
                    OperandScope::Global => out.push_str(&format!("movq (gbl_{}), %rcx\n{} {}, (%rcx)\n", d.symbol.name, mov_inst, val)),
                    OperandScope::Local(offset) => out.push_str(&format!("movq -{}(%rbp), %rcx\n{} {}, (%rcx)\n", offset, mov_inst, val)),
                }
            },
            Operand::TempOperand(t) => {
                let opd_scope = offset_table.get_opd(self);
                match opd_scope {
                    OperandScope::Global => panic!("tmp_{} was found in the global scope", t.id),
                    OperandScope::Local(offset) => out.push_str(&format!("{} {}, -{}(%rbp)\n", mov_inst, val, offset)),
                }
            },
            Operand::StringOperand(s) => panic!("Attempt to write to string str_{}", s.id),
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
            offset_table.insert_global_opd(global);
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
        out.push_str(&format!("{}: ", self.label.0));
        self.quad.x64_codegen(out, offset_table);
    }
}

impl<'a> X64Codegen<'a> for AssignQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        out.push_str(&format!("movq {}, %rax\nmovq %rax, {}\n", self.src.x64_opd(offset_table), self.dest.x64_opd(offset_table)))
    }
}

impl<'a> X64Codegen<'a> for ShortToIntQuad<'a> {
    fn x64_codegen<'b>(&'b self, out: &mut String, offset_table: &mut OperandMap<'a, 'b>) {
        todo!();
    }
}

impl<'a> X64Codegen<'a> for UnaryQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        match self.opcode {
            UnaryOp::Neg64 => out.push_str(&format!("movq {}, %rax\nnegq %rax\nmovq %rax, {}\n", self.src.x64_opd(offset_table), self.dest.x64_opd(offset_table))),
            UnaryOp::Neg8 => todo!(),
            UnaryOp::Not8 => out.push_str(&format!("mov {}, %ax\nnot %ax\nmov %ax, {}\n", self.src.x64_opd(offset_table), self.dest.x64_opd(offset_table))),
        }
    }
}

impl<'a> X64Codegen<'a> for BinaryQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for UnconditionalJumpQuad {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for ConditionalJumpQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for EnterQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for LeaveQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for GetArgQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for SetRetQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for CallQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for SetArgQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for GetRetQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for ReceiveQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for ReportQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}

impl<'a> X64Codegen<'a> for NopQuad {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        todo!()
    }
}