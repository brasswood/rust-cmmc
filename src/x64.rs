// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use std::collections::HashMap;
use std::fs;
use std::io::Write;

use enum_dispatch::enum_dispatch;

use crate::ir::*;
use crate::name::symbol::{SymbolType, Symbol};

#[derive(Clone)]
enum OperandScope {
    Global,
    Local(usize),
}

#[derive(Clone)]
struct OperandMap<'a, 'b> {
    symbol_map: HashMap<&'b Symbol<'a>, OperandScope>,
    temp_map: HashMap<&'b TempOperandStruct, OperandScope>,
    current_offset: usize,
    num_formals: usize,
}

impl<'a, 'b> OperandMap<'a, 'b> {
    fn new() -> OperandMap<'a, 'b> {
        OperandMap { symbol_map: HashMap::new(), temp_map: HashMap::new(), current_offset: 16, num_formals: 0 }
    }

    fn insert_global_opd(&mut self, opd: &'b SymbolOperandStruct<'a>) {
        let sym = opd.symbol.as_ref();
        self.symbol_map.insert(sym, OperandScope::Global);
    }

    fn insert_formal_opd(&mut self, opd: &'b SymbolOperandStruct<'a>) {
        self.num_formals += 1;
        self.insert_sym_opd(opd);
    }

    fn insert_sym_opd(&mut self, opd: &'b SymbolOperandStruct<'a>) {
        let sym = opd.symbol.as_ref();
        self.current_offset += opd.size()/8;
        self.symbol_map.insert(sym, OperandScope::Local(self.current_offset));
    }
    
    fn insert_temp_opd(&mut self, opd: &'b TempOperandStruct) {
        self.current_offset += opd.size()/8;
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

    fn all_vars_aligned(&self) -> usize {
        self.current_offset - 16 + ((16 - (self.current_offset % 16)) % 16) // is there a more concise way?
    }

    fn getarg_rbp_offset(&self, idx: usize) -> usize {
        match idx {
            0..=5 => 0,
            i => 8*(self.num_formals - (i + 1) + (self.num_formals % 2)) // the % 2 part is for SystemV stack alignment.
        }
    }
}

impl<'a> Operand<'a> {
    fn load<'b>(&self, to_reg: &str, out: &mut String, offset_table: &mut OperandMap<'a, 'b>) {
        let mov_inst = match self.size() {
            8 => "mov",
            64 => "movq",
            _ => unreachable!(),
        };
        match self {
            Operand::LitOperand(l) => {
                out.push_str(&format!("{} ${}, {}\n", mov_inst, l.value.to_string(), to_reg));
            }
            Operand::SymbolOperand(s) => {
                let opd_scope = offset_table.get_opd(self);
                let opd_str = match opd_scope {
                    OperandScope::Global => format!("(gbl_{})", s.symbol.name),
                    OperandScope::Local(offset) => format!("-{}(%rbp)", offset),
                };
                out.push_str(&format!("{} {}, {}\n", mov_inst, opd_str, to_reg));
            }
            Operand::AddrOperand(a) => {
                let opd_scope = offset_table.get_opd(self);
                match opd_scope {
                    OperandScope::Global => {
                        out.push_str(&format!("movq $gbl_{}, {}\n", a.symbol.name, to_reg));
                    }
                    OperandScope::Local(offset) => {
                        out.push_str(&format!("movq %rbp, {}\nsubq ${}, {}\n", to_reg, offset, to_reg));
                    }
                }
            }
            Operand::DerefOperand(d) => {
                let opd_scope = offset_table.get_opd(self);
                match opd_scope {
                    OperandScope::Global => {
                        out.push_str(&format!("movq (gbl_{}), %r12\n{} (%r12), {}\n", d.symbol.name, mov_inst, to_reg));
                    }
                    OperandScope::Local(offset) => {
                        out.push_str(&format!("movq -{}(%rbp), %r12\n{} (%r12), {}\n", offset, mov_inst, to_reg));
                    }
                }
            }
            Operand::TempOperand(t) => {
                let opd_scope = offset_table.get_opd(self);
                match opd_scope {
                    // temps are always locals
                    OperandScope::Global => panic!("tmp_{} was found in the global scope", t.id),
                    OperandScope::Local(offset) => {
                        out.push_str(&format!("{} -{}(%rbp), {}\n", mov_inst, offset, to_reg));
                    }
                }
            }
            Operand::StringOperand(s) => {
                out.push_str(&format!("movq $str_{}, {}\n", s.id, to_reg)); // string lits will be pointers
            }
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
                    OperandScope::Global => out.push_str(&format!("movq (gbl_{}), %r12\n{} {}, (%r12)\n", d.symbol.name, mov_inst, val)),
                    OperandScope::Local(offset) => out.push_str(&format!("movq -{}(%rbp), %r12\n{} {}, (%r12)\n", offset, mov_inst, val)),
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
            proc.x64_codegen(out, &mut offset_table.clone());
        }
    }
}

impl<'a> X64Codegen<'a> for IRProcedure<'a> {
    fn x64_codegen<'b>(&'b self, out: &mut String, offset_table: &mut OperandMap<'a, 'b>) {
        // First, generate the offset of all the variables we need
        for form_opd in &self.formals {
            offset_table.insert_formal_opd(form_opd);
        }
        for sym_opd in &self.locals {
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
        if self.label.0 != "" {
            out.push_str(&format!("{}: ", self.label.0));
        }
        self.quad.x64_codegen(out, offset_table);
    }
}

impl<'a> X64Codegen<'a> for AssignQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        let reg = match self.src.size() {
            8 => "%al",
            64 => "%rax",
            _ => unreachable!(),
        };
        self.src.load(reg, out, offset_table);
        self.dest.store(reg, out, offset_table);
    }
}

impl<'a> X64Codegen<'a> for ShortToIntQuad<'a> {
    fn x64_codegen<'b>(&'b self, out: &mut String, offset_table: &mut OperandMap<'a, 'b>) {
        self.src.load("%al", out, offset_table);
        out.push_str("movsx %al, %rax\n");
        self.dest.store("%rax", out, offset_table);
    }
}

impl<'a> X64Codegen<'a> for UnaryQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        match self.opcode {
            UnaryOp::Neg64 => {
                self.src.load("%rax", out, offset_table);
                out.push_str("negq %rax\n");
                self.dest.store("%rax", out, offset_table);
            }
            UnaryOp::Neg8 => {
                self.src.load("%al", out, offset_table);
                out.push_str("neg %al\n");
                self.dest.store("%al", out, offset_table);
            },
            UnaryOp::Not8 => {
                self.src.load("%al", out, offset_table);
                out.push_str("not %al\n");
                self.dest.store("%al", out, offset_table);
            }
        }
    }
}

impl<'a> X64Codegen<'a> for BinaryQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        match self.opcode {
            BinaryOp::Add64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("addq %rbx, %rax\n");
                self.dest.store("%rax", out, offset_table);
            }
            BinaryOp::Add8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("add %bl, %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Sub64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("subq %rbx, %rax\n");
                self.dest.store("%rax", out, offset_table);
            }
            BinaryOp::Sub8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("sub %bl, %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Div64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("idivq %rbx\n");
                self.dest.store("%rax", out, offset_table);
            }
            BinaryOp::Div8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("idiv %bl\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Mult64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("imulq %rbx\n");
                self.dest.store("%rax", out, offset_table);
            }
            BinaryOp::Mult8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("imul %bl\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Eq64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("cmpq %rax, %rbx\nsete %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Eq8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("cmp %al, %bl\nsete %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Neq64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("cmpq %rax, %rbx\nsetne %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Neq8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("cmp %al, %bl\nsetne %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Lt64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("cmpq %rax, %rbx\nsetl %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Lt8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("cmp %al, %bl\nsetl %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Gt64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("cmpq %rax, %rbx\nsetg %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Gt8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("cmp %al, %bl\nsetg %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Lte64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("cmpq %rax, %rbx\nsetle %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Lte8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("cmp %al, %bl\nsetle %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Gte64 => {
                self.lhs.load("%rax", out, offset_table);
                self.rhs.load("%rbx", out, offset_table);
                out.push_str("cmpq %rax, %rbx\nsetge %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Gte8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("cmp %al, %bl\nsetge %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::And8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("and %bl, %al\n");
                self.dest.store("%al", out, offset_table);
            }
            BinaryOp::Or8 => {
                self.lhs.load("%al", out, offset_table);
                self.rhs.load("%bl", out, offset_table);
                out.push_str("or %bl, %al\n");
                self.dest.store("%al", out, offset_table);
            }
        }
    }
}

impl<'a> X64Codegen<'a> for UnconditionalJumpQuad {
    fn x64_codegen<'b>(& 'b self, out: &mut String, _offset_table: &mut OperandMap< 'a, 'b>) {
        out.push_str(&format!("jmp {}\n", self.label.0));
    }
}

impl<'a> X64Codegen<'a> for ConditionalJumpQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        self.condition_src.load("%al", out, offset_table);
        out.push_str(&format!("cmp %al, $0\nje {}\n", self.label.0));
    }
}

impl<'a> X64Codegen<'a> for EnterQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        out.push_str(&format!("pushq %rbp\nmovq %rsp, %rbp\naddq $16, %rbp\nsubq ${}, %rsp\n", offset_table.all_vars_aligned()));
    }
}

impl<'a> X64Codegen<'a> for LeaveQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        out.push_str(&format!("addq ${}, %rsp\npopq %rbp\nretq\n", offset_table.all_vars_aligned()));
    }
}

impl<'a> X64Codegen<'a> for GetArgQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        match self.idx {
            0 => self.dest.store("%rdi", out, offset_table),
            1 => self.dest.store("%rsi", out, offset_table),
            2 => self.dest.store("%rdx", out, offset_table),
            3 => self.dest.store("%rcx", out, offset_table),
            4 => self.dest.store("%r08", out, offset_table),
            5 => self.dest.store("%r09", out, offset_table),
            i => {
                let src = match offset_table.getarg_rbp_offset(i) {
                    0 => "(%rbp)".to_string(),
                    offset => format!("{}(%rbp)", offset),
                };
                out.push_str(&format!("movq {}, %rax\n", src));
                self.dest.store("%rax", out, offset_table);
            }
        };
    }
}

impl<'a> X64Codegen<'a> for SetRetQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        self.src.load("%rax", out, offset_table);
    }
}

impl<'a> X64Codegen<'a> for CallQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        // 16-byte-align the stack before the call
        if offset_table.num_formals > 6 && offset_table.num_formals % 2 != 0 {
            out.push_str(&format!("subq $1, %rsp\n"));
        }
        out.push_str(&format!("callq fun_{}\n", self.func.name));
        // clean up if needed
        if offset_table.num_formals > 6 {
            let val = 8*(offset_table.num_formals - 6 + (offset_table.num_formals % 2));
            out.push_str(&format!("addq ${}, %rsp\n", val));
        }
    }
}

impl<'a> X64Codegen<'a> for SetArgQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        match self.idx {
            0 => self.src.load("%rdi", out, offset_table),
            1 => self.src.load("%rsi", out, offset_table),
            2 => self.src.load("%rdx", out, offset_table),
            3 => self.src.load("%rcx", out, offset_table),
            4 => self.src.load("%r08", out, offset_table),
            5 => self.src.load("%r09", out, offset_table),
            _ => {
                // okay this is sketchy as hekc but it should work because
                // all of the setarg quads are consecutively next to each other.
                self.src.load("%rax", out, offset_table);
                out.push_str(&format!("pushq %rax\n"));
            }
        }
    }
}

impl<'a> X64Codegen<'a> for GetRetQuad<'a> {
    fn x64_codegen<'b>(& 'b self, out: &mut String, offset_table: &mut OperandMap< 'a, 'b>) {
        self.dest.store("%rax", out, offset_table);
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
    fn x64_codegen<'b>(& 'b self, out: &mut String, _offset_table: &mut OperandMap< 'a, 'b>) {
        out.push_str("nop\n");
    }
}