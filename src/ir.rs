use std::rc::Rc;
use enum_dispatch::enum_dispatch;
use crate::ast::*;
use std::fs;
use std::io::Write;

use crate::name::symbol::SymbolType;
use crate::{name::symbol::Symbol, ast::ProgramNode};

#[enum_dispatch(ToString)]
trait ToString {
    fn to_string(&self) -> String;
}

pub fn write_3ac(tree: &ProgramNode, outfile: &mut fs::File) {
    let ir = IRProgram::from(tree);
    writeln!(outfile, "{}", ir.to_string()).unwrap();
}

struct IRProgram<'a> {
    globals: Vec<SymbolOperandStruct<'a>>,
    procedures: Vec<IRProcedure<'a>>,
    label_num: usize,
}

struct IRProcedure<'a> {
    symbol: Rc<Symbol<'a>>,
    formals: Vec<SymbolOperandStruct<'a>>,
    locals: Vec<SymbolOperandStruct<'a>>,
    temps: Vec<TempOperandStruct>,
    quads: Vec<LabeledQuad<'a>>,
}

#[enum_dispatch(ToString)]
enum Operand<'a> {
    LitOperand(LitOperandStruct),
    SymbolOperand(SymbolOperandStruct<'a>),
    AddrOperand(AddrOperandStruct<'a>),
    TempOperand(TempOperandStruct),
}

#[derive(Clone)]
struct SymbolOperandStruct<'a> { symbol: Rc<Symbol<'a>>, width: usize }

struct LitOperandStruct { value: String, width: usize }

struct AddrOperandStruct<'a> { symopd: SymbolOperandStruct<'a> }

struct TempOperandStruct { id: usize, width: usize }

struct LabeledQuad<'a> {
    label: String,
    quad: Quad<'a>,
}

#[enum_dispatch(ToString)]
enum Quad<'a> {
    Assign(AssignQuad<'a>),
    Unary(UnaryQuad<'a>),
    Binary(BinaryQuad<'a>),
    UnconditionalJump(UnconditionalJumpQuad),
    ConditionalJump(ConditionalJumpQuad),
    Enter(EnterQuad<'a>),
    Leave(LeaveQuad<'a>),
    GetArg(GetArgQuad<'a>),
    SetRet(SetRetQuad<'a>),
    Call(CallQuad<'a>),
    SetArg(SetArgQuad<'a>),
    GetRet(GetRetQuad<'a>),
    Receive(ReceiveQuad<'a>),
    Report(ReportQuad<'a>),
    Nop(NopQuad),
}

struct AssignQuad<'a> { dest: Operand<'a>, src: Operand<'a> }

struct UnaryQuad<'a> { dest: Operand<'a>, src: Operand<'a>, opcode: UnaryOp }

enum UnaryOp { Not8, Neg64 }

struct BinaryQuad<'a> { dest: Operand<'a>, src: Operand<'a>, opcode: BinaryOp }

enum BinaryOp { Add64, Sub64, Div64, Mult64, Eq64, Neq64, Lt64, Gt64, Lte64, Gte64, And64, Or64 }

struct UnconditionalJumpQuad { label: String }

struct ConditionalJumpQuad { label: String }

struct EnterQuad<'a> { func: Rc<Symbol<'a>> }

struct LeaveQuad<'a> { func: Rc<Symbol<'a>> }

struct GetArgQuad<'a> { idx: usize, dest: Operand<'a> }

struct SetRetQuad<'a> { src: Operand<'a> }

struct CallQuad<'a> { func: Rc<Symbol<'a>> }

struct SetArgQuad<'a> { idx: usize, src: Operand<'a> }

struct GetRetQuad<'a> { dest: Operand<'a> }

struct ReceiveQuad<'a> { dest: Operand<'a> }

struct ReportQuad<'a> { src: Operand<'a> }

struct NopQuad;

impl SymbolType {
    fn size(&self) -> usize {
        match self {
            SymbolType::Int => 64,
            SymbolType::Short => 8,
            SymbolType::Bool => 8,
            SymbolType::Str => unreachable!(),
            SymbolType::Void => unreachable!(),
            SymbolType::Ptr(_) => 64,
            SymbolType::Fn { .. } => unreachable!(),
        }
    }
}

impl<'a> VarDeclNode<'a> {
    fn emit_3ac_global(&self, program: &mut IRProgram<'a>) {
        let symbol = self.symbol.as_ref().expect("Symbol not found. Did you do type analysis?");
        program.globals.push(SymbolOperandStruct::from(symbol));
    }

    fn emit_3ac_local(&self, procedure: &mut IRProcedure<'a>) {
        let symbol = self.symbol.as_ref().expect("Symbol not found. Did you do type analysis?");
        procedure.locals.push(SymbolOperandStruct::from(symbol));
    }
}

impl<'a> FnDeclNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>) {
        let symbol = self.symbol.as_ref().expect("Symbol not found. Did you do type analysis?");
        let mut proc = IRProcedure::from_symbol(symbol);
        // first add the enter fn quad
        proc.quads.push(LabeledQuad {
            label: if self.symbol.as_ref().unwrap().name == "main" {
                "main".to_string()
            } else {
                format!("fun_{}", self.symbol.as_ref().unwrap().name.clone())
            },
            quad: Quad::Enter(EnterQuad { func: Rc::clone(symbol) }),
        });
        // getargs
        for (idx, formal_decl) in self.formals.iter().enumerate() {
            formal_decl.emit_3ac(idx, &mut proc)
        }
        // body
        for stmt in &self.stmts {
            match stmt {
                StmtNode::Decl(DeclNode::VarDecl(d)) => d.emit_3ac_local(&mut proc),
                StmtNode::Decl(_) => unreachable!(),
                // Scuffed (tm)
                StmtNode::AssignStmt(s) => s.emit_3ac(&mut proc),
                StmtNode::CallStmt(s) => s.emit_3ac(&mut proc),
                StmtNode::IfStmt(s) => s.emit_3ac(&mut proc),
                StmtNode::IfElseStmt(s) => s.emit_3ac(&mut proc),
                StmtNode::WhileStmt(s) => s.emit_3ac(&mut proc),
                StmtNode::PostIncStmt(s) => s.emit_3ac(&mut proc),
                StmtNode::PostDecStmt(s) => s.emit_3ac(&mut proc),
                StmtNode::ReadStmt(s) => s.emit_3ac(&mut proc),
                StmtNode::WriteStmt(s) => s.emit_3ac(&mut proc),
                StmtNode::ReturnStmt(s) => s.emit_3ac(&mut proc),
            }
        }
        // leave fn
        proc.quads.push(LabeledQuad {
            label: format!("lbl_{}", program.get_label_num()),
            quad: Quad::Leave(LeaveQuad { func: Rc::clone(symbol) })
        });
        // add the proc to the prog
        program.procedures.push(proc);
        // done
    }
}

impl<'a> FormalDeclNode<'a> {
    fn emit_3ac(&self, idx: usize, procedure: &mut IRProcedure<'a>) {
        let sym = SymbolOperandStruct::from(self.symbol.as_ref().expect("Symbol not found. Did you do type analysis?"));
        procedure.formals.push(sym.clone());
        procedure.quads.push(quad(Quad::GetArg(GetArgQuad { idx, dest: Operand::SymbolOperand(sym) })));
    }
}

impl<'a> AssignStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> CallStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> IfStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> IfElseStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> WhileStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> PostIncStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> PostDecStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> ReadStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> WriteStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> ReturnStmtNode<'a> {
    fn emit_3ac(&self, procedure: &mut IRProcedure<'a>) {
        todo!()
    }
}

impl<'a> IRProgram<'a> {
    fn from(tree: &ProgramNode<'a>) -> IRProgram<'a> {
        let mut program = IRProgram { globals: Vec::new(), procedures: Vec::new(), label_num: 0 };
        for decl in &tree.0 {
            match decl {
                DeclNode::VarDecl(d) => d.emit_3ac_global(&mut program),
                DeclNode::FnDecl(d) => d.emit_3ac(&mut program),
                _ => unreachable!(),
            }
        }
        program
    }
    
    fn to_string(&self) -> String {
        let mut global_string = String::new();
        for g in &self.globals {
            global_string.push_str(&format!("{}\n", g.to_str()));
        }
        let mut proc_strings = String::new();
        for p in &self.procedures {
            proc_strings.push_str(&p.header_string());
            proc_strings.push_str(&p.to_string());
        }
        format!(
            "[BEGIN GLOBALS]\n\
            {}\
            [END GLOBALS]\n\
            {}",
            global_string,
            proc_strings,
        )
    }

    fn get_label_num(&mut self) -> usize {
        let ret = self.label_num;
        self.label_num += 1;
        ret
    }
}

impl<'a> IRProcedure<'a> {
    fn from_symbol(sym: &Rc<Symbol<'a>>) -> IRProcedure<'a> {
        IRProcedure { symbol: Rc::clone(sym), formals: Vec::new(), locals: Vec::new(), temps: Vec::new(), quads: Vec::new() }
    }
    fn header_string(&self) -> String {
        let mut vars_string = String::new();
        for formal in &self.formals {
            vars_string.push_str(&format!("{} (formal arg of {})\n", formal.to_str(), formal.symbol.typ.size()));
        }
        for local in &self.locals {
            vars_string.push_str(&format!("{} (local var of {} bytes)\n", local.to_str(), local.symbol.typ.size()));
        }
        for temp in &self.temps {
            vars_string.push_str(&format!("{} (tmp var of {} bytes)\n", temp.to_string(), temp.width));
        }
        format!(
            "[BEGIN {} LOCALS]\n\
            {}\
            [END {} LOCALS]\n",
            self.symbol.name,
            vars_string,
            self.symbol.name,
        )
    }
    fn to_string(&self) -> String {
        let mut ret = String::new();
        for quad in &self.quads {
            ret.push_str(&format!("{}\n", quad.to_string()));
        }
        ret
    }
}

impl<'a> SymbolOperandStruct<'a> {
    fn from(symbol: &Rc<Symbol<'a>>) -> SymbolOperandStruct<'a> {
        SymbolOperandStruct {
            symbol: Rc::clone(symbol), width: symbol.typ.size()
        }
    }
    fn to_str(&self) -> &'a str {
        self.symbol.name
    }
}

impl<'a> LabeledQuad<'a> {
    fn to_string(&self) -> String {
        let lbl = if self.label == "" {
            "                ".to_string()
        } else {
            let spaces = " ".repeat(std::cmp::max(0, 16 - self.label.len() - 2));
            format!("{}: {}", self.label, spaces)
        };
        format!("{}{}", lbl, self.quad.to_string())
    }
}

impl<'a> ToString for AssignQuad<'a> {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl<'a> ToString for UnaryQuad<'a> {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl<'a> ToString for BinaryQuad<'a> {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ToString for UnconditionalJumpQuad {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ToString for ConditionalJumpQuad {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl<'a> ToString for EnterQuad<'a> {
    fn to_string(&self) -> String {
        format!("enter {}", self.func.name)
    }
}

impl<'a> ToString for LeaveQuad<'a> {
    fn to_string(&self) -> String {
        format!("leave {}", self.func.name)
    }
}

impl<'a> ToString for GetArgQuad<'a> {
    fn to_string(&self) -> String {
        format!("getarg {} {}", self.idx, self.dest.to_string())
    }
}

impl<'a> ToString for SetRetQuad<'a> {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl<'a> ToString for CallQuad<'a> {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl<'a> ToString for SetArgQuad<'a> {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl<'a> ToString for GetRetQuad<'a> {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl<'a> ToString for ReceiveQuad<'a> {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl<'a> ToString for ReportQuad<'a> {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ToString for NopQuad {
    fn to_string(&self) -> String {
        "nop".to_string()
    }
}

impl<'a> ToString for SymbolOperandStruct<'a> {
    fn to_string(&self) -> String {
        // with []
        format!("[{}]", self.symbol.name)
    }
}

impl ToString for LitOperandStruct {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl<'a> ToString for AddrOperandStruct<'a> {
    fn to_string(&self) -> String {
        // without []
        self.symopd.symbol.name.to_string()
    }
}

impl ToString for TempOperandStruct {
    fn to_string(&self) -> String {
        format!("tmp{}", self.width)
    }
}

fn quad(quad: Quad) -> LabeledQuad {
    LabeledQuad { label: String::new(), quad }
}