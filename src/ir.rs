use std::rc::{Rc, Weak};
use crate::ast::*;

use crate::name::symbol::SymbolType;
use crate::{name::symbol::Symbol, ast::ProgramNode};

struct IRProgram<'a> {
    globals: Vec<SymbolOperandStruct<'a>>,
    procedures: Vec<IRProcedure<'a>>,
}

struct IRProcedure<'a> {
    formals: Vec<SymbolOperandStruct<'a>>,
    locals: Vec<SymbolOperandStruct<'a>>,
    temps: Vec<TempOperandStruct>,
    quads: Vec<LabeledQuad<'a>>,
}

enum Operand<'a> {
    LitOperand(LitOperandStruct),
    SymbolOperand(SymbolOperandStruct<'a>),
    AddrOperand(AddrOperandStruct<'a>),
    TempOperand(TempOperandStruct),
}

struct SymbolOperandStruct<'a> { symbol: Weak<Symbol<'a>>, width: usize }

struct LitOperandStruct { value: String, width: usize }

struct AddrOperandStruct<'a> { opd: Box<Operand<'a>> }

struct TempOperandStruct { id: usize, width: usize }

struct LabeledQuad<'a> {
    label: String,
    quad: Quad<'a>,
}

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

impl<'a> ProgramNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>) {
        for decl in &self.0 {
            match decl {
                DeclNode::VarDecl(VarDeclNode { symbol: Some(s), .. }) => {
                    let symbol = Rc::downgrade(&s);
                    let width = s.typ.size();
                    let opd = SymbolOperandStruct { symbol, width };
                    program.globals.push(opd);
                }
                _ => todo!(),
            }
        }
    }
}