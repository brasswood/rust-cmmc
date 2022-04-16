use std::rc::Rc;
use enum_dispatch::enum_dispatch;
use crate::ast::{*, self};
use std::fs;
use std::io::Write;
use crate::type_check::TypeCheck;

use crate::name::symbol::SymbolType;
use crate::{name::symbol::Symbol, ast::ProgramNode};

#[enum_dispatch]
trait ToString {
    fn to_string(&self) -> String;
}

// I'm only defining this trait so I can enum_dispatch with it
#[enum_dispatch(ExpNode, LValNode)]
trait Flatten<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) -> Operand<'a>;
}

// Like Flatten, but does not return an operand. Used for statements.
#[enum_dispatch(StmtNode)]
trait Emit3AC<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>);
}

pub fn write_3ac(tree: &ProgramNode, outfile: &mut fs::File) {
    let ir = IRProgram::from(tree);
    writeln!(outfile, "{}", ir.to_string()).unwrap();
}

struct IRProgram<'a> {
    globals: Vec<SymbolOperandStruct<'a>>,
    strings: Vec<StringOperandStruct<'a>>,
    procedures: Vec<IRProcedure<'a>>,
    label_num: usize,
    string_num: usize,
}

struct IRProcedure<'a> {
    symbol: Rc<Symbol<'a>>,
    formals: Vec<SymbolOperandStruct<'a>>,
    locals: Vec<SymbolOperandStruct<'a>>,
    temps: Vec<TempOperandStruct>,
    quads: Vec<LabeledQuad<'a>>,
    return_label: Label,
    temp_num: usize,
}

#[enum_dispatch(ToString)]
#[derive(Clone)]
enum Operand<'a> {
    LitOperand(LitOperandStruct),
    SymbolOperand(SymbolOperandStruct<'a>),
    AddrOperand(AddrOperandStruct<'a>),
    DerefOperand(DerefOperandStruct<'a>),
    TempOperand(TempOperandStruct),
    StringOperand(StringOperandStruct<'a>),
}

#[derive(Clone)]
struct SymbolOperandStruct<'a> { symbol: Rc<Symbol<'a>> }

#[derive(Clone)]
struct LitOperandStruct { value: u32, width: usize }

#[derive(Clone)]
struct AddrOperandStruct<'a> { symbol: Rc<Symbol<'a>> }

#[derive(Clone)]
struct DerefOperandStruct<'a> { symbol: Rc<Symbol<'a>> }

#[derive(Clone)]
struct TempOperandStruct { id: usize, width: usize }

#[derive(Clone)]
struct StringOperandStruct<'a> { id: usize, value: &'a str }

#[derive(Clone)]
struct Label (String);

struct LabeledQuad<'a> {
    label: Label,
    quad: Quad<'a>,
}

#[enum_dispatch(ToString)]
enum Quad<'a> {
    Assign(AssignQuad<'a>),
    Unary(UnaryQuad<'a>),
    Binary(BinaryQuad<'a>),
    UnconditionalJump(UnconditionalJumpQuad),
    ConditionalJump(ConditionalJumpQuad<'a>),
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

enum UnaryOp { Neg8, Neg64 }

struct BinaryQuad<'a> { dest: Operand<'a>, lhs: Operand<'a>, rhs: Operand<'a>, opcode: BinaryOp }

enum BinaryOp { Add64, Add8, Sub64, Sub8, Div64, Div8, Mult64, Mult8, Eq64, Eq8, Neq64, Neq8, Lt64, Lt8, Gt64, Gt8, Lte64, Lte8, Gte64, Gte8, And8, Or8 }

struct UnconditionalJumpQuad { label: Label }

struct ConditionalJumpQuad<'a> { condition_src: Operand<'a>, label: Label }

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
    fn size(&self) -> usize { // in bits
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
        let mut proc = program.make_procedure(symbol);
        // first add the enter fn quad
        proc.quads.push(LabeledQuad {
            label: if self.symbol.as_ref().unwrap().name == "main" {
                Label("main".to_string())
            } else {
                Label(format!("fun_{}", self.symbol.as_ref().unwrap().name.clone()))
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
                StmtNode::AssignStmt(s) => s.emit_3ac(program, &mut proc),
                StmtNode::CallStmt(s) => s.emit_3ac(program, &mut proc),
                StmtNode::IfStmt(s) => s.emit_3ac(program, &mut proc),
                StmtNode::IfElseStmt(s) => s.emit_3ac(program, &mut proc),
                StmtNode::WhileStmt(s) => s.emit_3ac(program, &mut proc),
                StmtNode::PostIncStmt(s) => s.emit_3ac(program, &mut proc),
                StmtNode::PostDecStmt(s) => s.emit_3ac(program, &mut proc),
                StmtNode::ReadStmt(s) => s.emit_3ac(program, &mut proc),
                StmtNode::WriteStmt(s) => s.emit_3ac(program, &mut proc),
                StmtNode::ReturnStmt(s) => s.emit_3ac(program, &mut proc),
            }
        }
        // leave fn
        proc.quads.push(LabeledQuad {
            label: proc.return_label.clone(),
            quad: Quad::Leave(LeaveQuad { func: Rc::clone(symbol) })
        });
        // add to the program (yeah it's sketchy but it's one way to get around Rust not allowing multiple mutable borrows at a time)
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

impl<'a> Emit3AC<'a> for DeclNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        match self {
            DeclNode::VarDecl(d) => d.emit_3ac_local(procedure),
            _ => unreachable!(),
        }
    }
}

impl<'a> Emit3AC<'a> for AssignStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        // exactly flatten, we just don't return the result
        self.exp.flatten(program, procedure);
    }
}

impl<'a> Emit3AC<'a> for CallStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        // like CallExpNode.flatten(), but we don't get a result.
        // setargs
        for (idx, arg) in self.exp.args.iter().enumerate() {
            let arg_temp = arg.flatten(program, procedure);
            procedure.quads.push(quad(Quad::SetArg(SetArgQuad { idx, src: arg_temp })));
        }
        // call
        procedure.quads.push(quad(Quad::Call(CallQuad { func: Rc::clone(self.exp.id.symbol.as_ref().unwrap()) })));
    }
}

impl<'a> Emit3AC<'a> for IfStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        // put the condition into a temp
        let cond_temp = self.exp.flatten(program, procedure);
        let label_over = program.get_label();
        procedure.quads.push(quad(Quad::ConditionalJump(ConditionalJumpQuad { condition_src: cond_temp, label: label_over.clone() })));
        for s in &self.stmts {
            s.emit_3ac(program, procedure);
        }
        procedure.quads.push(LabeledQuad { label: label_over, quad: Quad::Nop(NopQuad)});
    }
}

impl<'a> Emit3AC<'a> for IfElseStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let cond_temp = self.exp.flatten(program, procedure);
        let label_else = program.get_label();
        let label_skip_else = program.get_label();
        procedure.quads.push(quad(Quad::ConditionalJump(ConditionalJumpQuad { condition_src: cond_temp, label: label_else.clone() })));
        for s in &self.true_stmts {
            s.emit_3ac(program, procedure);
        }
        procedure.quads.push(quad(Quad::UnconditionalJump(UnconditionalJumpQuad { label: label_skip_else.clone() })));
        procedure.quads.push(LabeledQuad { label: label_else, quad: Quad::Nop(NopQuad) });
        for s in &self.else_stmts {
            s.emit_3ac(program, procedure);
        }
        procedure.quads.push(LabeledQuad { label: label_skip_else, quad: Quad::Nop(NopQuad) });
    }
}

impl<'a> Emit3AC<'a> for WhileStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let loop_label = program.get_label();
        // put a labeled nop before getting the condition temp. That way when we jump back to the head, we get the condition
        procedure.quads.push(LabeledQuad { label: loop_label.clone(), quad: Quad::Nop(NopQuad) });
        let cond_temp = self.exp.flatten(program, procedure);
        let loop_end_label = program.get_label();
        procedure.quads.push(quad(Quad::ConditionalJump(ConditionalJumpQuad { condition_src: cond_temp, label: loop_end_label.clone() })));
        // push the loop body
        for s in &self.stmts {
            s.emit_3ac(program, procedure);
        }
        // end of loop, do an unconditional jump to head
        procedure.quads.push(quad(Quad::UnconditionalJump(UnconditionalJumpQuad { label: loop_label })));
        // after loop condition fails, nop and carry on
        procedure.quads.push(LabeledQuad { label: loop_end_label, quad: Quad::Nop(NopQuad) });
    }
}

impl<'a> Emit3AC<'a> for PostIncStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let my_opd = self.lval.flatten(program, procedure);
        let typ = self.lval.type_check(SymbolType::Void).unwrap();
        let (opcode, width) = match typ {
            SymbolType::Int | SymbolType::Ptr(_) => (BinaryOp::Add64, 64),
            SymbolType::Short => (BinaryOp::Add8, 8),
            _ => unreachable!(),
        };
        procedure.quads.push(quad(Quad::Binary(BinaryQuad { dest: my_opd.clone(), lhs: my_opd.clone(), opcode, rhs: Operand::LitOperand(LitOperandStruct { value: 1, width }) })));
    }
}

impl<'a> Emit3AC<'a> for PostDecStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let my_opd = self.lval.flatten(program, procedure);
        let typ = self.lval.type_check(SymbolType::Void).unwrap();
        let (opcode, width) = match typ {
            SymbolType::Int | SymbolType::Ptr(_) => (BinaryOp::Sub64, 64),
            SymbolType::Short => (BinaryOp::Sub8, 8),
            _ => unreachable!(),
        };
        procedure.quads.push(quad(Quad::Binary(BinaryQuad { dest: my_opd.clone(), lhs: my_opd.clone(), opcode, rhs: Operand::LitOperand(LitOperandStruct { value: 1, width }) })));
    }
}

impl<'a> Emit3AC<'a> for ReadStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let dest = self.lval.flatten(program, procedure);
        procedure.quads.push(quad(Quad::Receive(ReceiveQuad { dest })));
    }
}

impl<'a> Emit3AC<'a> for WriteStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let src = self.exp.flatten(program, procedure);
        procedure.quads.push(quad(Quad::Report(ReportQuad { src })));
    }
}

impl<'a> Emit3AC<'a> for ReturnStmtNode<'a> {
    fn emit_3ac(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        match &self.exp {
            Some(exp) => {
                // ???
                let last_opd = exp.flatten(program, procedure);
                procedure.quads.push(quad(Quad::SetRet(SetRetQuad { src: last_opd })));
            }
            None => (),
        }
        procedure.quads.push(quad(Quad::UnconditionalJump(UnconditionalJumpQuad { label: procedure.return_label.clone() })));
    }
}

impl<'a> Flatten<'a> for AssignExpNode<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let src = self.exp.flatten(program, procedure);
        let dest = self.lval.flatten(program, procedure);
        procedure.quads.push(quad(Quad::Assign(AssignQuad { src, dest: dest.clone() })));
        dest
    }
}

impl<'a> Flatten<'a> for UnaryExpNode<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let exp_type = self.type_check(SymbolType::Void).unwrap();
        let size = exp_type.size();
        let temp = procedure.get_temp_opd(size);
        let ret = temp.clone();
        let my_quad = match &self.op {
            ast::UnaryOp::Neg => {
                let opcode = UnaryOp::Neg64;
                let src = if let SymbolType::Short = exp_type {
                    let temp_cast = procedure.get_temp_opd(size);
                    let cast_src = self.exp.flatten(program, procedure);
                    procedure.quads.push(quad(Quad::Assign(AssignQuad { dest: temp_cast.clone(), src: cast_src })));
                    temp_cast
                } else {
                    self.exp.flatten(program, procedure)
                };
                Quad::Unary(UnaryQuad { dest: temp, src, opcode })
            }
            ast::UnaryOp::Not => {
                Quad::Unary(UnaryQuad { dest: temp, src: self.exp.flatten(program, procedure), opcode: UnaryOp::Neg8 })
            }
            ast::UnaryOp::Ref => {
                let symbol = match &*self.exp {
                    ExpNode::LVal(LValNode::ID(IDNode { symbol, .. })) => symbol.as_ref().unwrap(),
                    _ => unreachable!() // only IDs can be ref'd
                };
                Quad::Assign(AssignQuad { dest: temp, src: Operand::AddrOperand(AddrOperandStruct { symbol: Rc::clone(&symbol) }) })
            }
        };
        procedure.quads.push(quad(my_quad));
        ret
    }
}

impl<'a> Flatten<'a> for BinaryExpNode<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let size = self.type_check(SymbolType::Void).unwrap().size();
        let temp = procedure.get_temp_opd(size);
        let ret = temp.clone();
        let opcode = match &self.op {
            ast::BinaryOperator::Plus => match size {
                8 => BinaryOp::Add8,
                64 => BinaryOp::Add64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::Minus => match size {
                8 => BinaryOp::Sub8,
                64 => BinaryOp::Sub64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::Divide => match size {
                8 => BinaryOp::Div8,
                64 => BinaryOp::Div64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::Times => match size {
                8 => BinaryOp::Mult8,
                64 => BinaryOp::Mult64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::Equals => match size {
                8 => BinaryOp::Eq8,
                64 => BinaryOp::Eq64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::NotEquals => match size {
                8 => BinaryOp::Neq8,
                64 => BinaryOp::Neq64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::Less => match size {
                8 => BinaryOp::Lt8,
                64 => BinaryOp::Lt64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::Greater => match size {
                8 => BinaryOp::Gt8,
                64 => BinaryOp::Gt64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::LessEq => match size {
                8 => BinaryOp::Lte8,
                64 => BinaryOp::Lte64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::GreaterEq => match size {
                8 => BinaryOp::Gte8,
                64 => BinaryOp::Gte64,
                _ => unreachable!(),
            }
            ast::BinaryOperator::And => match size {
                8 => BinaryOp::And8,
                _ => unreachable!(),
            }
            ast::BinaryOperator::Or => match size {
                8 => BinaryOp::Or8,
                _ => unreachable!(),
            }
        }; // whew
        // if one is a short and one is an int, must create a tmp to store the short as an int
        let (lhs, rhs) = match (self.lhs.type_check(SymbolType::Void).unwrap(), self.rhs.type_check(SymbolType::Void).unwrap()) {
            (i @ SymbolType::Int, SymbolType::Short) => {
                let temp = procedure.get_temp_opd(i.size());
                let src = self.rhs.flatten(program, procedure);
                procedure.quads.push(quad(Quad::Assign(AssignQuad { dest: temp.clone(), src })));
                (self.lhs.flatten(program, procedure), temp)
            }
            (SymbolType::Short, i @ SymbolType::Int) => {
                let temp = procedure.get_temp_opd(i.size());
                let src = self.lhs.flatten(program, procedure);
                procedure.quads.push(quad(Quad::Assign(AssignQuad { dest: temp.clone(), src })));
                (temp, self.rhs.flatten(program, procedure))
            }
            _ => (self.lhs.flatten(program, procedure), self.rhs.flatten(program, procedure))

        };
        let quad = quad(Quad::Binary(BinaryQuad { dest: temp, lhs, rhs, opcode }));
        procedure.quads.push(quad);
        ret
    }
}

impl<'a> Flatten<'a> for CallExpNode<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let size = self.type_check(SymbolType::Void).unwrap().size();
        let temp = Operand::TempOperand(procedure.get_temp(size));
        let ret = temp.clone();
        // setargs
        for (idx, arg) in self.args.iter().enumerate() {
            let arg_temp = arg.flatten(program, procedure);
            procedure.quads.push(quad(Quad::SetArg(SetArgQuad { idx, src: arg_temp })));
        }
        // call
        procedure.quads.push(quad(Quad::Call(CallQuad { func: Rc::clone(self.id.symbol.as_ref().unwrap()) })));
        // getret
        procedure.quads.push(quad(Quad::GetRet(GetRetQuad { dest: temp })));
        // return the temp variable we gotret into
        ret
    }
}

impl<'a> Flatten<'a> for IDNode<'a> {
    fn flatten(&self, _program: &mut IRProgram<'a>, _procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let symbol = Rc::clone(self.symbol.as_ref().unwrap());
        Operand::SymbolOperand(SymbolOperandStruct { symbol })
    }
}

impl<'a> Flatten<'a> for DerefNode<'a> {
    fn flatten(&self, _program: &mut IRProgram<'a>, _procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let symbol = Rc::clone(self.id.symbol.as_ref().unwrap());
        Operand::DerefOperand(DerefOperandStruct { symbol })
    }
}

impl<'a> Flatten<'a> for TrueNode {
    fn flatten(&self, _program: &mut IRProgram<'a>, _procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        Operand::LitOperand(LitOperandStruct { value: 1, width: 8 })
    }
}

impl<'a> Flatten<'a> for FalseNode {
    fn flatten(&self, _program: &mut IRProgram<'a>, _procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        Operand::LitOperand(LitOperandStruct { value: 0, width: 8 })
    }
}

impl<'a> Flatten<'a> for IntLitNode {
    fn flatten(&self, _program: &mut IRProgram<'a>, _procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        Operand::LitOperand(LitOperandStruct { value: self.val, width: 64 })
    }
}

impl<'a> Flatten<'a> for ShortLitNode {
    fn flatten(&self, _program: &mut IRProgram<'a>, _procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        Operand::LitOperand(LitOperandStruct { value: self.val as u32, width: 8 })
    }
}

impl<'a> Flatten<'a> for StrLitNode<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, _procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        program.make_str_lit_opd(self.val)
    }
}

impl<'a> IRProgram<'a> {
    fn from(tree: &ProgramNode<'a>) -> IRProgram<'a> {
        let mut program = IRProgram { globals: Vec::new(), procedures: Vec::new(), strings: Vec::new(), label_num: 0, string_num: 0 };
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
            global_string.push_str(&format!("{}\n", g.to_loc_str()));
        }
        for s in &self.strings {
            global_string.push_str(&format!("{} {}\n", s.to_loc_string(), s.value))
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

    fn get_label(&mut self) -> Label {
        let num = self.label_num;
        self.label_num += 1;
        Label(format!("lbl_{}", num))
    }

    fn make_str_lit(&mut self, value: &'a str) -> StringOperandStruct<'a> {
        let id = self.string_num;
        self.string_num += 1;
        let ret = StringOperandStruct { value, id };
        self.strings.push(ret.clone());
        ret
    }

    fn make_str_lit_opd(&mut self, value: &'a str) -> Operand<'a> {
        Operand::StringOperand(self.make_str_lit(value))
    }

    fn make_procedure(&mut self, symbol: &Rc<Symbol<'a>>) -> IRProcedure<'a> {
        let return_label = self.get_label();
        IRProcedure {
            symbol: Rc::clone(symbol),
            formals: Vec::new(),
            locals: Vec::new(),
            temps: Vec::new(),
            quads: Vec::new(),
            return_label,
            temp_num: 0,
        }
    }
}

impl<'a> IRProcedure<'a> {
    fn header_string(&self) -> String {
        let mut vars_string = String::new();
        for formal in &self.formals {
            vars_string.push_str(&format!("{} (formal arg of {} bytes)\n", formal.to_loc_str(), formal.width()/8));
        }
        for local in &self.locals {
            vars_string.push_str(&format!("{} (local var of {} bytes)\n", local.to_loc_str(), local.width()/8));
        }
        for temp in &self.temps {
            vars_string.push_str(&format!("{} (tmp var of {} bytes)\n", temp.to_loc_string(), temp.width/8));
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
    fn get_temp(&mut self, width: usize) -> TempOperandStruct {
        let num = self.temp_num;
        self.temp_num += 1;
        let ret = TempOperandStruct { id: num, width };
        self.temps.push(ret.clone());
        ret
    }
    fn get_temp_opd(&mut self, width: usize) -> Operand<'a> {
        Operand::TempOperand(self.get_temp(width))
    }
}

impl<'a> SymbolOperandStruct<'a> {
    fn from(symbol: &Rc<Symbol<'a>>) -> SymbolOperandStruct<'a> {
        SymbolOperandStruct { symbol: Rc::clone(symbol) }
    }
    fn to_loc_str(&self) -> &'a str {
        self.symbol.name
    }

    fn width(&self) -> usize {
        self.symbol.typ.size()
    }
}

impl<'a> AddrOperandStruct<'a> {
    fn width(&self) -> usize {
        64
    }
}

impl<'a> DerefOperandStruct<'a> {
    fn width(&self) -> usize {
        self.symbol.typ.size()
    }
}

impl<'a> StringOperandStruct<'a> {
    fn to_loc_string(&self) -> String {
        format!("str_{}", self.id)
    }
}

impl TempOperandStruct {
    fn to_loc_string(&self) -> String {
        format!("tmp{}", self.id)
    }
}

impl<'a> LabeledQuad<'a> {
    fn to_string(&self) -> String {
        let lbl = if self.label.0 == "" {
            "\t\t\t".to_string()
        } else {
            let spaces = "\t".repeat(3 - (self.label.0.len()/4));
            format!("{}: {}", self.label.0, spaces)
        };
        format!("{}{}", lbl, self.quad.to_string())
    }
}

impl<'a> ToString for AssignQuad<'a> {
    fn to_string(&self) -> String {
        format!("{} := {}", self.dest.to_string(), self.src.to_string())
    }
}

impl<'a> ToString for UnaryQuad<'a> {
    fn to_string(&self) -> String {
        format!("{} := {} {}", self.dest.to_string(), self.opcode.to_str(), self.src.to_string())
    }
}

impl<'a> ToString for BinaryQuad<'a> {
    fn to_string(&self) -> String {
        format!("{} := {} {} {}", self.dest.to_string(), self.lhs.to_string(), self.opcode.to_str(), self.rhs.to_string())
    }
}

impl ToString for UnconditionalJumpQuad {
    fn to_string(&self) -> String {
        format!("goto {}", self.label.to_string())
    }
}

impl<'a> ToString for ConditionalJumpQuad<'a> {
    fn to_string(&self) -> String {
        format!("ifz {} goto {}", self.condition_src.to_string(), self.label.to_string())
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
        format!("getarg {} {}", self.idx+1, self.dest.to_string())
    }
}

impl<'a> ToString for SetRetQuad<'a> {
    fn to_string(&self) -> String {
        format!("setret {}", self.src.to_string())
    }
}

impl<'a> ToString for CallQuad<'a> {
    fn to_string(&self) -> String {
        format!("call {}", self.func.name)
    }
}

impl<'a> ToString for SetArgQuad<'a> {
    fn to_string(&self) -> String {
        format!("setarg {} {}", self.idx+1, self.src.to_string())
    }
}

impl<'a> ToString for GetRetQuad<'a> {
    fn to_string(&self) -> String {
        format!("getret {}", self.dest.to_string())
    }
}

impl<'a> ToString for ReceiveQuad<'a> {
    fn to_string(&self) -> String {
        format!("receive {}", self.dest.to_string())
    }
}

impl<'a> ToString for ReportQuad<'a> {
    fn to_string(&self) -> String {
        format!("report {}", self.src.to_string())
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
        format!("[{}]", self.to_loc_str())
    }
}

impl ToString for LitOperandStruct {
    fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}

impl<'a> ToString for AddrOperandStruct<'a> {
    fn to_string(&self) -> String {
        // without []
        self.symbol.name.to_string()
    }
}

impl<'a> ToString for DerefOperandStruct<'a> {
    fn to_string(&self) -> String {
        // with double []
        format!("[[{}]]", self.symbol.name.to_string())
    }
}

impl ToString for TempOperandStruct {
    fn to_string(&self) -> String {
        format!("[{}]", self.to_loc_string())
    }
}

impl<'a> ToString for StringOperandStruct<'a> {
    fn to_string(&self) -> String {
        self.to_loc_string()
    }
}

impl ToString for Label {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

impl BinaryOp {
    fn to_str(&self) -> &'static str {
        match self {
            BinaryOp::Add64 => "ADD64",
            BinaryOp::Add8 => "ADD8",
            BinaryOp::Sub64 => "SUB64",
            BinaryOp::Sub8 => "SUB8",
            BinaryOp::Div64 => "DIV64",
            BinaryOp::Div8 => "DIV8",
            BinaryOp::Mult64 => "MULT64",
            BinaryOp::Mult8 => "MULT8",
            BinaryOp::Eq64 => "EQ64",
            BinaryOp::Eq8 => "EQ8",
            BinaryOp::Neq64 => "NEQ64",
            BinaryOp::Neq8 => "NEQ8",
            BinaryOp::Lt64 => "LT64",
            BinaryOp::Lt8 => "LT8",
            BinaryOp::Gt64 => "GT64",
            BinaryOp::Gt8 => "GT8",
            BinaryOp::Lte64 => "LTE64",
            BinaryOp::Lte8 => "LTE8",
            BinaryOp::Gte64 => "GTE64",
            BinaryOp::Gte8 => "GTE8",
            BinaryOp::And8 => "AND8",
            BinaryOp::Or8 => "OR8",
        }
    }
}

impl UnaryOp {
    fn to_str(&self) -> &'static str {
        match self {
            UnaryOp::Neg8 => "NEG8",
            UnaryOp::Neg64 => "NEG64",
        }
    }
}

fn quad(quad: Quad) -> LabeledQuad {
    LabeledQuad { label: Label(String::new()), quad }
}