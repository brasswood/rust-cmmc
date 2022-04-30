// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use crate::ast::{self, *};
use crate::type_check::TypeCheck;
use enum_dispatch::enum_dispatch;
use std::fs;
use std::io::Write;
use std::rc::Rc;

use crate::name::symbol::SymbolType;
use crate::{ast::ProgramNode, name::symbol::Symbol};

#[enum_dispatch(Quad)]
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
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>);
}

pub fn write_3ac(tree: &mut ProgramNode, outfile: &mut fs::File) {
    let ir = IRProgram::from(tree);
    write!(outfile, "{}", ir.to_string()).unwrap();
}

pub struct IRProgram<'a> {
    pub globals: Vec<SymbolOperandStruct<'a>>,
    pub strings: Vec<StringOperandStruct<'a>>,
    pub procedures: Vec<IRProcedure<'a>>,
    label_num: usize,
    string_num: usize,
}

pub struct IRProcedure<'a> {
    pub symbol: Rc<Symbol<'a>>,
    pub formals: Vec<SymbolOperandStruct<'a>>,
    pub locals: Vec<SymbolOperandStruct<'a>>,
    pub temps: Vec<TempOperandStruct>,
    pub quads: Vec<LabeledQuad<'a>>,
    pub return_label: Label,
    temp_num: usize,
    pub formal_return_type: SymbolType,
}

#[enum_dispatch(ToString)]
#[derive(Clone)]
pub enum Operand<'a> {
    LitOperand(LitOperandStruct),
    SymbolOperand(SymbolOperandStruct<'a>),
    AddrOperand(AddrOperandStruct<'a>),
    DerefOperand(DerefOperandStruct<'a>),
    TempOperand(TempOperandStruct),
    StringOperand(StringOperandStruct<'a>),
}

#[derive(Clone)]
pub struct SymbolOperandStruct<'a> {
    pub symbol: Rc<Symbol<'a>>,
}

#[derive(Clone)]
pub struct LitOperandStruct {
    pub value: u32,
    pub size: usize,
}

#[derive(Clone)]
pub struct AddrOperandStruct<'a> {
    pub symbol: Rc<Symbol<'a>>,
}

#[derive(Clone)]
pub struct DerefOperandStruct<'a> {
    pub symbol: Rc<Symbol<'a>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TempOperandStruct {
    pub id: usize,
    pub size: usize,
}

#[derive(Clone)]
pub struct StringOperandStruct<'a> {
    pub id: usize,
    pub value: &'a str,
}

#[derive(Clone)]
pub struct Label(pub String);

pub struct LabeledQuad<'a> {
    pub label: Label,
    pub quad: Quad<'a>,
}

#[enum_dispatch]
pub enum Quad<'a> {
    Assign(AssignQuad<'a>),
    ShortToInt(ShortToIntQuad<'a>),
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

pub struct AssignQuad<'a> {
    pub dest: Operand<'a>,
    pub src: Operand<'a>,
}

pub struct ShortToIntQuad<'a> {
    pub dest: Operand<'a>,
    pub src: Operand<'a>,
}
pub struct UnaryQuad<'a> {
    pub dest: Operand<'a>,
    pub src: Operand<'a>,
    pub opcode: UnaryOp,
}

pub enum UnaryOp {
    Neg64,
    Neg8,
    Not8,
}

pub struct BinaryQuad<'a> {
    pub dest: Operand<'a>,
    pub lhs: Operand<'a>,
    pub rhs: Operand<'a>,
    pub opcode: BinaryOp,
}

pub enum BinaryOp {
    Add64,
    Add8,
    Sub64,
    Sub8,
    Div64,
    Div8,
    Mult64,
    Mult8,
    Eq64,
    Eq8,
    Neq64,
    Neq8,
    Lt64,
    Lt8,
    Gt64,
    Gt8,
    Lte64,
    Lte8,
    Gte64,
    Gte8,
    And8,
    Or8,
}

pub struct UnconditionalJumpQuad {
    pub label: Label,
}

pub struct ConditionalJumpQuad<'a> {
    pub condition_src: Operand<'a>,
    pub label: Label,
}

pub struct EnterQuad<'a> {
    pub func: Rc<Symbol<'a>>,
}

pub struct LeaveQuad<'a> {
    pub func: Rc<Symbol<'a>>,
}

pub struct GetArgQuad<'a> {
    pub idx: usize,
    pub dest: Operand<'a>,
}

pub struct SetRetQuad<'a> {
    pub src: Operand<'a>,
}

pub struct CallQuad<'a> {
    pub func: Rc<Symbol<'a>>,
}

pub struct SetArgQuad<'a> {
    pub idx: usize,
    pub src: Operand<'a>,
}

pub struct GetRetQuad<'a> {
    pub dest: Operand<'a>,
}

pub struct ReceiveQuad<'a> {
    pub dest: Operand<'a>,
    pub typ: SymbolType,
}

pub struct ReportQuad<'a> {
    pub src: Operand<'a>,
    pub typ: SymbolType,
}

pub struct NopQuad;

impl SymbolType {
    pub fn size(&self) -> usize {
        // in bits
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
        let symbol = self
            .symbol
            .as_ref()
            .expect("Symbol not found. Did you do type analysis?");
        program.globals.push(SymbolOperandStruct::from(symbol));
    }

    fn emit_3ac_local(&mut self, procedure: &mut IRProcedure<'a>) {
        let symbol = self
            .symbol
            .as_ref()
            .expect("Symbol not found. Did you do type analysis?");
            
        procedure.locals.push(SymbolOperandStruct::from(symbol));
    }
}

impl<'a> FnDeclNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>) {
        let symbol = self
            .symbol
            .as_ref()
            .expect("Symbol not found. Did you do type analysis?");
        let mut proc = program.make_procedure(symbol);
        // first add the enter fn quad
        proc.push_quad(LabeledQuad {
            label: if self.symbol.as_ref().unwrap().name == "main" {
                Label("main".to_string())
            } else {
                Label(format!(
                    "fun_{}",
                    self.symbol.as_ref().unwrap().name.clone()
                ))
            },
            quad: Quad::Enter(EnterQuad {
                func: Rc::clone(symbol),
            }),
        });
        // getargs
        for (idx, formal_decl) in self.formals.iter().enumerate() {
            formal_decl.emit_3ac(idx, &mut proc)
        }
        // body
        for stmt in &mut self.stmts {
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
        proc.push_quad(LabeledQuad {
            label: proc.return_label.clone(),
            quad: Quad::Leave(LeaveQuad {
                func: Rc::clone(symbol),
            }),
        });
        // add to the program (yeah it's sketchy but it's one way to get around Rust not allowing multiple mutable borrows at a time)
        program.procedures.push(proc);
        // done
    }
}

impl<'a> FormalDeclNode<'a> {
    fn emit_3ac(&self, idx: usize, procedure: &mut IRProcedure<'a>) {
        let sym = SymbolOperandStruct::from(
            self.symbol
                .as_ref()
                .expect("Symbol not found. Did you do type analysis?"),
        );
        procedure.formals.push(sym.clone());
        procedure.push_quad(quad(Quad::GetArg(GetArgQuad {
            idx,
            dest: Operand::SymbolOperand(sym),
        })));
    }
}

impl<'a> Emit3AC<'a> for DeclNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        match self {
            DeclNode::VarDecl(d) => d.emit_3ac_local(procedure),
            _ => unreachable!(),
        }
    }
}

impl<'a> Emit3AC<'a> for AssignStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        // exactly flatten, we just don't return the result
        self.exp.flatten(program, procedure);
    }
}

impl<'a> Emit3AC<'a> for CallStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        // like CallExpNode.flatten(), but we don't get a result.
        // setargs
        for (idx, arg) in self.exp.args.iter().enumerate() {
            let arg_temp = arg.flatten(program, procedure);
            procedure.push_quad(quad(Quad::SetArg(SetArgQuad { idx, src: arg_temp })));
        }
        // call
        procedure.push_quad(quad(Quad::Call(CallQuad {
            func: Rc::clone(self.exp.id.symbol.as_ref().unwrap()),
        })));
    }
}

impl<'a> Emit3AC<'a> for IfStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        // put the condition into a temp
        let cond_temp = self.exp.flatten(program, procedure);
        let label_over = program.get_label();
        procedure.push_quad(quad(Quad::ConditionalJump(ConditionalJumpQuad {
            condition_src: cond_temp,
            label: label_over.clone(),
        })));
        for s in &mut self.stmts {
            s.emit_3ac(program, procedure);
        }
        procedure.push_quad(LabeledQuad {
            label: label_over,
            quad: Quad::Nop(NopQuad),
        });
    }
}

impl<'a> Emit3AC<'a> for IfElseStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let cond_temp = self.exp.flatten(program, procedure);
        let label_else = program.get_label();
        let label_skip_else = program.get_label();
        procedure.push_quad(quad(Quad::ConditionalJump(ConditionalJumpQuad {
            condition_src: cond_temp,
            label: label_else.clone(),
        })));
        for s in &mut self.true_stmts {
            s.emit_3ac(program, procedure);
        }
        procedure.push_quad(quad(Quad::UnconditionalJump(UnconditionalJumpQuad {
            label: label_skip_else.clone(),
        })));
        procedure.push_quad(LabeledQuad {
            label: label_else,
            quad: Quad::Nop(NopQuad),
        });
        for s in &mut self.else_stmts {
            s.emit_3ac(program, procedure);
        }
        procedure.push_quad(LabeledQuad {
            label: label_skip_else,
            quad: Quad::Nop(NopQuad),
        });
    }
}

impl<'a> Emit3AC<'a> for WhileStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let loop_label = program.get_label();
        // put a labeled nop before getting the condition temp. That way when we jump back to the head, we get the condition
        procedure.push_quad(LabeledQuad {
            label: loop_label.clone(),
            quad: Quad::Nop(NopQuad),
        });
        let cond_temp = self.exp.flatten(program, procedure);
        let loop_end_label = program.get_label();
        procedure.push_quad(quad(Quad::ConditionalJump(ConditionalJumpQuad {
            condition_src: cond_temp,
            label: loop_end_label.clone(),
        })));
        // push the loop body
        for s in &mut self.stmts {
            s.emit_3ac(program, procedure);
        }
        // end of loop, do an unconditional jump to head
        procedure.push_quad(quad(Quad::UnconditionalJump(UnconditionalJumpQuad {
            label: loop_label,
        })));
        // after loop condition fails, nop and carry on
        procedure.push_quad(LabeledQuad {
            label: loop_end_label,
            quad: Quad::Nop(NopQuad),
        });
    }
}

impl<'a> Emit3AC<'a> for PostIncStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let my_opd = self.lval.flatten(program, procedure);
        let typ = self.lval.type_check(SymbolType::Void).unwrap();
        let (opcode, size) = match typ {
            SymbolType::Int | SymbolType::Ptr(_) => (BinaryOp::Add64, 64),
            SymbolType::Short => (BinaryOp::Add8, 8),
            _ => unreachable!(),
        };
        procedure.push_quad(quad(Quad::Binary(BinaryQuad {
            dest: my_opd.clone(),
            lhs: my_opd.clone(),
            opcode,
            rhs: Operand::LitOperand(LitOperandStruct { value: 1, size }),
        })));
    }
}

impl<'a> Emit3AC<'a> for PostDecStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let my_opd = self.lval.flatten(program, procedure);
        let typ = self.lval.type_check(SymbolType::Void).unwrap();
        let (opcode, size) = match typ {
            SymbolType::Int | SymbolType::Ptr(_) => (BinaryOp::Sub64, 64),
            SymbolType::Short => (BinaryOp::Sub8, 8),
            _ => unreachable!(),
        };
        procedure.push_quad(quad(Quad::Binary(BinaryQuad {
            dest: my_opd.clone(),
            lhs: my_opd.clone(),
            opcode,
            rhs: Operand::LitOperand(LitOperandStruct { value: 1, size }),
        })));
    }
}

impl<'a> Emit3AC<'a> for ReadStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let typ = self.lval.type_check(SymbolType::Void).unwrap();
        let dest = self.lval.flatten(program, procedure);
        procedure.push_quad(quad(Quad::Receive(ReceiveQuad { dest, typ })));
    }
}

impl<'a> Emit3AC<'a> for WriteStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        let typ = self.exp.type_check(SymbolType::Void).unwrap();
        let src = self.exp.flatten(program, procedure);
        procedure.push_quad(quad(Quad::Report(ReportQuad { src, typ })));
    }
}

impl<'a> Emit3AC<'a> for ReturnStmtNode<'a> {
    fn emit_3ac(&mut self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) {
        match &self.exp {
            Some(exp) => {
                let exp_type = exp.type_check(SymbolType::Void).unwrap();
                let last_opd = match (&procedure.formal_return_type, exp_type) {
                    (t1, t2) if *t1 == t2 => exp.flatten(program, procedure),
                    (SymbolType::Int, SymbolType::Short) => {
                        let exp_opd = exp.flatten(program, procedure);
                        let cast_opd = procedure.get_temp_opd(SymbolType::Int.size());
                        procedure.push_quad(quad(Quad::ShortToInt(ShortToIntQuad {
                            dest: cast_opd.clone(),
                            src: exp_opd,
                        })));
                        cast_opd
                    }
                    _ => unreachable!(),
                };
                procedure.push_quad(quad(Quad::SetRet(SetRetQuad { src: last_opd })));
            }
            None => (),
        }
        procedure.push_quad(quad(Quad::UnconditionalJump(UnconditionalJumpQuad {
            label: procedure.return_label.clone(),
        })));
    }
}

impl<'a> Flatten<'a> for AssignExpNode<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let src = self.exp.flatten(program, procedure);
        let dest = self.lval.flatten(program, procedure);
        procedure.push_quad(quad(Quad::Assign(AssignQuad {
            src,
            dest: dest.clone(),
        })));
        dest
    }
}

impl<'a> Flatten<'a> for UnaryExpNode<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let exp_type = self.type_check(SymbolType::Void).unwrap();
        let size = exp_type.size();
        let temp;
        let my_quad = match &self.op {
            ast::UnaryOp::Neg => {
                let opcode = match exp_type {
                    SymbolType::Int => UnaryOp::Neg64,
                    SymbolType::Short => UnaryOp::Neg8,
                    _ => unreachable!(),
                };
                let src = self.exp.flatten(program, procedure);
                temp = procedure.get_temp_opd(size);
                Quad::Unary(UnaryQuad {
                    dest: temp.clone(),
                    src,
                    opcode,
                })
            }
            ast::UnaryOp::Not => {
                let src = self.exp.flatten(program, procedure);
                temp = procedure.get_temp_opd(size);
                Quad::Unary(UnaryQuad {
                    dest: temp.clone(),
                    src,
                    opcode: UnaryOp::Not8,
                })
            }
            ast::UnaryOp::Ref => {
                let symbol = match &*self.exp {
                    ExpNode::LVal(LValNode::ID(IDNode { symbol, .. })) => symbol.as_ref().unwrap(),
                    _ => unreachable!(), // only IDs can be ref'd
                };
                temp = procedure.get_temp_opd(size);
                Quad::Assign(AssignQuad {
                    dest: temp.clone(),
                    src: Operand::AddrOperand(AddrOperandStruct {
                        symbol: Rc::clone(&symbol),
                    }),
                })
            }
        };
        procedure.push_quad(quad(my_quad));
        temp
    }
}

impl<'a> Flatten<'a> for BinaryExpNode<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let size = self.type_check(SymbolType::Void).unwrap().size();
        let opcode = match &self.op {
            ast::BinaryOperator::Plus => match size {
                8 => BinaryOp::Add8,
                64 => BinaryOp::Add64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::Minus => match size {
                8 => BinaryOp::Sub8,
                64 => BinaryOp::Sub64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::Divide => match size {
                8 => BinaryOp::Div8,
                64 => BinaryOp::Div64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::Times => match size {
                8 => BinaryOp::Mult8,
                64 => BinaryOp::Mult64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::Equals => match size {
                8 => BinaryOp::Eq8,
                64 => BinaryOp::Eq64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::NotEquals => match size {
                8 => BinaryOp::Neq8,
                64 => BinaryOp::Neq64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::Less => match size {
                8 => BinaryOp::Lt8,
                64 => BinaryOp::Lt64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::Greater => match size {
                8 => BinaryOp::Gt8,
                64 => BinaryOp::Gt64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::LessEq => match size {
                8 => BinaryOp::Lte8,
                64 => BinaryOp::Lte64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::GreaterEq => match size {
                8 => BinaryOp::Gte8,
                64 => BinaryOp::Gte64,
                _ => unreachable!(),
            },
            ast::BinaryOperator::And => match size {
                8 => BinaryOp::And8,
                _ => unreachable!(),
            },
            ast::BinaryOperator::Or => match size {
                8 => BinaryOp::Or8,
                _ => unreachable!(),
            },
        }; // whew
           // if one is a short and one is an int, must create a tmp to store the short as an int
        let (lhs, rhs) = match (
            self.lhs.type_check(SymbolType::Void).unwrap(),
            self.rhs.type_check(SymbolType::Void).unwrap(),
        ) {
            (i @ SymbolType::Int, SymbolType::Short) => {
                let temp = procedure.get_temp_opd(i.size());
                let src = self.rhs.flatten(program, procedure);
                procedure.push_quad(quad(Quad::ShortToInt(ShortToIntQuad {
                    dest: temp.clone(),
                    src,
                })));
                (self.lhs.flatten(program, procedure), temp)
            }
            (SymbolType::Short, i @ SymbolType::Int) => {
                let temp = procedure.get_temp_opd(i.size());
                let src = self.lhs.flatten(program, procedure);
                procedure.push_quad(quad(Quad::ShortToInt(ShortToIntQuad {
                    dest: temp.clone(),
                    src,
                })));
                (temp, self.rhs.flatten(program, procedure))
            }
            _ => (
                self.lhs.flatten(program, procedure),
                self.rhs.flatten(program, procedure),
            ),
        };
        let temp = procedure.get_temp_opd(size);
        let quad = quad(Quad::Binary(BinaryQuad {
            dest: temp.clone(),
            lhs,
            rhs,
            opcode,
        }));
        procedure.push_quad(quad);
        temp
    }
}

impl<'a> Flatten<'a> for CallExpNode<'a> {
    fn flatten(&self, program: &mut IRProgram<'a>, procedure: &mut IRProcedure<'a>) -> Operand<'a> {
        let formal_types = match self.id.type_check(SymbolType::Void).unwrap() {
            SymbolType::Fn { args, .. } => args,
            _ => unreachable!(),
        };
        // setargs
        for (idx, (arg, form_typ)) in self.args.iter().zip(formal_types).enumerate() {
            let arg_cast_temp = {
                let arg_typ = arg.type_check(SymbolType::Void).unwrap();
                let arg_temp = arg.flatten(program, procedure);
                match (form_typ, arg_typ) {
                    (t1, t2) if t1 == t2 => arg_temp,
                    (SymbolType::Int, SymbolType::Short) => {
                        let t = procedure.get_temp_opd(SymbolType::Int.size());
                        procedure.push_quad(quad(Quad::ShortToInt(ShortToIntQuad {
                            dest: t.clone(),
                            src: arg_temp,
                        })));
                        t
                    }
                    _ => unreachable!(), // shouldn't have made it through type analysis
                }
            };
            procedure.push_quad(quad(Quad::SetArg(SetArgQuad {
                idx,
                src: arg_cast_temp,
            })));
        }
        // call
        procedure.push_quad(quad(Quad::Call(CallQuad {
            func: Rc::clone(self.id.symbol.as_ref().unwrap()),
        })));
        // getret
        let size = self.type_check(SymbolType::Void).unwrap().size();
        let temp = Operand::TempOperand(procedure.get_temp(size));
        procedure.push_quad(quad(Quad::GetRet(GetRetQuad { dest: temp.clone() })));
        // return the temp variable we gotret into
        temp
    }
}

impl<'a> Flatten<'a> for IDNode<'a> {
    fn flatten(
        &self,
        _program: &mut IRProgram<'a>,
        _procedure: &mut IRProcedure<'a>,
    ) -> Operand<'a> {
        let symbol = Rc::clone(self.symbol.as_ref().unwrap());
        Operand::SymbolOperand(SymbolOperandStruct { symbol })
    }
}

impl<'a> Flatten<'a> for DerefNode<'a> {
    fn flatten(
        &self,
        _program: &mut IRProgram<'a>,
        _procedure: &mut IRProcedure<'a>,
    ) -> Operand<'a> {
        let symbol = Rc::clone(self.id.symbol.as_ref().unwrap());
        Operand::DerefOperand(DerefOperandStruct { symbol })
    }
}

impl<'a> Flatten<'a> for TrueNode {
    fn flatten(
        &self,
        _program: &mut IRProgram<'a>,
        _procedure: &mut IRProcedure<'a>,
    ) -> Operand<'a> {
        Operand::LitOperand(LitOperandStruct { value: 1, size: 8 })
    }
}

impl<'a> Flatten<'a> for FalseNode {
    fn flatten(
        &self,
        _program: &mut IRProgram<'a>,
        _procedure: &mut IRProcedure<'a>,
    ) -> Operand<'a> {
        Operand::LitOperand(LitOperandStruct { value: 0, size: 8 })
    }
}

impl<'a> Flatten<'a> for IntLitNode {
    fn flatten(
        &self,
        _program: &mut IRProgram<'a>,
        _procedure: &mut IRProcedure<'a>,
    ) -> Operand<'a> {
        Operand::LitOperand(LitOperandStruct {
            value: self.val,
            size: 64,
        })
    }
}

impl<'a> Flatten<'a> for ShortLitNode {
    fn flatten(
        &self,
        _program: &mut IRProgram<'a>,
        _procedure: &mut IRProcedure<'a>,
    ) -> Operand<'a> {
        Operand::LitOperand(LitOperandStruct {
            value: self.val as u32,
            size: 8,
        })
    }
}

impl<'a> Flatten<'a> for StrLitNode<'a> {
    fn flatten(
        &self,
        program: &mut IRProgram<'a>,
        _procedure: &mut IRProcedure<'a>,
    ) -> Operand<'a> {
        program.make_str_lit_opd(self.val)
    }
}

impl<'a> IRProgram<'a> {
    pub fn from(tree: &mut ProgramNode<'a>) -> IRProgram<'a> {
        let mut program = IRProgram {
            globals: Vec::new(),
            procedures: Vec::new(),
            strings: Vec::new(),
            label_num: 0,
            string_num: 0,
        };
        for decl in &mut tree.0 {
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
            global_string, proc_strings,
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
        let formal_return_type = if let SymbolType::Fn { ret, .. } = &symbol.typ {
            *ret.clone()
        } else {
            unreachable!()
        };
        IRProcedure {
            symbol: Rc::clone(symbol),
            formals: Vec::new(),
            locals: Vec::new(),
            temps: Vec::new(),
            quads: Vec::new(),
            return_label,
            temp_num: 0,
            formal_return_type,
        }
    }
}

impl<'a> IRProcedure<'a> {
    fn header_string(&self) -> String {
        let mut vars_string = String::new();
        for formal in &self.formals {
            vars_string.push_str(&format!(
                "{} (formal arg of {} bytes)\n",
                formal.to_loc_str(),
                formal.size() / 8
            ));
        }
        for local in &self.locals {
            vars_string.push_str(&format!(
                "{} (local var of {} bytes)\n",
                local.to_loc_str(),
                local.size() / 8
            ));
        }
        for temp in &self.temps {
            vars_string.push_str(&format!(
                "{} (tmp var of {} bytes)\n",
                temp.to_loc_string(),
                temp.size / 8
            ));
        }
        format!(
            "[BEGIN {} LOCALS]\n\
            {}\
            [END {} LOCALS]\n",
            self.symbol.name, vars_string, self.symbol.name,
        )
    }
    fn to_string(&self) -> String {
        let mut ret = String::new();
        for quad in &self.quads {
            ret.push_str(&format!("{}\n", quad.to_string()));
        }
        ret
    }
    fn get_temp(&mut self, size: usize) -> TempOperandStruct {
        let num = self.temp_num;
        self.temp_num += 1;
        let ret = TempOperandStruct { id: num, size };
        self.temps.push(ret.clone());
        ret
    }
    fn get_temp_opd(&mut self, width: usize) -> Operand<'a> {
        Operand::TempOperand(self.get_temp(width))
    }
    fn push_quad(&mut self, quad: LabeledQuad<'a>) {
        self.quads.push(quad);
    }
}

impl<'a> Operand<'a> {
    pub fn size(&self) -> usize {
        match self {
            Operand::LitOperand(l) => l.size(),
            Operand::SymbolOperand(s) => s.size(),
            Operand::AddrOperand(a) => a.size(),
            Operand::DerefOperand(d) => d.size(),
            Operand::TempOperand(t) => t.size(),
            Operand::StringOperand(s) => s.size(),
        }
    }
}

impl<'a> SymbolOperandStruct<'a> {
    fn from(symbol: &Rc<Symbol<'a>>) -> SymbolOperandStruct<'a> {
        SymbolOperandStruct {
            symbol: Rc::clone(symbol),
        }
    }
    fn to_loc_str(&self) -> &'a str {
        self.symbol.name
    }

    pub fn size(&self) -> usize {
        self.symbol.typ.size()
    }
}

impl<'a> AddrOperandStruct<'a> {
    pub fn size(&self) -> usize {
        64
    }
}

impl<'a> DerefOperandStruct<'a> {
    pub fn size(&self) -> usize {
        self.symbol.typ.size()
    }
}

impl<'a> StringOperandStruct<'a> {
    fn to_loc_string(&self) -> String {
        format!("str_{}", self.id)
    }
    
    pub fn size(&self) -> usize {
        64
    }
}

impl TempOperandStruct {
    fn to_loc_string(&self) -> String {
        format!("tmp{}", self.id)
    }

    pub fn size(&self) -> usize {
        self.size
    }
}

impl LitOperandStruct {
    pub fn size(&self) -> usize {
        self.size
    }
}

impl<'a> LabeledQuad<'a> {
    fn to_string(&self) -> String {
        let lbl = if self.label.0 == "" {
            "\t".repeat(3).to_string()
        } else {
            let lbl_cnt = self.label.0.chars().count() + 2;
            let spaces = "\t".repeat(3 - std::cmp::min(3, lbl_cnt / 4));
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

impl<'a> ToString for ShortToIntQuad<'a> {
    fn to_string(&self) -> String {
        format!("{} := {}", self.dest.to_string(), self.src.to_string())
    }
}

impl<'a> ToString for UnaryQuad<'a> {
    fn to_string(&self) -> String {
        format!(
            "{} := {} {}",
            self.dest.to_string(),
            self.opcode.to_str(),
            self.src.to_string()
        )
    }
}

impl<'a> ToString for BinaryQuad<'a> {
    fn to_string(&self) -> String {
        format!(
            "{} := {} {} {}",
            self.dest.to_string(),
            self.lhs.to_string(),
            self.opcode.to_str(),
            self.rhs.to_string()
        )
    }
}

impl ToString for UnconditionalJumpQuad {
    fn to_string(&self) -> String {
        format!("goto {}", self.label.to_string())
    }
}

impl<'a> ToString for ConditionalJumpQuad<'a> {
    fn to_string(&self) -> String {
        format!(
            "ifz {} goto {}",
            self.condition_src.to_string(),
            self.label.to_string()
        )
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
        format!("getarg {} {}", self.idx + 1, self.dest.to_string())
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
        format!("setarg {} {}", self.idx + 1, self.src.to_string())
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
            UnaryOp::Neg64 => "NEG64",
            UnaryOp::Neg8 => "NEG8",
            UnaryOp::Not8 => "NOT8",
        }
    }
}

fn quad(quad: Quad) -> LabeledQuad {
    LabeledQuad {
        label: Label(String::new()),
        quad,
    }
}
