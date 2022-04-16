// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use std::vec::Vec;
use std::rc::Rc;
use std::boxed::Box;
use std::fmt::Debug;
use crate::name::symbol::{Symbol, SymbolTable, AsSymbol};
use crate::name::NameAnalysis;
use crate::type_check::TypeCheck;
use crate::name::symbol::SymbolType;
use enum_dispatch::enum_dispatch;
use crate::parse::Unparse;
use get_pos_derive::GetPos;

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub start: (usize, usize), // (line, col)
    pub end: (usize, usize),
}

#[enum_dispatch(ExpNode, StmtNode, DeclNode, LValNode)]
pub trait GetPos {
    fn get_pos(&self) -> Pos;
}

#[derive(Debug, Clone)]
pub struct ProgramNode<'a> (pub Vec<DeclNode<'a>>);

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum ExpNode<'a> {
    AssignExp(AssignExpNode<'a>),
    UnaryExp(UnaryExpNode<'a>),
    BinaryExp(BinaryExpNode<'a>),
    CallExp(CallExpNode<'a>),
    LVal(LValNode<'a>),
    True(TrueNode),
    False(FalseNode),
    IntLit(IntLitNode),
    ShortLit(ShortLitNode),
    StrLit(StrLitNode<'a>),
}

#[derive(Debug, Clone, GetPos)]
pub struct AssignExpNode<'a> {
    pub lval: LValNode<'a>,
    pub exp: Box<ExpNode<'a>>,
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct UnaryExpNode<'a> {
    pub op: UnaryOp,
    pub exp: Box<ExpNode<'a>>,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
}

#[derive(Debug, Clone, GetPos)]
pub struct BinaryExpNode<'a> {
    pub op: BinaryOperator,
    pub lhs: Box<ExpNode<'a>>,
    pub rhs: Box<ExpNode<'a>>,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    And,
    Divide,
    Equals,
    GreaterEq,
    Greater,
    LessEq,
    Less,
    Minus,
    NotEquals,
    Or,
    Plus,
    Times,
}

#[derive(Debug, Clone, GetPos)]
pub struct CallExpNode<'a> {
    pub id: IDNode<'a>,
    pub args: Vec<ExpNode<'a>>,
    pub pos: Pos,
}

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum LValNode<'a> {
    ID(IDNode<'a>),
    Deref(DerefNode<'a>),
}

#[derive(Debug, Clone, GetPos)]
pub struct IDNode<'a> {
    pub name: &'a str,
    pub symbol: Option<Rc<Symbol<'a>>>,
    pub pos: Pos,
}

// contains the id being derefed
#[derive(Debug, Clone, GetPos)]
pub struct DerefNode<'a> {
    pub id: IDNode<'a>,
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct TrueNode {
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct FalseNode {
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct IntLitNode {
    pub val: u32,
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct ShortLitNode {
    pub val: u8,
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct StrLitNode<'a> {
    pub val: &'a str,
    pub pos: Pos,
}

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum StmtNode<'a> {
    AssignStmt(AssignStmtNode<'a>),
    CallStmt(CallStmtNode<'a>),
    Decl(DeclNode<'a>),
    IfStmt(IfStmtNode<'a>),
    IfElseStmt(IfElseStmtNode<'a>),
    WhileStmt(WhileStmtNode<'a>),
    PostIncStmt(PostIncStmtNode<'a>),
    PostDecStmt(PostDecStmtNode<'a>),
    ReadStmt(ReadStmtNode<'a>),
    WriteStmt(WriteStmtNode<'a>),
    ReturnStmt(ReturnStmtNode<'a>),
}

// Contains the assign expression
#[derive(Debug, Clone, GetPos)]
pub struct AssignStmtNode<'a> {
    pub exp: AssignExpNode<'a>,
    pub pos: Pos,
}

// Contains the expression to call
#[derive(Debug, Clone, GetPos)]
pub struct CallStmtNode<'a> {
    pub exp: CallExpNode<'a>,
    pub pos: Pos,
}

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum DeclNode<'a> {
    FnDecl(FnDeclNode<'a>),
    VarDecl(VarDeclNode<'a>),
    FormalDecl(FormalDeclNode<'a>),
}

#[derive(Debug, Clone, GetPos)]
pub struct FnDeclNode<'a> {
    pub typ: Type,
    pub id: IDNode<'a>,
    pub symbol: Option<Rc<Symbol<'a>>>,
    pub formals: Vec<FormalDeclNode<'a>>,
    pub stmts: Vec<StmtNode<'a>>,
    pub pos: Pos
}

#[derive(Debug, Clone, GetPos)]
pub struct VarDeclNode<'a> {
    pub typ: Type,
    pub id: IDNode<'a>,
    pub symbol: Option<Rc<Symbol<'a>>>,
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct FormalDeclNode<'a> {
    pub typ: Type,
    pub id: IDNode<'a>,
    pub symbol: Option<Rc<Symbol<'a>>>,
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct IfStmtNode<'a> {
    pub exp: ExpNode<'a>,
    pub stmts: Vec<StmtNode<'a>>,
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct IfElseStmtNode<'a> {
    pub exp: ExpNode<'a>,
    pub true_stmts: Vec<StmtNode<'a>>,
    pub else_stmts: Vec<StmtNode<'a>>,
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct WhileStmtNode<'a> {
    pub exp: ExpNode<'a>,
    pub stmts: Vec<StmtNode<'a>>,
    pub pos: Pos,
}

// contains the variable to increment or decrement
#[derive(Debug, Clone, GetPos)]
pub struct PostIncStmtNode<'a> {
    pub lval: LValNode<'a>,
    pub pos: Pos,
}

#[derive(Debug, Clone, GetPos)]
pub struct PostDecStmtNode<'a> {
    pub lval: LValNode<'a>,
    pub pos: Pos,
}

// contains the variable that will recieve the input
#[derive(Debug, Clone, GetPos)]
pub struct ReadStmtNode<'a> {
    pub lval: LValNode<'a>,
    pub pos: Pos,
}

// contains the expression to write
#[derive(Debug, Clone, GetPos)]
pub struct WriteStmtNode<'a> {
    pub exp: ExpNode<'a>,
    pub pos: Pos,
}

// The expression to return
#[derive(Debug, Clone, GetPos)]
pub struct ReturnStmtNode<'a> {
    pub exp: Option<ExpNode<'a>>,
    pub pos: Pos,
}


#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Short,
    Bool,
    Str,
    Void,
    Ptr(Box<Type>),
}

