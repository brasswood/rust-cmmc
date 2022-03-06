// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use std::vec::Vec;
use std::collections::HashMap;
use std::rc::Rc;
use std::boxed::Box;
use std::fmt::Debug;
use crate::name::symbol::{Symbol, SymbolTable, AsSymbol};
use crate::name::NameAnalysis;
use enum_dispatch::enum_dispatch;
use crate::parse::Unparse;

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

#[derive(Debug, Clone)]
pub struct AssignExpNode<'a> {
    pub lval: LValNode<'a>,
    pub exp: Box<ExpNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpNode<'a> {
    pub op: UnaryOp,
    pub exp: Box<ExpNode<'a>>,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
}

#[derive(Debug, Clone)]
pub struct BinaryExpNode<'a> {
    pub op: BinaryOperator,
    pub lhs: Box<ExpNode<'a>>,
    pub rhs: Box<ExpNode<'a>>,
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

#[derive(Debug, Clone)]
pub struct CallExpNode<'a> {
    pub id: IDNode<'a>,
    pub args: Vec<ExpNode<'a>>,
}

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum LValNode<'a> {
    ID(IDNode<'a>),
    Deref(DerefNode<'a>),
}

#[derive(Debug, Clone)]
pub struct IDNode<'a> {
    pub name: &'a str,
    pub symbol: Option<Rc<Symbol<'a>>>,
}

// contains the id being derefed
#[derive(Debug, Clone)]
pub struct DerefNode<'a> (pub IDNode<'a>);

#[derive(Debug, Clone)]
pub struct TrueNode;

#[derive(Debug, Clone)]
pub struct FalseNode;

#[derive(Debug, Clone)]
pub struct IntLitNode (pub i32);

#[derive(Debug, Clone)]
pub struct ShortLitNode (pub i16);

#[derive(Debug, Clone)]
pub struct StrLitNode<'a> (pub &'a str);


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
#[derive(Debug, Clone)]
pub struct AssignStmtNode<'a> (pub Box<AssignExpNode<'a>>);

// Contains the expression to call
#[derive(Debug, Clone)]
pub struct CallStmtNode<'a> (pub CallExpNode<'a>);

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum DeclNode<'a> {
    FnDecl(FnDeclNode<'a>),
    VarDecl(VarDeclNode<'a>),
    FormalDecl(FormalDeclNode<'a>),
}

#[derive(Debug, Clone)]
pub struct FnDeclNode<'a> {
    pub typ: Type,
    pub id: IDNode<'a>,
    pub formals: Vec<FormalDeclNode<'a>>,
    pub stmts: Vec<StmtNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct VarDeclNode<'a> {
    pub typ: Type,
    pub id: IDNode<'a>,
}

#[derive(Debug, Clone)]
pub struct FormalDeclNode<'a> {
    pub typ: Type,
    pub id: IDNode<'a>,
}

#[derive(Debug, Clone)]
pub struct IfStmtNode<'a> {
    pub exp: Box<ExpNode<'a>>,
    pub stmts: Vec<StmtNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct IfElseStmtNode<'a> {
    pub exp: Box<ExpNode<'a>>,
    pub true_stmts: Vec<StmtNode<'a>>,
    pub else_stmts: Vec<StmtNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct WhileStmtNode<'a> {
    pub exp: Box<ExpNode<'a>>,
    pub stmts: Vec<StmtNode<'a>>,
}

// contains the variable to increment or decrement
#[derive(Debug, Clone)]
pub struct PostIncStmtNode<'a> (pub LValNode<'a>);

#[derive(Debug, Clone)]
pub struct PostDecStmtNode<'a> (pub LValNode<'a>);

// contains the variable that will recieve the input
#[derive(Debug, Clone)]
pub struct ReadStmtNode<'a> (pub LValNode<'a>);

// contains the expression to write
#[derive(Debug, Clone)]
pub struct WriteStmtNode<'a> (pub Box<ExpNode<'a>>);

// The expression to return
#[derive(Debug, Clone)]
pub struct ReturnStmtNode<'a> (pub Option<Box<ExpNode<'a>>>);


#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Short,
    Bool,
    Str,
    Void,
    Ptr(Box<Type>),
}

