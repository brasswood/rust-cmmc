use std::vec::Vec;
use std::boxed::Box;
use crate::parse::unparse::Unparse;
use pest::iterators::Pair;
use crate::Rule;


#[derive(Debug, Clone)]
pub struct BinaryExpNode {
    pub op: BinaryOperator,
    pub lhs: Box<dyn ExpNode>,
    pub rhs: Box<dyn ExpNode>,
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

pub trait LValNode: ExpNode;

#[derive(Debug, Clone)]
pub struct IDNode (pub String);
impl LValNode for IDNode {}

#[derive(Debug, Clone)]
pub struct UnaryExpNode {
    pub op: UnaryOp,
    pub exp: Box<ExpNode>,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
}

// contains the id being derefed
#[derive(Debug, Clone)]
pub struct DerefNode (pub IDNode);
impl ExpNode for DerefNode {}

pub trait DeclNode: StmtNode;

#[derive(Debug, Clone)]
pub struct AssignExpNode {
    pub lval: Box<dyn LValNode>,
    pub exp: Box<dyn ExpNode>,
}
impl ExpNode for AssignExpNode {}

#[derive(Debug, Clone)]
pub struct VarDeclNode {
    pub typ: Type,
    pub id: IDNode,
}
impl DeclNode for VarDeclNode {}

#[derive(Debug, Clone)]
pub struct FormalDeclNode {
    pub typ: Type,
    pub id: IDNode,
}
impl DeclNode for FormalDeclNode {}

#[derive(Debug, Clone)]
pub struct FnDeclNode {
    pub typ: Type,
    pub id: IDNode,
    pub formals: Vec<FormalDeclNode>,
    pub stmts: Vec<Box<dyn StmtNode>>,
}
impl DeclNode for FnDeclNode {}

#[derive(Debug, Clone)]
pub struct CallExpNode {
    pub id: IDNode,
    pub args: Vec<Box<dyn ExpNode>>,
}
impl ExpNode for CallExpNode {}

pub trait ExpNode: ASTNode;

#[derive(Debug, Clone)]
pub struct IntLitNode (pub i32);
impl ExpNode for IntLitNode {}

#[derive(Debug, Clone)]
pub struct ShortLitNode (pub i16);
impl ExpNode for ShortLitNode {}

#[derive(Debug, Clone)]
pub struct StrLitNode (pub String);
impl ExpNode for StrLitNode {}

#[derive(Debug, Clone)]
pub struct TrueNode;
impl ExpNode for TrueNode {}

#[derive(Debug, Clone)]
pub struct FalseNode;
impl ExpNode for FalseNode {}

pub trait StmtNode: ASTNode

// Contains the expression to call
#[derive(Debug, Clone)]
pub struct CallStmtNode(pub Box<CallExpNode>);
impl StmtNode for CallStmtNode {}

#[derive(Debug, Clone)]
pub struct IfStmtNode {
    pub exp: Box<dyn ExpNode>,
    pub stmts: Vec<dyn StmtNode>,
}
impl StmtNode for IfStmtNode {}

#[derive(Debug, Clone)]
pub struct IfElseStmtNode {
    pub exp: Box<dyn ExpNode>,
    pub true_stmts: Vec<Box<dyn StmtNode>>,
    pub else_stmts: Vec<Box<dyn StmtNode>>,
}
impl StmtNode for IfElseStmtNode {}

// The expression to return
#[derive(Debug, Clone)]
pub struct ReturnStmtNode (pub Option<Box<dyn ExpNode>>);
impl StmtNode for ReturnStmtNode {}

#[derive(Debug, Clone)]
pub struct WhileStmtNode {
    pub exp: Box<dyn ExpNode>,
    pub stmts: Vec<Box<dyn StmtNode>>,
}
impl StmtNode for WhileStmtNode {}

// contains the variable to increment or decrement
#[derive(Debug, Clone)]
pub struct PostIncStmtNode (pub Box<dyn LValNode>);
impl StmtNode for PostIncStmtNode {}

#[derive(Debug, Clone)]
pub struct PostDecStmtNode (pub Box<dyn LValNode>);
impl StmtNode for PostDecStmtNode {}

// Contains the assign expression
#[derive(Debug, Clone)]
pub struct AssignStmtNode (pub Box<AssignExpNode>);
impl StmtNode for AssignStmtNode {}

// contains the variable that will recieve the input
#[derive(Debug, Clone)]
pub struct ReadStmtNode (pub LValNode);
impl StmtNode for ReadStmtNode {}

// contains the expression to write
#[derive(Debug, Clone)]
pub struct WriteStmtNode (pub Box<dyn ExpNode>);
impl StmtNode for WriteStmtNode {}

pub enum Type {
    Int,
    Short,
    Bool,
    Str,
    Void,
    Ptr(Box<Type>),
}

// contains the type being pointed to
#[derive(Debug, Clone)]
pub struct PtrTypeNode (pub Type);

pub trait ASTNode {
    fn to_string(&self) -> String;
}

#[derive(Debug, Clone)]
pub struct ProgramNode (pub Vec<dyn DeclNode>);
