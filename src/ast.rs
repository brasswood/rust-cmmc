use std::vec::Vec;
use std::boxed::Box;
use std::fmt::Debug;
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
pub trait ASTNode {
    fn to_string(&self, depth: usize) -> String;
}

#[derive(Debug, Clone)]
pub struct ProgramNode (pub Vec<DeclNode>);

#[enum_dispatch(ASTNode)]
#[derive(Debug, Clone)]
pub enum ExpNode {
    AssignExp(AssignExpNode),
    UnaryExp(UnaryExpNode),
    BinaryExp(BinaryExpNode),
    CallExp(CallExpNode),
    LVal(LValNode),
    True(TrueNode),
    False(FalseNode),
    IntLit(IntLitNode),
    ShortLit(ShortLitNode),
    StrLit(StrLitNode),
}

#[derive(Debug, Clone)]
pub struct AssignExpNode {
    pub lval: LValNode,
    pub exp: Box<ExpNode>,
}

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

#[derive(Debug, Clone)]
pub struct BinaryExpNode {
    pub op: BinaryOperator,
    pub lhs: Box<ExpNode>,
    pub rhs: Box<ExpNode>,
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
pub struct CallExpNode {
    pub id: IDNode,
    pub args: Vec<ExpNode>,
}

#[enum_dispatch(ASTNode)]
#[derive(Debug, Clone)]
pub enum LValNode {
    ID(IDNode),
    Deref(DerefNode),
}

#[derive(Debug, Clone)]
pub struct IDNode (pub String);

// contains the id being derefed
#[derive(Debug, Clone)]
pub struct DerefNode (pub IDNode);

#[derive(Debug, Clone)]
pub struct TrueNode;

#[derive(Debug, Clone)]
pub struct FalseNode;

#[derive(Debug, Clone)]
pub struct IntLitNode (pub i32);

#[derive(Debug, Clone)]
pub struct ShortLitNode (pub i16);

#[derive(Debug, Clone)]
pub struct StrLitNode (pub String);


#[enum_dispatch(ASTNode)]
#[derive(Debug, Clone)]
pub enum StmtNode {
    AssignStmt(AssignStmtNode),
    CallStmt(CallStmtNode),
    Decl(DeclNode),
    IfStmt(IfStmtNode),
    IfElseStmt(IfElseStmtNode),
    WhileStmt(WhileStmtNode),
    PostIncStmt(PostIncStmtNode),
    PostDecStmt(PostDecStmtNode),
    ReadStmt(ReadStmtNode),
    WriteStmt(WriteStmtNode),
    ReturnStmt(ReturnStmtNode),
}

// Contains the assign expression
#[derive(Debug, Clone)]
pub struct AssignStmtNode (pub Box<AssignExpNode>);

// Contains the expression to call
#[derive(Debug, Clone)]
pub struct CallStmtNode(pub CallExpNode);

#[enum_dispatch(ASTNode)]
#[derive(Debug, Clone)]
pub enum DeclNode {
    FnDecl(FnDeclNode),
    VarDecl(VarDeclNode),
    FormalDecl(FormalDeclNode),
}

#[derive(Debug, Clone)]
pub struct FnDeclNode {
    pub typ: Type,
    pub id: IDNode,
    pub formals: Vec<FormalDeclNode>,
    pub stmts: Vec<StmtNode>,
}

#[derive(Debug, Clone)]
pub struct VarDeclNode {
    pub typ: Type,
    pub id: IDNode,
}

#[derive(Debug, Clone)]
pub struct FormalDeclNode {
    pub typ: Type,
    pub id: IDNode,
}

#[derive(Debug, Clone)]
pub struct IfStmtNode {
    pub exp: Box<ExpNode>,
    pub stmts: Vec<StmtNode>,
}

#[derive(Debug, Clone)]
pub struct IfElseStmtNode {
    pub exp: Box<ExpNode>,
    pub true_stmts: Vec<StmtNode>,
    pub else_stmts: Vec<StmtNode>,
}

#[derive(Debug, Clone)]
pub struct WhileStmtNode {
    pub exp: Box<ExpNode>,
    pub stmts: Vec<StmtNode>,
}

// contains the variable to increment or decrement
#[derive(Debug, Clone)]
pub struct PostIncStmtNode (pub LValNode);

#[derive(Debug, Clone)]
pub struct PostDecStmtNode (pub LValNode);

// contains the variable that will recieve the input
#[derive(Debug, Clone)]
pub struct ReadStmtNode (pub LValNode);

// contains the expression to write
#[derive(Debug, Clone)]
pub struct WriteStmtNode (pub Box<ExpNode>);

// The expression to return
#[derive(Debug, Clone)]
pub struct ReturnStmtNode (pub Option<Box<ExpNode>>);


#[derive(Debug, Clone)]
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

