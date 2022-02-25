use std::vec::Vec;
use std::boxed::Box;
use enum_as_inner::EnumAsInner;
use enum_dispatch::enum_dispatch;
use crate::parse::unparse::Unparse;

#[feature(macro_attributes_in_derive_output)]

#[derive(Debug, Clone)]
pub struct BinaryExpNode {
    pub op: BinaryOperator,
    pub lhs: Box<ExpNode>,
    pub rhs: Box<ExpNode>,
}

#[derive(Debug, Clone, EnumAsInner)]
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

#[enum_dispatch(Unparse)]
#[derive(Debug, Clone, EnumAsInner)]
pub enum LValNode {
    DerefNode(DerefNode),
    IDNode(IDNode),
}

#[derive(Debug, Clone)]
pub struct IDNode (pub String);

#[derive(Debug, Clone)]
pub struct UnaryExpNode {
    pub op: UnaryOp,
    pub exp: Box<ExpNode>,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
}

// contains the id being derefed
#[derive(Debug, Clone)]
pub struct DerefNode (pub IDNode);

#[enum_dispatch(Unparse)]
#[derive(Debug, Clone, EnumAsInner)]
pub enum DeclNode {
    FnDeclNode(FnDeclNode),
    FormalDeclNode(VarDeclStruct),
    VarDeclNode(VarDeclStruct),
}

#[derive(Debug, Clone)]
pub struct AssignExpNode {
    pub lval: LValNode,
    pub exp: Box<ExpNode>,
}

#[derive(Debug, Clone)]
pub struct VarDeclStruct {
    pub typ: TypeNode,
    pub id: IDNode,
}

#[derive(Debug, Clone)]
pub struct FnDeclNode {
    pub typ: Box<TypeNode>,
    pub id: IDNode,
    pub formals: Vec<VarDeclStruct>,
    pub stmts: Vec<StmtNode>,
}

#[derive(Debug, Clone)]
pub struct CallExpNode {
    pub id: IDNode,
    pub args: Vec<ExpNode>,
}

#[enum_dispatch(Unparse)]
#[derive(Debug, Clone, EnumAsInner)]
pub enum ExpNode {
    AssignExpNode(AssignExpNode),
    BinaryExpNode(BinaryExpNode),
    CallExpNode(CallExpNode),
    FalseNode(FalseNode),
    IntLitNode(IntLitNode),
    LValNode(LValNode),
    ShortLitNode(ShortLitNode),
    StrLitNode(StrLitNode),
    TrueNode(TrueNode),
    UnaryExpNode(UnaryExpNode),
}

#[derive(Debug, Clone)]
pub struct IntLitNode (pub i32);

#[derive(Debug, Clone)]
pub struct ShortLitNode (pub i16);

#[derive(Debug, Clone)]
pub struct StrLitNode (pub String);

#[derive(Debug, Clone)]
pub struct TrueNode;

#[derive(Debug, Clone)]
pub struct FalseNode;

#[enum_dispatch(Unparse)]
#[derive(Debug, Clone, EnumAsInner)]
pub enum StmtNode {
    AssignStmtNode(AssignStmtNode),
    CallStmtNode(CallStmtNode),
    DeclNode(DeclNode),
    IfElseStmtNode(IfElseStmtNode),
    IfStmtNode(IfStmtNode),
    PostDecStmtNode(PostIncStmtStruct),
    PostIncStmtNode(PostIncStmtStruct),
    ReadStmtNode(ReadStmtNode),
    ReturnStmtNode(ReturnStmtNode),
    WhileStmtNode(WhileStmtNode),
    WriteStmtNode(WriteStmtNode),
}

// Contains the expression to call
#[derive(Debug, Clone)]
pub struct CallStmtNode(pub Box<CallExpNode>);

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

// The expression to return
#[derive(Debug, Clone)]
pub struct ReturnStmtNode (pub Option<Box<ExpNode>>);

#[derive(Debug, Clone)]
pub struct WhileStmtNode {
    pub exp: Box<ExpNode>,
    pub stmts: Vec<StmtNode>,
}

// contains the variable to increment or decrement
#[derive(Debug, Clone)]
pub struct PostIncStmtStruct (pub LValNode);

// Contains the assign expression
#[derive(Debug, Clone)]
pub struct AssignStmtNode (pub Box<AssignExpNode>);

// contains the variable that will recieve the input
#[derive(Debug, Clone)]
pub struct ReadStmtNode (pub LValNode);

// contains the expression to write
#[derive(Debug, Clone)]
pub struct WriteStmtNode (pub Box<ExpNode>);

#[enum_dispatch(Unparse)]
#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeNode {
    BoolTypeNode,
    IntTypeNode,
    PtrTypeNode(PtrTypeNode),
    ShortTypeNode,
    StringTypeNode,
    VoidTypeNode,
}

// contains the type being pointed to
#[derive(Debug, Clone)]
pub struct PtrTypeNode (pub Box<TypeNode>);

#[enum_dispatch(Unparse)]
#[derive(Debug, Clone, EnumAsInner)]
pub enum ASTNode {
    ExpNode(ExpNode),
    ProgramNode(ProgramNode),
    StmtNode(StmtNode),
    TypeNode(TypeNode),
} 

#[derive(Debug, Clone)]
pub struct ProgramNode (pub Vec<DeclNode>);
