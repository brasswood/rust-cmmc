use std::vec::Vec;
use std::boxed::Box;
use std::fmt::Debug;

#[derive(Debug)]
pub struct BinaryExpNode {
    pub op: BinaryOperator,
    pub lhs: Box<dyn ExpNode>,
    pub rhs: Box<dyn ExpNode>,
}
impl ExpNode for BinaryExpNode{}

#[derive(Debug)]
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

pub trait LValNode: ExpNode + Debug {}

pub fn upcast(obj: Box<dyn LValNode>) -> Box<dyn ExpNode> {
    obj.as_exp_node()
}


#[derive(Debug)]
pub struct IDNode (pub String);
impl LValNode for IDNode {}
impl ExpNode for IDNode {}

#[derive(Debug)]
pub struct UnaryExpNode {
    pub op: UnaryOp,
    pub exp: Box<dyn ExpNode>,
}
impl ExpNode for UnaryExpNode {}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
}

// contains the id being derefed
#[derive(Debug)]
pub struct DerefNode (pub IDNode);
impl ExpNode for DerefNode {}
impl LValNode for DerefNode {}

pub trait DeclNode: StmtNode + Debug {}

#[derive(Debug)]
pub struct AssignExpNode {
    pub lval: Box<dyn LValNode>,
    pub exp: Box<dyn ExpNode>,
}
impl ExpNode for AssignExpNode {}

#[derive(Debug)]
pub struct VarDeclNode {
    pub typ: Type,
    pub id: IDNode,
}
impl DeclNode for VarDeclNode {}
impl StmtNode for VarDeclNode {}

#[derive(Debug)]
pub struct FormalDeclNode {
    pub typ: Type,
    pub id: IDNode,
}
impl DeclNode for FormalDeclNode {}
impl StmtNode for FormalDeclNode {}

#[derive(Debug)]
pub struct FnDeclNode {
    pub typ: Type,
    pub id: IDNode,
    pub formals: Vec<FormalDeclNode>,
    pub stmts: Vec<Box<dyn StmtNode>>,
}
impl DeclNode for FnDeclNode {}
impl StmtNode for FnDeclNode {}

#[derive(Debug)]
pub struct CallExpNode {
    pub id: IDNode,
    pub args: Vec<Box<dyn ExpNode>>,
}
impl ExpNode for CallExpNode {}

pub trait AsExpNode {
    fn as_exp_node<'a>(self: Box<Self>) -> Box<dyn ExpNode + 'a>
    where
        Self: 'a;
}

impl<T: ExpNode + Sized> AsExpNode for T {
    fn as_exp_node<'a>(self: Box<Self>) -> Box<dyn ExpNode + 'a>
        where
            Self: 'a,
    {
        self
    }
}

pub trait ExpNode: AsExpNode + ASTNode + Debug {}

#[derive(Debug)]
pub struct IntLitNode (pub i32);
impl ExpNode for IntLitNode {}

#[derive(Debug)]
pub struct ShortLitNode (pub i16);
impl ExpNode for ShortLitNode {}

#[derive(Debug)]
pub struct StrLitNode (pub String);
impl ExpNode for StrLitNode {}

#[derive(Debug)]
pub struct TrueNode;
impl ExpNode for TrueNode {}

#[derive(Debug)]
pub struct FalseNode;
impl ExpNode for FalseNode {}

pub trait StmtNode: ASTNode + Debug {}

// Contains the expression to call
#[derive(Debug)]
pub struct CallStmtNode(pub CallExpNode);
impl StmtNode for CallStmtNode {}

#[derive(Debug)]
pub struct IfStmtNode {
    pub exp: Box<dyn ExpNode>,
    pub stmts: Vec<Box<dyn StmtNode>>,
}
impl StmtNode for IfStmtNode {}

#[derive(Debug)]
pub struct IfElseStmtNode {
    pub exp: Box<dyn ExpNode>,
    pub true_stmts: Vec<Box<dyn StmtNode>>,
    pub else_stmts: Vec<Box<dyn StmtNode>>,
}
impl StmtNode for IfElseStmtNode {}

// The expression to return
#[derive(Debug)]
pub struct ReturnStmtNode (pub Option<Box<dyn ExpNode>>);
impl StmtNode for ReturnStmtNode {}

#[derive(Debug)]
pub struct WhileStmtNode {
    pub exp: Box<dyn ExpNode>,
    pub stmts: Vec<Box<dyn StmtNode>>,
}
impl StmtNode for WhileStmtNode {}

// contains the variable to increment or decrement
#[derive(Debug)]
pub struct PostIncStmtNode (pub Box<dyn LValNode>);
impl StmtNode for PostIncStmtNode {}

#[derive(Debug)]
pub struct PostDecStmtNode (pub Box<dyn LValNode>);
impl StmtNode for PostDecStmtNode {}

// Contains the assign expression
#[derive(Debug)]
pub struct AssignStmtNode (pub Box<AssignExpNode>);
impl StmtNode for AssignStmtNode {}

// contains the variable that will recieve the input
#[derive(Debug)]
pub struct ReadStmtNode (pub Box<dyn LValNode>);
impl StmtNode for ReadStmtNode {}

// contains the expression to write
#[derive(Debug)]
pub struct WriteStmtNode (pub Box<dyn ExpNode>);
impl StmtNode for WriteStmtNode {}

#[derive(Debug)]
pub enum Type {
    Int,
    Short,
    Bool,
    Str,
    Void,
    Ptr(Box<Type>),
}

// contains the type being pointed to
#[derive(Debug)]
pub struct PtrTypeNode (pub Type);

pub trait ASTNode {
    fn to_string(&self, depth: usize) -> String;
}

#[derive(Debug)]
pub struct ProgramNode (pub Vec<Box<dyn DeclNode>>);
