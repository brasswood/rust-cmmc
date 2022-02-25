use crate::ast::*;
use pest::{Parser, iterators::Pair};
use crate::peg::{Rule, CMMParser};
use std::process;
use std::io::{self, Write};

pub fn parse(input: &str) -> Pair<Rule> {
    return match CMMParser::parse(Rule::program, input) {
        Ok(mut ps) => {
            let p = ps.next().unwrap();
            println!("{:?}", p.as_rule());
            p
        }
        Err(_) => {
            writeln!(io::stderr(), "syntax error\nParse failed").unwrap();
            process::exit(1)
        }
    };
}

impl ProgramNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let inner_pairs = pair.into_inner();
        let mut decls: Vec<Box<dyn DeclNode>> = Vec::new();
        for pair in inner_pairs {
            if let Rule::decl = pair.as_rule() {
                decls.push(generate_decl_node(pair));
            }
        }
        ProgramNode(decls)
    }
}

impl ASTNode for ProgramNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl VarDeclNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        VarDeclNode {
            typ: Type::from(inner_pairs.next().unwrap()),
            id: IDNode::from(inner_pairs.next().unwrap()),
        }
    }
}

impl ASTNode for VarDeclNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl FnDeclNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        let typ = Type::from(inner_pairs.next().unwrap());
        let id = IDNode::from(inner_pairs.next().unwrap());
        let mut formals: Vec<FormalDeclNode> = Vec::new();
        let stmts: Vec<Box<dyn StmtNode>>;
        let maybe_formals = inner_pairs.nth(1).unwrap();
        let stmt_list_pair;
        if let Rule::formals = maybe_formals.as_rule() {
            for formal in maybe_formals.into_inner() {
                if let Rule::formalDecl = formal.as_rule() {
                    formals.push(FormalDeclNode::from(formal));
                }
            }
            stmt_list_pair = inner_pairs.nth(2).unwrap();
        } else {
            stmt_list_pair = inner_pairs.nth(1).unwrap();
        }
        stmts = generate_stmt_list(stmt_list_pair);
        FnDeclNode { typ, id, formals, stmts }
    }
}

impl ASTNode for FnDeclNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl Type {
    pub fn from(pair: Pair<Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        let fst = inner_pairs.next().unwrap();
        let actual_pair;
        let is_ptr;
        if let Rule::PTR = fst.as_rule() {
            actual_pair = inner_pairs.next().unwrap();
            is_ptr = true;
        } else {
            actual_pair = fst;
            is_ptr = false;
        }
        let actual_type = match actual_pair
            .into_inner()
            .next()
            .unwrap()
            .as_rule() {
            Rule::INT => Type::Int,
            Rule::BOOL => Type::Bool,
            Rule::STRING => Type::Str,
            Rule::SHORT => Type::Short,
            Rule::VOID => Type::Void,
            _ => unreachable!(),
        };
        if is_ptr {
            Type::Ptr(Box::new(actual_type))
        } else {
            actual_type
        }
    }
}

impl ASTNode for Type {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl FormalDeclNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        FormalDeclNode {
            typ: Type::from(inner_pairs.next().unwrap()),
            id: IDNode::from(inner_pairs.next().unwrap()),
        }
    }
}

impl ASTNode for FormalDeclNode {
    fn to_string(&self) -> String {
        todo!()
    }
}



impl IDNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        IDNode(pair.as_str().to_string())
    }
}

impl ASTNode for IDNode {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

impl IntLitNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        IntLitNode(pair.as_str().parse().unwrap())
    }
}

impl ASTNode for IntLitNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ShortLitNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let s = pair.as_str();
        let len = s.len();
        ShortLitNode(s[..len-1].parse().unwrap())
    }
}

impl ASTNode for ShortLitNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl StrLitNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        StrLitNode(pair.as_str().to_string())
    }
}

impl ASTNode for StrLitNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl AssignExpNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        let lval = generate_lval_node(inner_pairs.next().unwrap());
        let exp = generate_exp_node(inner_pairs.nth(1).unwrap());
        AssignExpNode { lval, exp }
    }
}

impl ASTNode for AssignExpNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl CallExpNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        let id = IDNode::from(inner_pairs.next().unwrap());
        let mut args: Vec<Box<dyn ExpNode>> = Vec::new();
        let maybe_actuals_pair = inner_pairs.nth(1).unwrap();
        if let Rule::actualsList = maybe_actuals_pair.as_rule() {
            let mut pairs = maybe_actuals_pair.into_inner();
            while let Some(pair) = pairs.next(){
                args.push(generate_exp_node(pair));
                pairs.next();
            }
        }
        CallExpNode { id, args }
    }
}

impl ASTNode for CallExpNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

pub fn generate_exp_node(exp_pair: Pair<Rule>) -> Box<dyn ExpNode> {
    match exp_pair.as_rule() {
        Rule::exp => generate_exp_node(
            exp_pair.into_inner().next().unwrap()
        ),
        Rule::assignExp => Box::new(AssignExpNode::from(exp_pair)),
        Rule::boolExp
        | Rule::logicTerm
        | Rule::compareExp
        | Rule::sum
        | Rule::term => {
            // There are two cases here. The first is that we are
            // looking at a pair which has a chain of one or more 
            // operators. The second is that we're just looking at 
            // a singular factor from above.
            let mut inner_pairs = exp_pair.into_inner();
            let mut lhs = generate_exp_node(inner_pairs.next().unwrap());
            while let Some(pair) = inner_pairs.next() {
                let op = match pair.as_rule() {
                    Rule::OR => BinaryOperator::Or,
                    Rule::AND => BinaryOperator::And,
                    Rule::EQUALS => BinaryOperator::Equals,
                    Rule::NOTEQUALS => BinaryOperator::NotEquals,
                    Rule::GREATER => BinaryOperator::Greater,
                    Rule::GREATEREQ => BinaryOperator::GreaterEq,
                    Rule::LESS => BinaryOperator::Less,
                    Rule::LESSEQ => BinaryOperator::LessEq,
                    Rule::PLUS => BinaryOperator::Plus,
                    Rule::MINUS => BinaryOperator::Minus,
                    Rule::TIMES => BinaryOperator::Times,
                    Rule::DIVIDE => BinaryOperator::Divide,
                    _ => unreachable!(),
                };
                let rhs = generate_exp_node(inner_pairs.next().unwrap());
                lhs = Box::new(BinaryExpNode { op, lhs, rhs });
            }
            lhs
        }
        Rule::factor => {
            let mut inner_pairs = exp_pair.into_inner();
            let fst = inner_pairs.next().unwrap();
            if let Rule::NOT = fst.as_rule() {
                return Box::new(UnaryExpNode {
                    op: UnaryOp::Not,
                    exp: generate_exp_node(inner_pairs.next().unwrap()),
                })
            } else if let Rule::assignExp = fst.as_rule() {
                return Box::new(AssignExpNode::from(fst))
            }
            let actual;
            let neg_node;
            if let Rule::MINUS = fst.as_rule() {
                actual = inner_pairs.next().unwrap();
                neg_node = true;
            } else {
                actual = fst;
                neg_node = false;
            }
            let actual: Box<dyn ExpNode> = match actual.as_rule() {
                Rule::INTLITERAL => Box::new(IntLitNode::from(actual)),
                Rule::SHORTLITERAL => Box::new(ShortLitNode::from(actual)),
                Rule::STRLITERAL => Box::new(StrLitNode::from(actual)),
                Rule::TRUE => Box::new(TrueNode),
                Rule::FALSE => Box::new(FalseNode),
                Rule::LPAREN => generate_exp_node(
                    inner_pairs.next().unwrap()
                ),
                Rule::AMP => Box::new(UnaryExpNode {
                    op: UnaryOp::Ref,
                    exp: Box::new(
                        IDNode::from(inner_pairs.next().unwrap())
                    ),
                }),
                Rule::callExp => Box::new(CallExpNode::from(actual)),
                Rule::lval => upcast(generate_lval_node(actual)),
                _ => unreachable!(),
            };
            if neg_node {
                Box::new(UnaryExpNode {
                    op: UnaryOp::Neg,
                    exp: actual,
                })
            } else {
                actual
            }
        }
        _ => unreachable!(),
    }
}

impl ASTNode for BinaryExpNode {
    fn to_string(&self) -> String {
        let op_str = match self.op {
            BinaryOperator::And => "and",
            BinaryOperator::Divide => "/",
            BinaryOperator::Equals => "==",
            BinaryOperator::GreaterEq => ">=",
            BinaryOperator::Greater => ">",
            BinaryOperator::LessEq => "<=",
            BinaryOperator::Less => "<",
            BinaryOperator::Minus => "-",
            BinaryOperator::NotEquals => "!=",
            BinaryOperator::Or => "or",
            BinaryOperator::Plus => "+",
            BinaryOperator::Times => "*",
        }.to_string();
        let lhs_str = self.lhs.to_string();
        let rhs_str = self.rhs.to_string();
        format!("{} {} {}", lhs_str, op_str, rhs_str)
    }
}

impl ASTNode for UnaryExpNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for DerefNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for TrueNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for FalseNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for CallStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for IfStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for IfElseStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for ReturnStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for WhileStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for PostIncStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for PostDecStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for AssignStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for ReadStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ASTNode for WriteStmtNode {
    fn to_string(&self) -> String {
        todo!()
    }
}

pub fn generate_lval_node(pair: Pair<Rule>) -> Box<dyn LValNode> {
    let mut inner_pairs = pair.into_inner();
    let fst = inner_pairs.next().unwrap();
    match fst.as_rule() {
        Rule::AT => Box::new(
            DerefNode(IDNode::from(inner_pairs.next().unwrap()))
        ),
        Rule::id => Box::new(IDNode::from(fst)),
        _ => unreachable!(),
    }
}

pub fn generate_decl_node(pair: Pair<Rule>) -> Box<dyn DeclNode> {
    let inner_pair = pair.into_inner().next().unwrap();
    match inner_pair.as_rule() {
        Rule::varDecl => Box::new(VarDeclNode::from(inner_pair)),
        Rule::fnDecl => Box::new(FnDeclNode::from(inner_pair)),
        _ => unreachable!(),
    }
}

pub fn generate_stmt_list(pair: Pair<Rule>) -> Vec<Box<dyn StmtNode>> {
    let mut vec: Vec<Box<dyn StmtNode>> = Vec::new();
    for p in pair.into_inner() {
        vec.push(generate_stmt_node(p));
    }
    vec
}

pub fn generate_stmt_node(pair: Pair<Rule>) -> Box<dyn StmtNode> {
    let mut inner_pairs = pair.into_inner();
    let fst = inner_pairs.next().unwrap();
    match fst.as_rule() {
        Rule::varDecl => Box::new(VarDeclNode::from(fst)),
        Rule::assignExp => Box::new(
            AssignStmtNode(Box::new(AssignExpNode::from(fst)))
        ),
        Rule::lval => match inner_pairs.next().unwrap().as_rule() {
            Rule::DEC => Box::new(
                PostDecStmtNode(generate_lval_node(fst))
            ),
            Rule::INC => Box::new(
                PostIncStmtNode(generate_lval_node(fst))
            ),
            _ => unreachable!(),
        },
        Rule::READ => Box::new(
            ReadStmtNode(generate_lval_node(inner_pairs.next().unwrap()))
        ),
        Rule::WRITE => Box::new(
            WriteStmtNode(generate_exp_node(inner_pairs.next().unwrap()))
        ),
        Rule::IF => {
            let exp = generate_exp_node(inner_pairs.nth(1).unwrap());
            let true_stmts = generate_stmt_list(
                inner_pairs.nth(2).unwrap()
            );
            let maybe_else = inner_pairs.nth(1);
            if let Some(else_pair) = maybe_else {
                let else_stmts = generate_stmt_list(
                    inner_pairs.nth(1).unwrap()
                );
                Box::new(IfElseStmtNode { exp, true_stmts, else_stmts })
            } else {
                Box::new(IfStmtNode { exp, stmts: true_stmts })
            }
        }
        Rule::WHILE => {
            let exp = generate_exp_node(inner_pairs.nth(1).unwrap());
            let stmts = generate_stmt_list(inner_pairs.nth(2).unwrap());
            Box::new(WhileStmtNode { exp, stmts })
        }
        Rule::RETURN => {
            let maybe_exp = inner_pairs.next().unwrap();
            match maybe_exp.as_rule() {
                Rule::exp => Box::new(
                    ReturnStmtNode(Some(generate_exp_node(maybe_exp)))
                ),
                _ => Box::new(ReturnStmtNode(None)),
            }
        }
        Rule::callExp => Box::new(CallStmtNode(CallExpNode::from(fst))),
        _ => unreachable!(),
    }
}
