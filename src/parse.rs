// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use crate::ast::*;
use crate::ast::ExpNode::*;
use crate::ast::LValNode::*;
use crate::ast::StmtNode::*;
use crate::ast::DeclNode::*;
use crate::name::symbol::SymbolType;
use pest::{Parser, iterators::Pair};
use crate::peg::{Rule, CMMParser};
use std::process;
use std::io::{self, Write};
use std::fs::File;
use enum_dispatch::enum_dispatch;

#[enum_dispatch(ExpNode, LValNode, StmtNode, DeclNode)]
pub trait Unparse {
    fn to_string(&self, depth: usize) -> String;
}

pub fn parse(input: &str) -> Pair<Rule> {
    return match CMMParser::parse(Rule::program, input) {
        Ok(mut ps) => {
            let p = ps.next().unwrap();
            // println!("{:?}", p.as_rule());
            p
        }
        Err(_) => {
            writeln!(io::stderr(), "syntax error\nUnparse failed").unwrap();
            process::exit(1)
        }
    };
}


pub fn unparse(tree: &ProgramNode, output: &mut File) {
    if let Err(_) = writeln!(output, "{}", tree.to_string(0)) {
        writeln!(io::stderr(), "Error writing to file").unwrap();
        process::exit(1);
    }
}

impl<'a> ProgramNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        let inner_pairs = pair.into_inner();
        let mut decls: Vec<DeclNode> = Vec::new();
        for pair in inner_pairs {
            if let Rule::decl = pair.as_rule() {
                decls.push(DeclNode::from(pair));
            }
        }
        ProgramNode(decls)
    }
}

impl Unparse for ProgramNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        let mut ret = String::new();
        for decl in &self.0 {
            ret.push_str(&decl.to_string(depth));
            ret.push('\n');
        }
        ret
    }
}

impl<'a> VarDeclNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        let mut inner_pairs = pair.clone().into_inner();
        VarDeclNode {
            typ: Type::from(inner_pairs.next().unwrap()),
            id: IDNode::from(inner_pairs.next().unwrap()),
            pair,
        }
    }
}

impl Unparse for VarDeclNode<'_> {
    fn to_string(&self, depth: usize) -> String {
       format!("{}{} {};",
           "    ".repeat(depth),
           self.typ.to_string(0),
           self.id.to_string(0),
        )
    }
}

impl<'a> FnDeclNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        let typ = Type::from(inner_pairs.next().unwrap());
        let id = IDNode::from(inner_pairs.next().unwrap());
        let mut formals: Vec<FormalDeclNode> = Vec::new();
        let stmts: Vec<StmtNode>;
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

impl Unparse for FnDeclNode<'_> {
    fn to_string(&self, depth: usize) -> String {
       let mut formals = String::new();
       let iter = &mut self.formals.iter();
       if let Some(formal) = iter.next() {
           formals.push_str(&formal.to_string(0));
           while let Some(formal) = iter.next() {
               formals.push_str(", ");
               formals.push_str(&formal.to_string(0));
           }
       }
       let mut stmts = String::new();
       for stmt in &self.stmts {
           stmts.push_str(&stmt.to_string(depth+1));
           stmts.push('\n');
       }
       format!("\n{}{} {}({}) {{\n{}{}}}\n",
        "    ".repeat(depth),
        &self.typ.to_string(0),
        &self.id.to_string(0),
        &formals,
        &stmts,
        "    ".repeat(depth),
       )
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

impl Unparse for Type {
    fn to_string(&self, _: usize) -> String {
        match self {
            Type::Int => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Str => "string".to_string(),
            Type::Short => "short".to_string(),
            Type::Void => "void".to_string(),
            Type::Ptr(t) => "ptr ".to_string() + &t.to_string(0),
        }
    }
}

impl<'a> FormalDeclNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        let mut inner_pairs = pair.clone().into_inner();
        FormalDeclNode {
            typ: Type::from(inner_pairs.next().unwrap()),
            id: IDNode::from(inner_pairs.next().unwrap()),
            pair,
        }
    }
}

impl Unparse for FormalDeclNode<'_> {
    fn to_string(&self, _: usize) -> String {
        format!("{} {}", self.typ.to_string(0), self.id.to_string(0))
    }
}



impl<'a> IDNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        IDNode { name: pair.as_str(), symbol: None, pair }
    }
}

impl Unparse for IDNode<'_> {
    fn to_string(&self, _: usize) -> String {
        let id = self.name.to_string();
        match &self.symbol {
            Some(sym) => format!("{}({})", id, sym.typ.to_string()),
            None => id,
        }
    }
}

impl SymbolType {
    fn to_string(&self) -> String {
        match self {
            SymbolType::Int => "int".to_string(),
            SymbolType::Short => "short".to_string(),
            SymbolType::Bool => "bool".to_string(),
            SymbolType::Str => "string".to_string(),
            SymbolType::Void => "void".to_string(),
            SymbolType::Ptr(t) => format!("ptr {}", t.to_string()),
            SymbolType::Fn { args, ret } => {
                let args_str = {
                    let mut it = args.iter();
                    if let Some(arg) = it.next() {
                        arg.to_string() + &{
                            let mut others = String::new();
                            while let Some(arg) = it.next() {
                                others = format!(
                                    "{},{}",
                                    others, 
                                    arg.to_string()
                                );
                            }
                            others
                        }
                    } else {
                        "".to_string()
                    }
                };
                format!("{}->{}", args_str, ret.to_string())
            }
        }
    }
}


impl IntLitNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        IntLitNode(pair.as_str().parse().unwrap())
    }
}

impl Unparse for IntLitNode {
    fn to_string(&self, _: usize) -> String {
        format!("{}", self.0)
    }
}

impl ShortLitNode {
    pub fn from(pair: Pair<Rule>) -> Self {
        let s = pair.as_str();
        let len = s.len();
        ShortLitNode(s[..len-1].parse().unwrap())
    }
}

impl Unparse for ShortLitNode {
    fn to_string(&self, _: usize) -> String {
        format!("{}S", self.0)
    }
}

impl<'a> StrLitNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        StrLitNode(pair.as_str())
    }
}

impl Unparse for StrLitNode<'_> {
    fn to_string(&self, _: usize) -> String {
        self.0.to_string()
    }
}

impl<'a> AssignExpNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        let lval = LValNode::from(inner_pairs.next().unwrap());
        let exp = ExpNode::from(inner_pairs.nth(1).unwrap());
        AssignExpNode { lval, exp: Box::new(exp) }
    }
}

impl Unparse for AssignExpNode<'_> {
    fn to_string(&self, _: usize) -> String {
        format!("({} = {})", self.lval.to_string(0), self.exp.to_string(0))
    }
}

impl<'a> CallExpNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        let id = IDNode::from(inner_pairs.next().unwrap());
        let mut args: Vec<ExpNode> = Vec::new();
        let maybe_actuals_pair = inner_pairs.nth(1).unwrap();
        if let Rule::actualsList = maybe_actuals_pair.as_rule() {
            let mut pairs = maybe_actuals_pair.into_inner();
            while let Some(pair) = pairs.next(){
                args.push(ExpNode::from(pair));
                pairs.next();
            }
        }
        CallExpNode { id, args }
    }
}

impl Unparse for CallExpNode<'_> {
    fn to_string(&self, _: usize) -> String {
        let mut actuals = String::new();
        let iter = &mut self.args.iter();
        if let Some(arg) = iter.next() {
            actuals.push_str(&arg.to_string(0));
            while let Some(arg) = iter.next() {
                actuals.push_str(", ");
                actuals.push_str(&arg.to_string(0));
            }
        }
        format!("{}({})", self.id.to_string(0), actuals)
    }
}

impl<'a> ExpNode<'a> {
    pub fn from(exp_pair: Pair<'a, Rule>) -> Self {
        match exp_pair.as_rule() {
            Rule::exp => ExpNode::from(
                exp_pair.into_inner().next().unwrap()
            ),
            Rule::assignExp => AssignExp(AssignExpNode::from(exp_pair)),
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
                let mut lhs = ExpNode::from(inner_pairs.next().unwrap());
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
                    let rhs = ExpNode::from(inner_pairs.next().unwrap());
                    lhs = BinaryExp(
                        BinaryExpNode {
                            op, 
                            lhs: Box::new(lhs), 
                            rhs: Box::new(rhs), 
                        }
                    );
                }
                lhs
            }
            Rule::factor => {
                let mut inner_pairs = exp_pair.into_inner();
                let fst = inner_pairs.next().unwrap();
                if let Rule::NOT = fst.as_rule() {
                    return UnaryExp(UnaryExpNode {
                        op: UnaryOp::Not,
                        exp: Box::new(
                            ExpNode::from(inner_pairs.next().unwrap())
                        ),
                    });
                } else if let Rule::assignExp = fst.as_rule() {
                    return AssignExp(AssignExpNode::from(fst));
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
                let actual: ExpNode = match actual.as_rule() {
                    Rule::INTLITERAL => IntLit(IntLitNode::from(actual)),
                    Rule::SHORTLITERAL => ShortLit(ShortLitNode::from(actual)),
                    Rule::STRLITERAL => StrLit(StrLitNode::from(actual)),
                    Rule::TRUE => True(TrueNode),
                    Rule::FALSE => False(FalseNode),
                    Rule::LPAREN => ExpNode::from(
                        inner_pairs.next().unwrap()
                    ),
                    Rule::AMP => UnaryExp(
                        UnaryExpNode {
                            op: UnaryOp::Ref,
                            exp: Box::new(
                                LVal(
                                    ID(
                                        IDNode::from(
                                            inner_pairs.next().unwrap()
                                        )
                                    )
                                )
                            ),
                        }
                    ),
                    Rule::callExp => CallExp(CallExpNode::from(actual)),
                    Rule::lval => LVal(LValNode::from(actual)),
                    _ => unreachable!(),
                };
                if neg_node {
                    UnaryExp(
                        UnaryExpNode { op: UnaryOp::Neg, exp: Box::new(actual) }
                    )
                } else {
                    actual
                }
            }
            _ => unreachable!(),
        }
    }
}

impl Unparse for BinaryExpNode<'_> {
    fn to_string(&self, _: usize) -> String {
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
        let lhs_str = self.lhs.to_string(0);
        let rhs_str = self.rhs.to_string(0);
        format!("({} {} {})", lhs_str, op_str, rhs_str)
    }
}

impl Unparse for UnaryExpNode<'_> {
    fn to_string(&self, _: usize) -> String {
        let op = match self.op {
            UnaryOp::Neg => "-",
            UnaryOp::Ref => "&",
            UnaryOp::Not => "!",
        };
        let exp = self.exp.to_string(0);
        format!("({}{})", op, exp)
    }
}

impl Unparse for DerefNode<'_> {
    fn to_string(&self, _: usize) -> String {
        format!("@{}", self.0.to_string(0))
    }
}

impl Unparse for TrueNode {
    fn to_string(&self, _: usize) -> String {
        "true".to_string()
    }
}

impl Unparse for FalseNode {
    fn to_string(&self, _: usize) -> String {
        "false".to_string()
    }
}

impl Unparse for CallStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        format!("{}{};", "    ".repeat(depth), self.0.to_string(0))
    }
}

impl Unparse for IfStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        let mut stmts = String::new();
        for stmt in &self.stmts {
            stmts.push_str(&stmt.to_string(depth+1));
            stmts.push('\n');
        }
        format!("\n{}if ({}) {{\n{}{}}}\n", 
            "    ".repeat(depth),
            self.exp.to_string(0),
            stmts,
            "    ".repeat(depth),
        )
    }
}

impl Unparse for IfElseStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        let mut true_stmts = String::new();
        for stmt in &self.true_stmts {
            true_stmts.push_str(&stmt.to_string(depth+1));
            true_stmts.push('\n');
        }
        let mut else_stmts = String::new();
        for stmt in &self.else_stmts {
            else_stmts.push_str(&stmt.to_string(depth+1));
            else_stmts.push('\n');
        }
        format!("\n{}if ({}) {{\n{}{}}} else {{\n{}{}}}\n",
            "    ".repeat(depth),
            self.exp.to_string(0),
            true_stmts,
            "    ".repeat(depth),
            else_stmts,
            "    ".repeat(depth),
        )
    }
}

impl Unparse for ReturnStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        match &self.0 {
            Some(exp) => format!("{}return {};", 
                "    ".repeat(depth),
                exp.to_string(0),
            ),
            None => format!("{}return;", "    ".repeat(depth)),
        }
    }
}

impl Unparse for WhileStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        let mut stmts = String::new();
        for stmt in &self.stmts {
            stmts.push_str(&stmt.to_string(depth+1));
            stmts.push('\n');
        }
        format!("\n{}while ({}) {{\n{}{}}}\n",
            "    ".repeat(depth),
            self.exp.to_string(0),
            stmts,
            "    ".repeat(depth),
        )
    }
}

impl Unparse for PostIncStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        format!("{}{}++;", "    ".repeat(depth), self.0.to_string(0))
    }
}

impl Unparse for PostDecStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        format!("{}{}--;", "    ".repeat(depth), self.0.to_string(0))
    }
}

impl Unparse for AssignStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        format!("{}{} = {};", 
            "    ".repeat(depth),
            self.0.lval.to_string(0), 
            self.0.exp.to_string(0),
        )
    }
}

impl Unparse for ReadStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        format!("{}read {};", 
            "    ".repeat(depth),
            self.0.to_string(0),
        )
    }
}

impl Unparse for WriteStmtNode<'_> {
    fn to_string(&self, depth: usize) -> String {
        format!("{}write {};", 
            "    ".repeat(depth),
            self.0.to_string(0),
        )
    }
}

impl<'a> LValNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        let fst = inner_pairs.next().unwrap();
        match fst.as_rule() {
            Rule::AT => Deref(DerefNode(
                IDNode::from(inner_pairs.next().unwrap())
            )),
            Rule::id => ID(IDNode::from(fst)),
            _ => unreachable!(),
        }
    }
}

impl<'a> DeclNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        let inner_pair = pair.into_inner().next().unwrap();
        match inner_pair.as_rule() {
            Rule::varDecl => VarDecl(VarDeclNode::from(inner_pair)),
            Rule::fnDecl => FnDecl(FnDeclNode::from(inner_pair)),
            _ => unreachable!(),
        }
    }
}

// lifetimes elised here
// pub fn generate_stmt_list<'a>(pair: Pair<'a, Rule>) -> Vec<StmtNode<'a>>
pub fn generate_stmt_list(pair: Pair<Rule>) -> Vec<StmtNode> {
    let mut vec: Vec<StmtNode> = Vec::new();
    for p in pair.into_inner() {
        vec.push(StmtNode::from(p));
    }
    vec
}

impl<'a> StmtNode<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Self {
        let mut inner_pairs = pair.into_inner();
        let fst = inner_pairs.next().unwrap();
        match fst.as_rule() {
            Rule::varDecl => Decl(VarDecl(VarDeclNode::from(fst))),
            Rule::assignExp => AssignStmt(
                AssignStmtNode(Box::new(AssignExpNode::from(fst)))
            ),
            Rule::lval => match inner_pairs.next().unwrap().as_rule() {
                Rule::DEC => PostDecStmt(
                    PostDecStmtNode(LValNode::from(fst))
                ),
                Rule::INC => PostIncStmt(
                    PostIncStmtNode(LValNode::from(fst))
                ),
                _ => unreachable!(),
            },
            Rule::READ => ReadStmt(
                ReadStmtNode(LValNode::from(inner_pairs.next().unwrap()))
            ),
            Rule::WRITE => WriteStmt(
                WriteStmtNode(
                    Box::new(ExpNode::from(inner_pairs.next().unwrap()))
                )
            ),
            Rule::IF => {
                let exp = Box::new(
                    ExpNode::from(inner_pairs.nth(1).unwrap())
                );
                let true_stmts = generate_stmt_list(
                    inner_pairs.nth(2).unwrap()
                );
                let maybe_else = inner_pairs.nth(1);
                if let Some(_) = maybe_else {
                    let else_stmts = generate_stmt_list(
                        inner_pairs.nth(1).unwrap()
                    );
                    IfElseStmt(
                        IfElseStmtNode { exp, true_stmts, else_stmts }
                    )
                } else {
                    IfStmt(IfStmtNode { exp, stmts: true_stmts })
                }
            }
            Rule::WHILE => {
                let exp = Box::new(
                    ExpNode::from(inner_pairs.nth(1).unwrap())
                );
                let stmts = generate_stmt_list(inner_pairs.nth(2).unwrap());
                WhileStmt(WhileStmtNode { exp, stmts })
            }
            Rule::RETURN => {
                let maybe_exp = inner_pairs.next().unwrap();
                match maybe_exp.as_rule() {
                    Rule::exp => ReturnStmt(
                        ReturnStmtNode(
                            Some(Box::new(ExpNode::from(maybe_exp)))
                        )
                    ),
                    _ => ReturnStmt(ReturnStmtNode(None)),
                }
            }
            Rule::callExp => CallStmt(CallStmtNode(CallExpNode::from(fst))),
            _ => unreachable!(),
        }
    }
}
