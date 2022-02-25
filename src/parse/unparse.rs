use crate::ast::*;
use std::io::{self, Write};
use std::process;
use std::fs::File;
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
pub trait Unparse {
    fn unparse(&self) -> &'static str;
}

impl Unparse for BinaryExpNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for IDNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for UnaryExpNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for DerefNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for AssignExpNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for VarDeclStruct {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for FnDeclNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for CallExpNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for IntLitNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for ShortLitNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for StrLitNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for TrueNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for FalseNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for CallStmtNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for IfStmtNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for IfElseStmtNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for ReturnStmtNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for WhileStmtNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for PostIncStmtStruct {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for AssignStmtNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for ReadStmtNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for WriteStmtNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for PtrTypeNode {
    fn unparse(&self) -> &'static str {""}
}

impl Unparse for ProgramNode {
    fn unparse(&self) -> &'static str {""}
}


pub fn unparse(tree: &ASTNode, mut output: File) {
    if let Err(_) = writeln!(output, "{}", unparse_rec(tree)) {
        writeln!(io::stderr(), "Error writing to file").unwrap();
        process::exit(1);
    }
}

pub fn unparse_rec(tree: &ASTNode) -> String {
    println!("{:?}", tree);
    return match tree {
        ASTNode::ProgramNode(prog_node) => {
            let mut ret = String::new();
            let decl_list = &prog_node.0;
            for node in decl_list {
                ret.push_str(
                    &unparse_rec(
                        &ASTNode::StmtNode(
                            StmtNode::DeclNode(node.clone())
                        )
                    )
                );
                ret.push('\n');
            }
            ret
        }
        ASTNode::ExpNode(n) => {
            match n {
                ExpNode::LValNode(n) => {
                    match n {
                        LValNode::IDNode(id) => id.0.clone(),
                        _ => "".to_string(),
                    }
                }
                _ => "".to_string(),
            }
        }
        ASTNode::StmtNode(n) => {
            match n {
                StmtNode::DeclNode(_) => "generic decl".to_string(),
                _ => "".to_string(),
            }
        }
        _ => "".to_string(),
    };
}

