pub mod unparse;

use crate::peg::{Rule, CMMParser};
use pest::{Parser, iterators::{Pair, Pairs}};
use std::io::{self, Write};
use std::process;
use crate::ast::*;
pub use unparse::unparse;

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

pub fn generate_tree(pair: Pair<Rule>) -> ASTNode {
    match pair.as_rule() {
        Rule::program => {
            let mut decls: Vec<DeclNode> = Vec::new();
            for p in pair.into_inner() {
                // unwrap the value, since generate_tree returns an ASTNode
                // which is too generic
                if let Rule::EOI = p.as_rule() {}
                else {
                    decls.push(
                        generate_tree(p)
                            .into_stmt_node()
                            .unwrap()
                            .into_decl_node()
                            .unwrap()
                    );
                }
            }
            ASTNode::ProgramNode(ProgramNode(decls))
        }
        Rule::id => {
            ASTNode::ExpNode(
                ExpNode::LValNode(
                    LValNode::IDNode(
                        IDNode(pair.as_str().to_string())
                    )
                )
            )
        }
        Rule::decl => {
           let inner = pair.into_inner().next().unwrap();
           generate_tree(inner)
        }
        Rule::varDecl => {
            let mut inner = pair.into_inner();
            // get the type first
            let type_node = generate_tree(inner.next().unwrap())
                .into_type_node()
                .unwrap();
            // then get the id
            let id_node = generate_tree(inner.next().unwrap())
                .into_exp_node()
                .unwrap()
                .into_l_val_node()
                .unwrap()
                .into_id_node()
                .unwrap();
            // put them together into an ASTNode
            ASTNode::StmtNode(
                StmtNode::DeclNode(
                    DeclNode::VarDeclNode(
                        VarDeclStruct {
                            typ: type_node,
                            id: id_node,
                        }
                    )
                )
            )
        }
        Rule::Type => {
            let mut inner = pair.into_inner();
            let maybe_ptr_pair = inner.next().unwrap();
            if let Rule::PTR = maybe_ptr_pair.as_rule() {
                let prim_type = inner.next()
                    .unwrap();
                let type_node = generate_tree(prim_type)
                    .into_type_node()
                    .unwrap();
                ASTNode::TypeNode(
                    TypeNode::PtrTypeNode(
                        PtrTypeNode(Box::new(type_node))
                    )
                )
            } else {
                generate_tree(maybe_ptr_pair)
            }
        }
        Rule::primType => {
            let prim_type_node = {
                match pair.into_inner().next().unwrap().as_rule() {
                    Rule::INT => TypeNode::IntTypeNode,
                    Rule::BOOL => TypeNode::BoolTypeNode,
                    Rule::STRING => TypeNode::StringTypeNode,
                    Rule::SHORT => TypeNode::ShortTypeNode,
                    Rule::VOID => TypeNode::VoidTypeNode,
                    _ => unreachable!(),
                }
            };
            ASTNode::TypeNode(prim_type_node)
        }
        Rule::fnDecl => {
            let mut pairs = pair.into_inner();
            let type_pair = pairs.next().unwrap();
            let id_pair = pairs.next().unwrap();
            pairs.next();
            let maybe_formals_pair = pairs.next().unwrap();
            let stmt_list_pair;
            let maybe_formals_pair = {
                if let Rule::formals = maybe_formals_pair.as_rule() {
                    stmt_list_pair = pairs.nth(2).unwrap();
                    Some(maybe_formals_pair)
                } else {
                    stmt_list_pair = pairs.nth(1).unwrap();
                    None
                }
            };
            let typ = generate_tree(type_pair)
                .into_type_node()
                .unwrap();
            let id = generate_tree(id_pair)
                .into_exp_node()
                .unwrap()
                .into_l_val_node()
                .unwrap()
                .into_id_node()
                .unwrap();
            ASTNode::StmtNode(
                StmtNode::DeclNode(
                    DeclNode::FnDeclNode(
                        FnDeclNode {
                            typ: Box::new(typ),
                            id,
                            formals: match maybe_formals_pair {
                                Some(formals_pair) => {
                                    let mut vec: Vec<VarDeclStruct>
                                        = Vec::new();
                                    for formal_pair in formals_pair.into_inner() {
                                        if let Rule::formalDecl
                                            = formal_pair.as_rule()
                                        {
                                            vec.push(
                                                generate_tree(formal_pair)
                                                    .into_stmt_node()
                                                    .unwrap()
                                                    .into_decl_node()
                                                    .unwrap()
                                                    .into_formal_decl_node()
                                                    .unwrap()
                                            );
                                        }
                                    }
                                    vec
                                }
                                None => Vec::new(),
                            },
                            stmts: {
                                let mut vec: Vec<StmtNode> = Vec::new();
                                for stmt in stmt_list_pair.into_inner() {
                                        vec.push(
                                            generate_tree(stmt)
                                                .into_stmt_node()
                                                .unwrap()
                                        );
                                }
                                vec
                            },
                        // end struct
                        }
                    // end DeclNode
                    )
                // end StmtNode
                )
            // end ASTNode
            )
        // end case Rule::fnDecl
        }
        Rule::formalDecl => {
            let mut pairs = pair.into_inner();
            let type_pair = pairs.next().unwrap();
            let id_pair = pairs.next().unwrap();
            let typ = generate_tree(type_pair)
                .into_type_node()
                .unwrap();
            let id = generate_tree(id_pair)
                .into_exp_node()
                .unwrap()
                .into_l_val_node()
                .unwrap()
                .into_id_node()
                .unwrap();
            ASTNode::StmtNode(
                StmtNode::DeclNode(
                    DeclNode::FormalDeclNode(
                        VarDeclStruct {
                            typ,
                            id,
                        }
                    )
                )
            )
        }
        Rule::stmt => {
            generate_stmt_tree(pair)
        }
        Rule::assignExp => ASTNode::ExpNode(generate_exp_tree(pair)),
        Rule::exp => generate_tree(pair.into_inner().next().unwrap()),
        Rule::boolExp => ASTNode::ExpNode(generate_exp_tree(pair)),
        Rule::lval => {
            let mut inner = pair.into_inner();
            let maybe_at = inner.next().unwrap();
            if let Rule::AT = maybe_at.as_rule() {
                let id = generate_tree(inner.next().unwrap())
                    .into_exp_node()
                    .unwrap()
                    .into_l_val_node()
                    .unwrap()
                    .into_id_node()
                    .unwrap();
                ASTNode::ExpNode(
                    ExpNode::LValNode(
                        LValNode::DerefNode(
                            DerefNode(id)
                        )
                    )
                )
            } else {
                generate_tree(maybe_at)
            }
        }
        _ => unreachable!(),
    // end match
    }
// end function
}

fn generate_stmt_tree(pair: Pair<Rule>) -> ASTNode {
    let mut pairs = pair.into_inner();
    let fst = pairs.next().unwrap();
    match fst.as_rule() {
        Rule::varDecl => generate_tree(fst),
        Rule::assignExp => ASTNode::StmtNode(
            StmtNode::AssignStmtNode(
                AssignStmtNode(
                    Box::new(
                        generate_tree(fst)
                            .into_exp_node()
                            .unwrap()
                            .into_assign_exp_node()
                            .unwrap()
                    )
                )
            )
        ),
        Rule::lval => {
            let snd = pairs.next().unwrap();
            let lval = generate_tree(fst)
                .into_exp_node()
                .unwrap()
                .into_l_val_node()
                .unwrap();
            match snd.as_rule() {
                Rule::DEC => {
                    ASTNode::StmtNode(
                        StmtNode::PostDecStmtNode(
                            PostIncStmtStruct(lval)
                        )
                    )
                }
                Rule::INC => {
                    ASTNode::StmtNode(
                        StmtNode::PostIncStmtNode(
                            PostIncStmtStruct(lval)
                        )
                    )
                }
                _ => unreachable!(),
            }
        }
        Rule::READ => {
            let snd = pairs.next().unwrap();
            let lval = generate_tree(snd)
                .into_exp_node()
                .unwrap()
                .into_l_val_node()
                .unwrap();
            ASTNode::StmtNode(
                StmtNode::ReadStmtNode(
                    ReadStmtNode(lval)
                )
            )
        }
        Rule::WRITE => {
            let snd = pairs.next().unwrap();
            ASTNode::StmtNode(
                StmtNode::WriteStmtNode(
                    WriteStmtNode(
                        Box::new(
                            generate_tree(snd)
                                .into_exp_node()
                                .unwrap()
                        )
                    )
                )
            )
        }
        Rule::IF => {
            let exp_pair = pairs.nth(1).unwrap();
            let stmt_list_pair = pairs.nth(2).unwrap();
            let stmts = {
                let mut vec: Vec<StmtNode> = Vec::new();
                for stmt in stmt_list_pair.into_inner() {
                    vec.push(
                        generate_tree(stmt)
                            .into_stmt_node()
                            .unwrap()
                    );
                }
                vec
            };
            let maybe_else_stmts = pairs.nth(3);
            match maybe_else_stmts {
                None => {
                    ASTNode::StmtNode(
                        StmtNode::IfStmtNode(
                            IfStmtNode{
                                exp: Box::new(
                                         generate_tree(exp_pair)
                                            .into_exp_node()
                                            .unwrap()
                                ),
                                stmts,
                            }
                        )
                    )
                }
                Some(else_stmt_pair) => {
                    let else_stmts = {
                        let mut vec: Vec<StmtNode> = Vec::new();
                        for stmt in else_stmt_pair.into_inner() {
                            if let ASTNode::StmtNode(s) = generate_tree(stmt) {
                                vec.push(s);
                            }
                        }
                        vec
                    };
                    ASTNode::StmtNode(
                        StmtNode::IfElseStmtNode(
                            IfElseStmtNode{
                                exp: Box::new(
                                         generate_tree(exp_pair)
                                            .into_exp_node()
                                            .unwrap()
                                ),
                                true_stmts: stmts,
                                else_stmts,
                            }
                        )
                    )
                }
            }
        }
        Rule::WHILE => {
            let exp_pair = pairs.nth(1).unwrap();
            let stmt_list_pair = pairs.nth(2).unwrap();
            let stmts = {
                let mut vec: Vec<StmtNode> = Vec::new();
                for stmt in stmt_list_pair.into_inner() {
                    vec.push(
                        generate_tree(stmt)
                            .into_stmt_node()
                            .unwrap()
                    );
                }
                vec
            };
            ASTNode::StmtNode(
                StmtNode::WhileStmtNode(
                    WhileStmtNode{
                        exp: Box::new(
                                 generate_tree(exp_pair)
                                    .into_exp_node()
                                    .unwrap()
                        ),
                        stmts,
                    }
                )
            )
        }
        Rule::RETURN => {
            let snd = pairs.next().unwrap();
            let ret = {
                if let Rule::exp = snd.as_rule() {
                    ReturnStmtNode(
                        Some(
                            Box::new(
                                generate_tree(snd)
                                    .into_exp_node()
                                    .unwrap()
                            )
                        )
                    )
                } else {
                    ReturnStmtNode(None)
                }
            };
            ASTNode::StmtNode(StmtNode::ReturnStmtNode(ret))
        }
        Rule::callExp => ASTNode::StmtNode(
            StmtNode::CallStmtNode(
                CallStmtNode(
                    Box::new(
                        generate_exp_tree(fst)
                            .into_call_exp_node()
                            .unwrap()
                    )
                )
            )
        ),
        _ => unreachable!(),
    }
}

fn generate_exp_tree(pair: Pair<Rule>) -> ExpNode {
    match pair.as_rule() {
        Rule::exp => generate_exp_tree(pair.into_inner().next().unwrap()),
        Rule::boolExp
        | Rule::logicTerm
        | Rule::compareExp
        | Rule::sum
        | Rule::term => {
            let mut pairs = pair.into_inner();
            generate_bin_tree_rec(
                generate_exp_tree(pairs.next().unwrap()),
                pairs,
            )
        }
        Rule::factor => {
            let mut inner = pair.into_inner();
            let fst = inner.next().unwrap();
            match fst.as_rule() {
                Rule::NOT => ExpNode::UnaryExpNode(
                    UnaryExpNode {
                        op: UnaryOp::Not,
                        exp: Box::new(
                            generate_exp_tree(inner.next().unwrap())
                        ),
                    }
                ),
                Rule::assignExp => generate_exp_tree(fst),
                _ => generate_minus_tree(fst, inner),
            }
        }
        Rule::assignExp => {
            let mut inner = pair.into_inner();
            let lval = generate_tree(inner.next().unwrap())
                .into_exp_node()
                .unwrap()
                .into_l_val_node()
                .unwrap();
            let exp = generate_exp_tree(inner.nth(1).unwrap());
            ExpNode::AssignExpNode(
                AssignExpNode {
                    lval,
                    exp: Box::new(exp),
                }
            )
        }
        Rule::callExp => {
            let mut inner = pair.into_inner();
            let id = generate_tree(inner.next().unwrap())
                .into_exp_node()
                .unwrap()
                .into_l_val_node()
                .unwrap()
                .into_id_node()
                .unwrap();
            let mut vec: Vec<ExpNode> = Vec::new();
            let maybe_actuals = inner.nth(1).unwrap();
            if let Rule::actualsList = maybe_actuals.as_rule() {
                for pair in maybe_actuals.into_inner() {
                    if let Rule::exp = pair.as_rule() {
                        vec.push(generate_exp_tree(pair));
                    }
                }
            }
            ExpNode::CallExpNode(
                CallExpNode {
                    id,
                    args: vec,
                }
            )
        }
        _ => unreachable!(),
    }
}

fn generate_minus_tree(
    fst: Pair<Rule>, 
    mut remaining: Pairs<Rule>
) -> ExpNode {
    match fst.as_rule() {
        Rule::MINUS => ExpNode::UnaryExpNode(
            UnaryExpNode {
                op: UnaryOp::Neg,
                exp: Box::new(
                    generate_minus_tree(
                        remaining.next().unwrap(),
                        remaining
                    )
                ),
            }
        ),
        Rule::INTLITERAL => ExpNode::IntLitNode(
            IntLitNode(
                fst.as_str().parse().unwrap()
            )
        ),
        Rule::SHORTLITERAL => ExpNode::ShortLitNode(
            ShortLitNode(
                {
                    let lexeme = fst.as_str();
                    let len = lexeme.len();
                    lexeme[..(len-1)].parse().unwrap()
                }
            )
        ),
        Rule::STRLITERAL => ExpNode::StrLitNode(
            StrLitNode(fst.as_str().to_string())
        ),
        Rule::TRUE => ExpNode::TrueNode(TrueNode),
        Rule::FALSE => ExpNode::FalseNode(FalseNode),
        Rule::LPAREN => generate_exp_tree(
            remaining.next().unwrap()
        ),
        Rule::AMP => ExpNode::UnaryExpNode(
            UnaryExpNode {
                op: UnaryOp::Ref,
                exp: Box::new(
                    generate_tree(remaining.next().unwrap())
                        .into_exp_node()
                        .unwrap()
                ),
            }
        ),
        Rule::callExp => generate_exp_tree(fst),
        Rule::lval => generate_tree(fst).into_exp_node().unwrap(),
        _ => unreachable!(),
    }
}

fn generate_bin_tree_rec(
    tree: ExpNode, 
    mut pairs: Pairs<Rule>
) -> ExpNode {
    let lhs = tree;
    if let Some(pair) = pairs.next() {
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
        let rhs = generate_exp_tree(pairs.next().unwrap());
        generate_bin_tree_rec(
            ExpNode::BinaryExpNode(
                BinaryExpNode {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op,
                }
            ),
            pairs
        )
    } else {
        lhs
    }
}


