// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use crate::name::symbol::{Symbol, SymbolType, SymbolType::*};
use crate::ast::{*, ExpNode::*, LValNode::*};
use crate::error::error;
use std::process;
use std::io::{self, Write};
use enum_dispatch::enum_dispatch;

pub fn type_check(tree: &mut ProgramNode) {
    if let Err(()) = tree.type_check(Void) {
        writeln!(io::stderr(), "Type Analysis Failed").unwrap();
        process::exit(1);
    }
}

#[enum_dispatch(DeclNode, StmtNode, ExpNode, LValNode)]
pub trait TypeCheck {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()>;
}

impl<'a> TypeCheck for ProgramNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
       type_check_list(&self.0, return_type)
    }
}

impl<'a> TypeCheck for AssignExpNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!()
    }
}

impl<'a> TypeCheck for UnaryExpNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!() 
    }
}

impl<'a> TypeCheck for BinaryExpNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        match self.op {
            BinaryOperator::Plus
            | BinaryOperator::Minus
            | BinaryOperator::Times
            | BinaryOperator::Divide => {
                match self.lhs.type_check(return_type.clone()) {
                    Ok(Int) => match self.rhs.type_check(return_type) {
                        Ok(Int) | Ok(Short) => Ok(Int), // Short promotes to int
                        Ok(Ptr(t)) => Ok(Ptr(t)),
                        Err(()) => Err(()), // Avoid generating multiple errors
                        _ => {
                            error(
                                &self.rhs.get_pos(),
                                "Arithmetic operator applied to invalid\
                                operand",
                            );
                            Err(())
                        }
                    }
                    Ok(Short) => match self.rhs.type_check(return_type) {
                        Ok(Short) => Ok(Short),
                        Ok(Int) => Ok(Int),
                        Ok(Ptr(t)) => Ok(Ptr(t)),
                        Err(()) => Err(()),
                        _ => {
                            error(
                                &self.rhs.get_pos(),
                                "Arithmetic operator applied to invalid\
                                operand",
                            );
                            Err(())
                        }
                    }
                    Ok(Ptr(t)) => match self.rhs.type_check(return_type) {
                        Ok(Int) | Ok(Short) => Ok(Ptr(t)),
                        Ok(Ptr(_)) => todo!("Report some error here"),
                        Err(()) => Err(()),
                        _ => {
                            error(
                                &self.rhs.get_pos(),
                                "Arithmetic operator applied to invalid\
                                operand",
                            );
                            Err(())
                        }
                    }
                    Err(()) => match self.rhs.type_check(return_type) {
                        Ok(Int) | Ok(Short) | Ok(Ptr(_)) | Err(()) => Err(()),
                        _ => {
                            error(
                                &self.rhs.get_pos(),
                                "Arithmetic operator applied to invalid\
                                operand",
                            );
                            Err(())
                        }
                    }
                    _ => {
                        error(
                            &self.lhs.get_pos(),
                            "Arithmetic operator applied to invalid operand",
                        );
                        match self.rhs.type_check(return_type) {
                            Ok(Ptr(_)) | Ok(Short) | Ok(Int) | Err(()) => (),
                            _ => error(
                                &self.rhs.get_pos(),
                                "Arithmetic operator applied to invalid\
                                operand",
                            ),
                        }
                        Err(())
                    }
                }
            }
            BinaryOperator::Less
            | BinaryOperator::LessEq
            | BinaryOperator::Greater
            | BinaryOperator::GreaterEq => {
                match self.lhs.type_check(return_type.clone()) {
                    Ok(Ptr(_)) => match self.rhs.type_check(return_type) {
                        Ok(Ptr(_)) => Ok(Bool),
                        Err(()) => Err(()),
                        _ => {
                            error(
                                &self.rhs.get_pos(),
                                "Relational operator applied to non-numeric\
                                operand",
                            );
                            Err(())
                        }
                    }
                    Ok(Int) | Ok(Short) => {
                        match self.rhs.type_check(return_type) {
                            Ok(Int) | Ok(Short) => Ok(Bool),
                            Err(()) => Err(()),
                            _ => {
                                error(
                                    &self.rhs.get_pos(),
                                    "Relational operator applied to non-numeric\
                                    operand",
                                );
                                Err(())
                            }
                        }
                    }
                    Err(()) => match self.rhs.type_check(return_type) {
                        Ok(Ptr(_)) | Ok(Int) | Ok(Short) | Err(()) => Err(()),
                        _ => {
                            error(
                                &self.rhs.get_pos(),
                                "Relational operator applied to non-numeric\
                                operand",
                            );
                            Err(())
                        }
                    }
                    _ => {
                        error(
                            &self.lhs.get_pos(),
                            "Arithmetic operator applied to invalid operand",
                        );
                        match self.rhs.type_check(return_type) {
                            Ok(Ptr(_)) | Ok(Short) | Ok(Int) | Err(()) => (),
                            _ => error(
                                &self.rhs.get_pos(),
                                "Arithmetic operator applied to invalid\
                                operand",
                            ),
                        }
                        Err(())
                    }
                }
            }
            BinaryOperator::And | BinaryOperator::Or => {
                match self.lhs.type_check(return_type.clone()) {
                    Ok(Bool) => match self.rhs.type_check(return_type) {
                        Ok(Bool) => Ok(Bool),
                        Err(()) => Err(()),
                        _ => {
                            error(
                                &self.rhs.get_pos(),
                                "Logical operator applied to non-bool operand"
                            );
                            Err(())
                        }
                    }
                    Err(()) => match self.rhs.type_check(return_type) {
                        Ok(Bool) | Err(()) => Err(()),
                        _ => {
                            error(
                                &self.rhs.get_pos(),
                                "Logical operator applied to non-bool operand"
                            );
                            Err(())
                        }
                    }
                    _ => {
                        error(
                            &self.lhs.get_pos(),
                            "Logical operator applied to non-bool operand"
                        );
                        match self.rhs.type_check(return_type) {
                            Ok(Bool) | Err(()) => (),
                            _ => error(
                                &self.rhs.get_pos(),
                                "Logical operator applied to non-bool operand"
                            ),
                        }
                        Err(())
                    }
                }
            }
            BinaryOperator::Equals
            | BinaryOperator::NotEquals => {
                match self.lhs.type_check(return_type.clone()) {
                    Ok(Int) | Ok(Short) => {
                        match self.rhs.type_check(return_type) {
                            Ok(Int) | Ok(Short) => Ok(Bool),
                            Err(()) => Err(()),
                            Ok(Fn{ .. }) | Ok(Void) => {
                                error(
                                    &self.rhs.get_pos(),
                                    "Invalid equality operand",
                                );
                                Err(())
                            }
                            _ => {
                                error(
                                    &self.rhs.get_pos(),
                                    "Invalid equality operation",
                                );
                                Err(())
                            }
                        }
                    }
                    Err(()) => match self.rhs.type_check(return_type) {
                        Ok(Fn{ .. }) | Ok(Void) => {
                            error(
                                &self.rhs.get_pos(),
                                "Invalid equality operand",
                            );
                            Err(())
                        }
                        _ => Err(()), // here we can't tell if this would be an
                        // invalid equality operation, because the lhs is the
                        // error type. So we just don't report anything.
                    }
                    Ok(Fn{ .. }) | Ok(Void) => {
                        error(
                            &self.lhs.get_pos(),
                            "Invalid equality operand",
                        );
                        match self.rhs.type_check(return_type) {
                            Ok(Fn{ .. }) | Ok(Void) => error(
                                &self.rhs.get_pos(),
                                "Invalid equality operand",
                            ),
                            _ => (),
                        }
                        Err(())
                    }
                    Ok(Ptr(_)) => match self.rhs.type_check(return_type) {
                        Ok(Ptr(_)) => Ok(Bool),
                        Ok(Fn{ .. }) | Ok(Void) => {
                            error(
                                &self.rhs.get_pos(),
                                "Invalid equality operand",
                            );
                            Err(())
                        }
                        Err(()) => Err(()),
                        _ => {
                            error(
                                &self.rhs.get_pos(),
                                "Invalid equality operation",
                            );
                            Err(())
                        }
                    }
                }
            }
            _ => todo!()
        }
    }
}

impl<'a> TypeCheck for CallExpNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        if let Fn { args, ret } = &self.id.symbol.as_ref().unwrap().typ {
            if self.args.len() != args.len() {
                error(&self.pos, "Function call with wrong number of args");
                return Err(());
            }
            let args_match = self
                .args
                .iter()
                .zip(args.iter())
                .fold(
                    true,
                    |acc, (act, form)| {
                        if act.type_check(Void).unwrap() != *form {
                            error(
                                &act.get_pos(),
                                "Type of actual does not match type of formal",
                            );
                            false
                        } else {
                            acc && true
                        }
                    },
                );
            if !args_match {
                return Err(());
            }
            Ok(*ret.clone())
        } else {
            error(&self.id.pos, "Attempt to call a non-function");
            Err(())
        }
    }
}

impl<'a> TypeCheck for IDNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(self.symbol.as_ref().unwrap().typ.clone())
    }
}

impl<'a> TypeCheck for DerefNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Ptr(Box::new(self.id.type_check(return_type).unwrap())))
    }
}

impl TypeCheck for TrueNode {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Bool)
    }
}

impl TypeCheck for FalseNode {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Bool)
    }
}

impl TypeCheck for IntLitNode {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Int)
    }
}

impl TypeCheck for ShortLitNode {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Short)
    }
}

impl<'a> TypeCheck for StrLitNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!()
    }
}

impl<'a> TypeCheck for AssignStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!()
    }
}

impl<'a> TypeCheck for CallStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        self.exp.type_check(return_type.clone())
    }
}

impl<'a> TypeCheck for FnDeclNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        // No need to check the formals. But we do need to typecheck everything
        // in the function body. We also need to set the return type here.
        type_check_list(&self.stmts, self.typ.as_symbol_type())
    }
}

impl<'a> TypeCheck for VarDeclNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        // It's confusing, but the result of declaring a variable is
        // Void, or at least we don't really care about it. There will never
        // be errors to report here, declaring a variable of any type is always
        // a valid operation.
        Ok(Void)
    }
}

impl<'a> TypeCheck for FormalDeclNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        // We sanitized formals (i.e. made sure they're not void) in name
        // analysis, so there's no type checking to do here.
        Ok(Void)
    }
}

impl<'a> TypeCheck for IfStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        self.exp
            .type_check(return_type.clone())
            .and(type_check_list(&self.stmts, return_type.clone()))
    }
}

impl<'a> TypeCheck for IfElseStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        self.exp
            .type_check(return_type.clone())
            .and(type_check_list(&self.true_stmts, return_type.clone()))
            .and(type_check_list(&self.else_stmts, return_type.clone()))
    }
}

impl<'a> TypeCheck for WhileStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
       self.exp
           .type_check(return_type.clone())
           .and(type_check_list(&self.stmts, return_type.clone()))
    }
}

impl<'a> TypeCheck for PostIncStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!()
    }
}

impl<'a> TypeCheck for PostDecStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!()
    }
}

impl<'a> TypeCheck for ReadStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        if let ID(id_node) = &self.lval {
            if let Fn { .. } = &id_node.symbol.as_ref().unwrap().typ {
                error(
                    &id_node.pos, 
                    "Attempt to assign user input to function",
                );
                return Err(())
            }
        }
        self.lval.type_check(return_type)
    }
}

impl<'a> TypeCheck for WriteStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        if let LVal(ID(id_node)) = &self.exp { 
            // Here we unwrap. The important precondition is that name analysis
            // has already been run and the symbol is populated.
            if let Fn { .. } = &id_node.symbol.as_ref().unwrap().typ {
                error(&id_node.pos, "Attempt to output a function");
                return Err(())
            }
        } else if let CallExp(CallExpNode { id, .. }) = &self.exp {
            if let Fn { args: _, ret } = &id.symbol.as_ref().unwrap().typ {
                if let Void = **ret {
                    error(&id.pos, "Attempt to write void");
                    return Err(())
                }
            }
        }
        self.exp.type_check(return_type)
    }
}

impl<'a> TypeCheck for ReturnStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!()
    }
}

fn type_check_list<T: TypeCheck>(
    list: &Vec<T>,
    return_type: SymbolType,
) -> Result<SymbolType, ()> {
    list.iter().fold(
        Ok(Void), |acc, item| acc.and(item.type_check(return_type.clone()))
    )
}

