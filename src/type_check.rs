// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt) EXCEPT FOR "The View From Halfway Down" from Bojack
// Horseman.

use crate::ast::*;
use crate::error::error;
use crate::name::symbol::{SymbolType, SymbolType::*};
use enum_dispatch::enum_dispatch;
use std::io::{self, Write};
use std::process;

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
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        type_check_list(&self.0, Void)
    }
}

impl<'a> TypeCheck for AssignExpNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        let lval_type = self.lval.type_check(Void);
        let exp_type = self.exp.type_check(Void);
        match (lval_type, exp_type) {
            (t1 @ Ok(Fn { .. } | Str), t2) | (t1, t2 @ Ok(Fn { .. } | Str | Void)) => {
                let t1_res = match t1 {
                    Ok(Fn { .. } | Str) => {
                        error(&self.lval.get_pos(), "Invalid assignment operand");
                        Err(())
                    }
                    _ => t1,
                };
                let t2_res = match t2 {
                    Ok(Fn { .. } | Str | Void) => {
                        error(&self.exp.get_pos(), "Invalid assignment operand");
                        Err(())
                    }
                    _ => t2,
                };
                t1_res.and(t2_res)
            }
            (Ok(t1), Ok(t2)) => {
                if t1 == t2 {
                    Ok(t1)
                } else if let (Int, Short) = (t1, t2) {
                    Ok(Int)
                } else {
                    error(&self.get_pos(), "Invalid assignment operation");
                    Err(())
                }
            }
            (Err(()), _) | (_, Err(())) => Err(()),
        }
    }
}

impl<'a> TypeCheck for UnaryExpNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        match &self.op {
            UnaryOp::Neg => match self.exp.type_check(Void) {
                t @ (Ok(Short) | Ok(Int) | Err(())) => t,
                _ => {
                    error(
                        &self.exp.get_pos(),
                        "Arithmetic operator applied to invalid operand",
                    );
                    Err(())
                }
            },
            UnaryOp::Not => match self.exp.type_check(Void) {
                t @ (Ok(Bool) | Err(())) => t,
                _ => {
                    error(
                        &self.exp.get_pos(),
                        "Logical operator applied to non-bool operand",
                    );
                    Err(())
                }
            },
            UnaryOp::Ref => match self.exp.type_check(Void) {
                Ok(Void | Fn { .. } | Ptr(_)) => {
                    error(&self.exp.get_pos(), "Invalid ref operand");
                    Err(())
                }
                Err(()) => Err(()),
                Ok(t) => Ok(Ptr(Box::new(t))),
            },
        }
    }
}

impl<'a> TypeCheck for BinaryExpNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        let (lhs, rhs) = (
            self.lhs.type_check(return_type.clone()),
            self.rhs.type_check(return_type),
        );
        match self.op {
            BinaryOperator::And | BinaryOperator::Or => match (lhs, rhs) {
                (Ok(Bool), Ok(Bool)) => Ok(Bool),
                (lhs, rhs) => {
                    match lhs {
                        Ok(Bool) => (),
                        _ => error(
                            &self.lhs.get_pos(),
                            "Logical operator applied to non-bool \
                                operand",
                        ),
                    }
                    match rhs {
                        Ok(Bool) => (),
                        _ => error(
                            &self.rhs.get_pos(),
                            "Logical operator applied to non-bool \
                                operand",
                        ),
                    }
                    Err(())
                }
            },
            BinaryOperator::Plus
            | BinaryOperator::Minus
            | BinaryOperator::Times
            | BinaryOperator::Divide => {
                match (lhs, rhs) {
                    (Ok(Short), Ok(Short)) => Ok(Short),
                    (Ok(Int | Short), Ok(Int | Short)) => Ok(Int),
                    (Ok(Ptr(t)), Ok(Int | Short)) | (Ok(Int | Short), Ok(Ptr(t))) => {
                        Ok(Ptr(t.clone()))
                    }
                    (Ok(Ptr(_)), Ok(Ptr(_))) => {
                        // The correct error isn't defined in the spec, so I
                        // think this is a reasonable thing to do.
                        error(&self.get_pos(), "Invalid arithmetic operation");
                        // Also... NASAL DEMONS!!!
                        println!(
                            "\t\tSECRETARIAT\n\
                            \t\tA poem. Original, obviously. It's called: \
                            \"The View from Halfway Down\".\n\
                            \n\
                            SECRETARIAT clears his throat\n\
                            \n\
                            \t\tThe weak breeze whispers nothing\n\
                            \t\tThe water screams sublime\n\
                            \t\tHis feet shift, teeter-totter\n\
                            \t\tDeep breath, stand back, it's time\n\
                            \n\
                            \t\tToes untouch the overpass\n\
                            \t\tSoon he's water bound\n\
                            \t\tEyes locked shut but peek to see\n\
                            \t\tThe view from halfway down\n\
                            \n\
                            Applause, spotlight shines on DOORWAY TO \
                            BLACKNESS\n\
                            \n\
                            \t\t(points at DOORWAY)\n\
                            \t\tI'm not done, hold on. I'm not done.\n\
                            \t\t(affirmatively)\n\
                            \t\tI'm not done.\n\
                            \n\
                            Spotlight turns off, Secretariat faces AUDIENCE \
                            and clears throat\n\
                            \n\
                            \t\tA little wind, a summer sun\n\
                            \t\tA river rich and regal\n\
                            \t\tA flood of fond endorphins\n\
                            \t\tBrings a calm that knows no equal\n\
                            \n\
                            \t\tYou're flying now\n\
                            \t\tYou see things much more clear than from the \
                            ground\n\
                            \t\tIt's all okay, it would be\n\
                            \t\tWere you not now halfway down\n\
                            \n\
                            Spotlight shines on DOORWAY TO BLACKNESS again. \
                            SECRETARIAT nervously looks at it then looks back \
                            at AUDIENCE\n\
                            \n\
                            \t\t(quivering)\n\
                            \t\tThrash to break from gravity\n\
                            \t\tWhat now could slow the drop\n\
                            \t\tAll I'd give for toes to touch\n\
                            \t\tThe safety back at top\n\
                            \n\
                            Spotlight shines on DOORWAY TO BLACKNESS, this \
                            time closer to SECRETARIAT.\n\
                            \n\
                            \t\t(frightened, looking at door)\n\
                            \t\tI change my mind, I- I change my mind, \
                            I don't- I don't wanna-\n\
                            \n\
                            HERB puts his hand on SECRETARIAT's shoulder.\n\
                            \n\
                            \t\tHERB \n\
                            \t\t(comforting)\n\
                            \t\tIt's okay.\n\
                            \n\
                            \t\tSECRETARIAT\n\
                            \t\t(desperately reading poem)\n\
                            \t\tBut this is it, the deed is done\n\
                            \t\tSilence drowns the sound\n\
                            \t\tBefore I leaped I should've seen\n\
                            \t\tThe view from halfway down, No-\n\
                            \n\
                            Spotlight shines on DOORWAY TO BLACKNESS, now \
                            directly behind SECRETARIAT.\n\
                            \n\
                            \t\tI really should have thought about\n\
                            \t\tThe view from halfway down\n\
                            \n\
                            \t\tHERB\n\
                            \t\t(soothingly)\n\
                            \t\tFind your peace, big guy. Find it.\n\
                            \n\
                            \t\tSECRETARIAT\n\
                            \t\t(panicked)\n\
                            \t\tI wish I could have known about\n\
                            \t\tThe view from halfway down-\n\
                            \n\
                            SECRETARIAT falls through DOORWAY, his voice \
                            echoing as he falls into the void."
                        );
                        Err(())
                    }
                    (lhs, rhs) => {
                        match lhs {
                            Ok(Int | Short | Ptr(_)) | Err(()) => (),
                            _ => error(
                                &self.lhs.get_pos(),
                                "Arithmetic operator applied to invalid operand",
                            ),
                        }
                        match rhs {
                            Ok(Int | Short | Ptr(_)) | Err(()) => (),
                            _ => error(
                                &self.rhs.get_pos(),
                                "Arithmetic operator applied to invalid operand",
                            ),
                        }
                        Err(())
                    }
                }
            }
            BinaryOperator::Less
            | BinaryOperator::LessEq
            | BinaryOperator::Greater
            | BinaryOperator::GreaterEq => match (lhs, rhs) {
                (Ok(Int | Short), Ok(Int | Short)) => Ok(Bool),
                (lhs, rhs) => {
                    match lhs {
                        Ok(Int | Short) | Err(()) => (),
                        _ => error(
                            &self.lhs.get_pos(),
                            "Relational operator applied to non-numeric \
                                operand",
                        ),
                    }
                    match rhs {
                        Ok(Int | Short) | Err(()) => (),
                        _ => error(
                            &self.rhs.get_pos(),
                            "Relational operator applied to non-numeric \
                                operand",
                        ),
                    }
                    Err(())
                }
            },
            BinaryOperator::Equals | BinaryOperator::NotEquals => match (lhs, rhs) {
                (lhs @ Ok(Void | Fn { .. } | Ptr(_) | Str), rhs)
                | (lhs, rhs @ Ok(Void | Fn { .. } | Ptr(_) | Str)) => {
                    if let Ok(Void | Fn { .. } | Ptr(_) | Str) = lhs {
                        error(&self.lhs.get_pos(), "Invalid equality operand");
                    }
                    if let Ok(Void | Fn { .. } | Ptr(_) | Str) = rhs {
                        error(&self.rhs.get_pos(), "Invalid equality operand");
                    }
                    Err(())
                }
                (Ok(t1), Ok(t2)) if t1 == t2 => Ok(Bool),
                (Ok(Int) | Ok(Short), Ok(Int) | Ok(Short)) => Ok(Bool),
                (Err(()), _) | (_, Err(())) => Err(()),
                _ => {
                    error(&self.get_pos(), "Invalid equality operation");
                    Err(())
                }
            },
        }
    }
}

impl<'a> TypeCheck for CallExpNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        if let Ok(Fn { args, ret }) = self.id.type_check(Void) {
            if self.args.len() != args.len() {
                error(&self.pos, "Function call with wrong number of args");
                return Err(());
            }
            let args_match = self
                .args
                .iter()
                .zip(args.iter())
                .fold(true, |acc, (act, form)| {
                    let act_t = act.type_check(Void).unwrap();
                    match (form, act_t) {
                        (t1, t2) if *t1 == t2 => acc && true,
                        (Int, Short) => acc && true,
                        _ => {
                            error(
                                &act.get_pos(),
                                "Type of actual does not match type of formal",
                            );
                            false
                        }
                    }
                });
            if !args_match {
                return Err(());
            }
            Ok(*ret.clone())
        } else {
            error(&self.id.get_pos(), "Attempt to call a non-function");
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
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        match self.id.type_check(Void) {
            Ok(Ptr(t)) => Ok(*t),
            _ => {
                error(&self.get_pos(), "Invalid operand for dereference");
                Err(())
            }
        }
    }
}

impl TypeCheck for TrueNode {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Bool)
    }
}

impl TypeCheck for FalseNode {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Bool)
    }
}

impl TypeCheck for IntLitNode {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Int)
    }
}

impl TypeCheck for ShortLitNode {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Short)
    }
}

impl<'a> TypeCheck for StrLitNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Str)
    }
}

impl<'a> TypeCheck for AssignStmtNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        match self.exp.type_check(Void) {
            Ok(_) => Ok(Void),
            Err(()) => Err(()),
        }
    }
}

impl<'a> TypeCheck for CallStmtNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        match self.exp.type_check(Void) {
            Ok(_) => Ok(Void),
            Err(()) => Err(()),
        }
    }
}

impl<'a> TypeCheck for FnDeclNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        // No need to check the formals. But we do need to typecheck everything
        // in the function body. We also need to set the return type here.
        type_check_list(&self.stmts, self.typ.as_symbol_type())
    }
}

impl<'a> TypeCheck for VarDeclNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        // It's confusing, but the result of declaring a variable is
        // Void, or at least we don't really care about it. There will never
        // be errors to report here, declaring a variable of any type is always
        // a valid operation.
        Ok(Void)
    }
}

impl<'a> TypeCheck for FormalDeclNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        // We sanitized formals (i.e. made sure they're not void) in name
        // analysis, so there's no type checking to do here.
        Ok(Void)
    }
}

impl<'a> TypeCheck for IfStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        let exp_result = match self.exp.type_check(return_type.clone()) {
            Ok(Bool) => Ok(Void),
            Err(()) => Err(()),
            _ => {
                error(
                    &self.exp.get_pos(),
                    "Non-bool expression used as an if condition",
                );
                Err(())
            }
        };
        let stmts_result = type_check_list(&self.stmts, return_type);
        exp_result.and(stmts_result)
    }
}

impl<'a> TypeCheck for IfElseStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        let exp_result = match self.exp.type_check(return_type.clone()) {
            Ok(Bool) => Ok(Void),
            Err(()) => Err(()),
            _ => {
                error(
                    &self.exp.get_pos(),
                    "Non-bool expression used as an if condition",
                );
                Err(())
            }
        };
        let stmts_result = type_check_list(&self.true_stmts, return_type.clone())
            .and(type_check_list(&self.else_stmts, return_type));
        exp_result.and(stmts_result)
    }
}

impl<'a> TypeCheck for WhileStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        let exp_result = match self.exp.type_check(return_type.clone()) {
            Ok(Bool) => Ok(Void),
            Err(()) => Err(()),
            _ => {
                error(
                    &self.exp.get_pos(),
                    "Non-bool expression used as a while condition",
                );
                Err(())
            }
        };
        let stmts_result = type_check_list(&self.stmts, return_type);
        exp_result.and(stmts_result)
    }
}

impl<'a> TypeCheck for PostIncStmtNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        match self.lval.type_check(Void) {
            Ok(Int | Short | Ptr(_)) => Ok(Void),
            Ok(_) => {
                error(
                    &self.lval.get_pos(),
                    "Arithmetic operator applied to invalid operand",
                );
                Err(())
            }
            Err(()) => Err(()),
        }
    }
}

impl<'a> TypeCheck for PostDecStmtNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        match self.lval.type_check(Void) {
            Ok(Int | Short | Ptr(_)) => Ok(Void),
            Ok(_) => {
                error(
                    &self.lval.get_pos(),
                    "Arithmetic operator applied to invalid operand",
                );
                Err(())
            }
            Err(()) => Err(()),
        }
    }
}

impl<'a> TypeCheck for ReadStmtNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        match self.lval.type_check(Void) {
            Ok(Bool | Int | Short | Str) => Ok(Void),
            Ok(Fn { .. }) => {
                error(
                    &self.lval.get_pos(),
                    "Attempt to assign user input to function",
                );
                Err(())
            }
            Ok(Ptr(_)) => {
                error(&self.lval.get_pos(), "Attempt to read a raw pointer");
                Err(())
            }
            Ok(Void) => unreachable!(),
            Err(()) => Err(()),
        }
    }
}

impl<'a> TypeCheck for WriteStmtNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        match self.exp.type_check(Void) {
            Ok(Fn { .. }) => {
                error(&self.exp.get_pos(), "Attempt to output a function");
                Err(())
            }
            Ok(Void) => {
                error(&self.exp.get_pos(), "Attempt to write void");
                Err(())
            }
            Ok(Ptr(_)) => {
                error(&self.exp.get_pos(), "Attempt to write a raw pointer");
                Err(())
            }
            Ok(Int | Short | Bool | Str) => Ok(Void),
            Err(()) => Err(()),
        }
    }
}

impl<'a> TypeCheck for ReturnStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        match self.exp {
            Some(ref exp) => match exp.type_check(Void) {
                Ok(t) if t == return_type => Ok(Void),
                Ok(Short) if return_type == Int => Ok(Void),
                Ok(t) if t != Void && return_type == Void => {
                    error(&exp.get_pos(), "Return with a value in void function");
                    Err(())
                }
                Err(()) => Err(()),
                _ => {
                    error(&exp.get_pos(), "Bad return value");
                    Err(())
                }
            },
            None => match return_type {
                Void => Ok(Void),
                _ => {
                    error(&self.get_pos(), "Missing return value");
                    Err(())
                }
            },
        }
    }
}

fn type_check_list<T: TypeCheck>(list: &Vec<T>, return_type: SymbolType) -> Result<SymbolType, ()> {
    list.iter().fold(Ok(Void), |acc, item| {
        acc.and(item.type_check(return_type.clone()))
    })
}
