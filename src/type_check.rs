// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt) EXCEPT FOR "The View From Halfway Down" from Bojack
// Horseman.

use crate::name::symbol::{SymbolType, SymbolType::*};
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
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!()
    }
}

impl<'a> TypeCheck for UnaryExpNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!() 
    }
}

impl<'a> TypeCheck for BinaryExpNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        let (lhs, rhs) = (
            self.lhs.type_check(return_type.clone()),
            self.rhs.type_check(return_type),
        );
        match self.op {
            BinaryOperator::And | BinaryOperator::Or => {
                match (lhs, rhs) {
                    (Ok(Bool), Ok(Bool)) => Ok(Bool),
                    (lhs, rhs) => {
                        match lhs {
                            Ok(Bool) => (),
                            _ => error(
                                &self.lhs.get_pos(),
                                "Logical operator applied to non-bool\
                                operand",
                            ),
                        }
                        match rhs {
                            Ok(Bool) => (),
                            _ => error(
                                &self.rhs.get_pos(),
                                "Logical operator applied to non-bool\
                                operand",
                            ),
                        }
                        Err(())
                    }
                }
            }
            BinaryOperator::Plus
            | BinaryOperator::Minus
            | BinaryOperator::Times
            | BinaryOperator::Divide => {
                match (lhs, rhs) {
                    (Ok(Short), Ok(Short)) => Ok(Short),
                    (Ok(Int) | Ok(Short), Ok(Int) | Ok(Short)) => Ok(Int),
                    (Ok(Ptr(_)), Ok(Int) | Ok(Short))
                    | (Ok(Int) | Ok(Short), Ok(Ptr(_))) => {
                        Ok(Ptr(Box::new(Void)))
                    }
                    (Ok(Ptr(_)), Ok(Ptr(_))) => {
                        error(&self.get_pos(), "Invalid arithmetic operation");
                        println!("nasal demons!");
                        Err(())
                    }
                    (lhs, rhs) => {
                        match lhs {
                            Ok(Int) | Ok(Short) | Ok(Ptr(_)) | Err(()) => (),
                            _ => error(
                                &self.lhs.get_pos(),
                                "Arithmetic operator applied to invalid operand",
                            )
                        }
                        match rhs {
                            Ok(Int) | Ok(Short) | Ok(Ptr(_)) | Err(()) => (),
                            _ => error(
                                &self.rhs.get_pos(),
                                "Arithmetic operator applied to invalid operand",
                            )
                        }
                        Err(())
                    }
                }
            }
            BinaryOperator::Less
            | BinaryOperator::LessEq
            | BinaryOperator::Greater
            | BinaryOperator::GreaterEq => {
                match (lhs, rhs) {
                    (Ok(Ptr(_)), Ok(Ptr(_))) 
                    | (Ok(Int) | Ok(Short), Ok(Int) | Ok(Short)) => Ok(Bool),
                    // NOTE: The following pattern is reached when there are two
                    // valid operands, but they're not compatible with each other
                    (Ok(Ptr(_)) | Ok(Int) | Ok(Short),
                    Ok(Ptr(_)) | Ok(Int) | Ok(Short)) => {
                        // The correct error isn't defined in the spec, so I
                        // think this is a reasonable thing to do.
                        error(&self.get_pos(), "Invalid relational operation");
                        // Also... NASAL DEMONS!!!
                        println!(
                            "\t\tSECRETARIAT
                            \t\tA poem. Original, obviously. It's called:\
                            'The View from Halfway Down'.
                            
                            SECRETARIAT clears his throat
                            
                            \t\tThe weak breeze whispers nothing
                            \t\tThe water screams sublime
                            \t\tHis feet shift, teeter-totter
                            \t\tDeep breath, stand back, it's time
                            
                            \t\tToes untouch the overpass
                            \t\tSoon he's water bound
                            \t\tEyes locked shut but peek to see
                            \t\tThe view from halfway down
                            
                            Applause, spotlight shines on DOORWAY TO BLACKNESS
                            
                            \t\t(points at DOORWAY) I'm not done, hold on. \
                            I'm not done.
                            \t\t(affirmatively) I'm not done.
                            
                            Spotlight turns off, Secretariat faces AUDIENCE \
                            and clears throat
                            
                            \t\tA little wind, a summer sun
                            \t\tA river rich and regal
                            \t\tA flood of fond endorphins
                            \t\tBrings a calm that knows no equal
                            
                            \t\tYou're flying now
                            \t\tYou see things much more clear than from the \
                            ground
                            \t\tIt's all okay, it would be
                            \t\tWere you not now halfway down
                            
                            Spotlight shines on DOORWAY TO BLACKNESS again. \
                            SECRETARIAT nervously looks at it then looks back \
                            at AUDIENCE

                            \t\t(quivering)
                            \t\tThrash to break from gravity
                            \t\tWhat now could slow the drop
                            \t\tAll I'd give for toes to touch
                            \t\tThe safety back at top
                            
                            Spotlight shines on DOORWAY TO BLACKNESS, this \
                            time closer to SECRETARIAT.
                            
                            \t\t(frightened, looking at door)
                            \t\tI change my mind, I- I change my mind, \
                            \t\tI don't- I don't wanna-
                            
                            HERB puts his hand on SECRETARIAT's shoulder.

                            \t\tHERB (comforting)
                            \t\tIt's okay.
                            
                            \t\tSECRETARIAT (desperately reading poem)
                            \t\tBut this is it, the deed is done
                            \t\tSilence drowns the sound
                            \t\tBefore I leaped I should've seen
                            \t\tThe view from halfway down, No-
                            
                            Spotlight shines on DOORWAY TO BLACKNESS, now \
                            directly behind SECRETARIAT.
                            
                            \t\tI really should have thought about
                            \t\tThe view from halfway down
                            
                            \t\tHERB (soothingly)
                            \t\tFind your peace, big guy. Find it.
                            
                            \t\tSECRETARIAT (panicked)
                            \t\tI wish I could have known about
                            \t\tThe view from halfway down-
                            
                            SECRETARIAT falls through DOORWAY, his voice \
                            echoing as he falls into the void."
                        );
                        Err(())
                    }
                    (lhs, rhs) => {
                        match lhs {
                            Ok(Ptr(_)) | Ok(Int) | Ok(Short) | Err(()) => (),
                            _ => error(
                                &self.lhs.get_pos(),
                                "Relational operator applied to non-numeric\
                                operand",
                            )
                        }
                        match rhs {
                            Ok(Ptr(_)) | Ok(Int) | Ok(Short) | Err(()) => (),
                            _ => error(
                                &self.rhs.get_pos(),
                                "Relational operator applied to non-numeric\
                                operand",
                            )
                        }
                        Err(())
                    }
                }
            }
            BinaryOperator::Equals
            | BinaryOperator::NotEquals => {
                match (lhs, rhs) {
                    (lhs @ (Ok(Void) | Ok(Fn{ .. })), rhs)
                    | (lhs, rhs @ (Ok(Void) | Ok(Fn{..}))) => {
                        if let Ok(Void) | Ok(Fn{..}) = lhs {
                            error(
                                &self.lhs.get_pos(),
                                "Invalid equality operand",
                            );
                        }
                        if let Ok(Void) | Ok(Fn{..}) = rhs {
                            error(
                                &self.rhs.get_pos(),
                                "Invalid equality operand",
                            );
                        }
                        Err(())
                    }
                    (Ok(t1), Ok(t2)) if t1 == t2 => Ok(Bool),
                    (Ok(Ptr(_)), Ok(Ptr(_))) => Ok(Bool),
                    _ => {
                        error(&self.get_pos(), "Invalid equality operation");
                        Err(())
                    }
                }
            }
        }
    }
}

impl<'a> TypeCheck for CallExpNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
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
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        Ok(Ptr(Box::new(self.id.type_check(Void).unwrap())))
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
        todo!()
    }
}

impl<'a> TypeCheck for CallStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        self.exp.type_check(return_type)
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
        self.exp
            .type_check(return_type.clone())
            .and(type_check_list(&self.stmts, return_type))
    }
}

impl<'a> TypeCheck for IfElseStmtNode<'a> {
    fn type_check(&self, return_type: SymbolType) -> Result<SymbolType, ()> {
        self.exp
            .type_check(return_type.clone())
            .and(type_check_list(&self.true_stmts, return_type.clone()))
            .and(type_check_list(&self.else_stmts, return_type))
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
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
        todo!()
    }
}

impl<'a> TypeCheck for PostDecStmtNode<'a> {
    fn type_check(&self, _return_type: SymbolType) -> Result<SymbolType, ()> {
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
        match self.exp {
            Some(ref exp) => {
                match exp.type_check(Void) {
                    Ok(t) if t == return_type => Ok(Void),
                    Ok(t) if t != Void && return_type == Void => {
                        error(
                            &exp.get_pos(),
                            "Return with a value in void function",
                        );
                        Err(())
                    }
                    Err(()) => Err(()),
                    _ => {
                        error(&exp.get_pos(), "Bad return value");
                        Err(())
                    }
                }
            }
            None => {
                match return_type {
                    Void => Ok(Void),
                    _ => {
                        error(&self.get_pos(), "Missing return value");
                        Err(())
                    }

                }
            }
        }
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

