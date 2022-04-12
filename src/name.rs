// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use std::rc::Rc;
use std::vec::Vec;
use std::fs::File;
use crate::ast::{ExpNode::*, *};
use crate::parse;
use std::process;
use std::io::{self, Write};
use crate::error::error;
use enum_dispatch::enum_dispatch;
use crate::name::symbol::{SymbolTable, SymbolType};

use self::symbol::AsSymbol;

pub mod symbol;

pub fn name_analysis(program: &mut ProgramNode, output: &mut File) {
    name_analysis_silent(program);
    parse::unparse(program, output);
}

pub fn name_analysis_silent(program: &mut ProgramNode) {
    // initialize a symbol table
    let mut table = SymbolTable::new();
    if let Err(()) = program.name_analysis(&mut table) {
        writeln!(io::stderr(), "Name Analysis Failed").unwrap();
        process::exit(1)
    }
}

#[enum_dispatch(LValNode, StmtNode, DeclNode)]
pub trait NameAnalysis<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()>;
}

impl<'a> NameAnalysis<'a> for ProgramNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        // enter global scope
        table.enter_scope();
        // run name analysis on all the decls, discarding the result
        let res = block_name_analysis(&mut self.0, table);
        // exit the global scope
        table.exit_scope();
        res
    }
}

// In general, do not set the symbol field of IDNode if we are in a
// declaration, but do if we are in a use.
impl<'a> NameAnalysis<'a> for FnDeclNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        let symbol = Rc::new(self.as_symbol());
        self.symbol = Some(Rc::clone(&symbol));
        let self_res = table.insert_symbol(&symbol);
        if let Err(()) = self_res {
            error(&self.id.pos, "Multiply declared identifier");
        }
        table.enter_scope();
        // remember: formals are in the function body's scope
        // and if we did something like void f(int a, int a), that would
        // be an error.
        let formals_res = block_name_analysis(&mut self.formals, table);
        let stmts_res = block_name_analysis(&mut self.stmts, table);
        let res = self_res.and(formals_res).and(stmts_res);
        table.exit_scope();
        res
    }
}

impl<'a> NameAnalysis<'a> for FormalDeclNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        // First, check if we have type void
        let is_void = match SymbolType::from_formal_decl(self) {
            SymbolType::Void => true,
            _ => false,
        };
        // then check if it's in the symbol table
        let is_declared = table.id_is_in_current_scope(self.id.name);

        if is_declared {
            error(&self.id.pos, "Multiply declared identifier");
        }
        if is_void {
            error(&self.pos, "Invalid type in declaration");
        }

        if !(is_void || is_declared) {
            let symbol = Rc::new(self.as_symbol());
            self.symbol = Some(Rc::clone(&symbol));
            table.insert_symbol(&symbol).unwrap();
            Ok(())
        } else {
            Err(())
        }
    }
}

impl<'a> NameAnalysis<'a> for VarDeclNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        // First, check if we have type void
        let is_void = match SymbolType::from_var_decl(self) {
            SymbolType::Void => true,
            _ => false,
        };
        // then check if it's in the symbol table
        let is_declared = table.id_is_in_current_scope(self.id.name);

        if is_declared {
            error(&self.id.pos, "Multiply declared identifier");
        }
        if is_void {
            error(&self.pos, "Invalid type in declaration");
        }

        if !(is_void || is_declared) {
            let symbol = Rc::new(self.as_symbol());
            self.symbol = Some(Rc::clone(&symbol));
            table.insert_symbol(&symbol).unwrap();
            Ok(())
        } else {
            Err(())
        }
    }
}

impl<'a> NameAnalysis<'a> for AssignStmtNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        // delegate to inner expression
        self.exp.name_analysis(table)
    }
}

impl<'a> NameAnalysis<'a> for CallStmtNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        // delegate to inner expression
        self.exp.name_analysis(table)
    }
}

impl<'a> NameAnalysis<'a> for IfStmtNode<'a> {
    fn name_analysis(&mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        let exp_res = self.exp.name_analysis(table);
        table.enter_scope();
        let stmt_res = block_name_analysis(&mut self.stmts, table);
        table.exit_scope();
        exp_res.and(stmt_res)
    }
}

impl<'a> NameAnalysis<'a> for IfElseStmtNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        let exp_res = self.exp.name_analysis(table);
        table.enter_scope();
        let if_stmt_res = block_name_analysis(&mut self.true_stmts, table);
        table.exit_scope();
        table.enter_scope();
        let else_stmt_res = block_name_analysis(&mut self.else_stmts, table);
        table.exit_scope();
        exp_res.and(if_stmt_res).and(else_stmt_res)
    }
}

impl<'a> NameAnalysis<'a> for WhileStmtNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        let exp_res = self.exp.name_analysis(table);
        table.enter_scope();
        let stmts_res = block_name_analysis(&mut self.stmts, table);
        table.exit_scope();
        exp_res.and(stmts_res)
    }
}

impl<'a> NameAnalysis<'a> for PostIncStmtNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        self.lval.name_analysis(table)
    }
}

impl<'a> NameAnalysis<'a> for PostDecStmtNode<'a> {
    fn name_analysis(&mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        self.lval.name_analysis(table)
    }
}

impl<'a> NameAnalysis<'a> for ReadStmtNode<'a> {
    fn name_analysis(&mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        self.lval.name_analysis(table)
    }
}

impl<'a> NameAnalysis<'a> for WriteStmtNode<'a> {
    fn name_analysis(&mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        self.exp.name_analysis(table)
    }
}

impl<'a> NameAnalysis<'a> for ReturnStmtNode<'a> {
    fn name_analysis(&mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        match &mut self.exp {
            None => Ok(()),
            Some(n) => n.name_analysis(table),
        }
    }
}

impl<'a> NameAnalysis<'a> for ExpNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        match self {
            AssignExp(e) => e.name_analysis(table),
            UnaryExp(e) => e.name_analysis(table),
            BinaryExp(e) => e.name_analysis(table),
            CallExp(e) => e.name_analysis(table),
            LVal(e) => e.name_analysis(table),
            _ => Ok(()),
        }
    }
}


impl<'a> NameAnalysis<'a> for AssignExpNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        let lval_res = self.lval.name_analysis(table);
        let exp_res = self.exp.name_analysis(table);
        lval_res.and(exp_res)
    }
}

impl<'a> NameAnalysis<'a> for UnaryExpNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        self.exp.name_analysis(table)
    }
}

impl<'a> NameAnalysis<'a> for BinaryExpNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        let lhs_res = self.lhs.name_analysis(table);
        let rhs_res = self.rhs.name_analysis(table);
        lhs_res.and(rhs_res)
    }
}

impl<'a> NameAnalysis<'a> for CallExpNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        let id_res = self.id.name_analysis(table);
        let args_res = block_name_analysis(&mut self.args, table);
        id_res.and(args_res)
    }
}

impl<'a> NameAnalysis<'a> for DerefNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        self.id.name_analysis(table)
    }
}

impl<'a> NameAnalysis<'a> for IDNode<'a> {
    fn name_analysis(
        &mut self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()> {
        // lookup myself in the symbol table. If I don't exist that's bad.
        // but if I do exist, wire me up
        match table.get_symbol(self.name) {
            Ok(symbol) => {
                self.symbol = Some(symbol);
                Ok(())
            }
            Err(()) => {
                error(&self.pos, "Undeclared identifier");
                Err(())
            }
        }
    }
}


fn block_name_analysis<'a, T: NameAnalysis<'a>>(
    block: &mut Vec<T>, 
    table: &mut SymbolTable<'a>,
) -> Result<(), ()> {
    // Helper function to accumulate the result of running name analysis
    // on a list
    block
    .iter_mut()
    .map(|decl| decl.name_analysis(table))
    .fold(
        Ok(()), 
        |acc, res| acc.and(res),
    )
}
 
