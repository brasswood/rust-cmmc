// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use std::vec::Vec;
use std::boxed::Box;
use std::fs::File;
use crate::ast::{self, *};
use std::io::{self, Write};
use enum_dispatch::enum_dispatch;
use crate::name::symbol::SymbolTable;

pub mod symbol;

pub fn name_analysis(program: &ProgramNode, output: File) {
    // initialize a symbol table
    let mut table = SymbolTable::new();
    program.name_analysis(&mut table);
}

#[enum_dispatch(LValNode, StmtNode, DeclNode)]
pub trait NameAnalysis<'a> {
    fn name_analysis(
        &self,
        table: &mut SymbolTable<'a>,
    ) -> Result<(), ()>;
}

impl<'a> NameAnalysis<'a> for ProgramNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        // enter global scope
        table.enter_scope();
        // run name analysis on all the decls, discarding the result
        let res = block_name_analysis(&self.0, table);
        // exit the global scope
        table.exit_scope();
        res
    }
}

// In general, do not set the symbol field of IDNode if we are in a
// declaration, but do if we are in a use.
impl<'a> NameAnalysis<'a> for FnDeclNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        let self_res = table.insert_decl(self);
        table.enter_scope();
        // remember: formals are in the function body's scope
        // and if we did something like void f(int a, int a), that would
        // be an error.
        let formals_res = block_name_analysis(&self.formals, table);
        let stmts_res = block_name_analysis(&self.stmts, table);
        let res = self_res.and(formals_res).and(stmts_res);
        table.exit_scope();
        res
    }
}

impl<'a> NameAnalysis<'a> for AssignExpNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for FormalDeclNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for VarDeclNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for ReturnStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for WriteStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for ReadStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for PostDecStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for PostIncStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for WhileStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for IfElseStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for IfStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for CallStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for AssignStmtNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for DerefNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for IDNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for CallExpNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for BinaryExpNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for UnaryExpNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}

impl<'a> NameAnalysis<'a> for ExpNode<'a> {
    fn name_analysis(&self, table: &mut SymbolTable<'a>) -> Result<(), ()> {
        todo!()
    }
}


fn block_name_analysis<'a, T: NameAnalysis<'a>>(
    block: &Vec<T>, 
    table: &mut SymbolTable<'a>,
) -> Result<(), ()> {
    // Helper function to accumulate the result of running name analysis
    // on a list
    block
    .into_iter()
    .map(|decl| decl.name_analysis(table))
    .fold(
        Ok(()), 
        |acc, res| acc.and(res),
    )
}
 
