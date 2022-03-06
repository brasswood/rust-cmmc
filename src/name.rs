// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use std::collections::HashMap;
use std::vec::Vec;
use std::boxed::Box;
use std::fs::File;
use crate::ast::{self, *};
use std::io::{self, Write};
use enum_dispatch::enum_dispatch;

#[derive(Debug, Clone)]
pub struct Symbol<'a> {
    name: &'a str,
    typ: SymbolType,
}

#[enum_dispatch(DeclNode)]
pub trait AsSymbol<'a> {
    fn as_symbol(&self) -> Symbol<'a>;
}

impl<'a> AsSymbol<'a> for FnDeclNode<'a> {
    fn as_symbol(&self) -> Symbol<'a> {
        Symbol { name: self.id.name, typ: SymbolType::from_fn_decl(self) }
    }
}

impl<'a> AsSymbol<'a> for VarDeclNode<'a> {
    fn as_symbol(&self) -> Symbol<'a> {
        Symbol { name: self.id.name, typ: SymbolType::from_var_decl(self) }
    }
}

impl<'a> AsSymbol<'a> for FormalDeclNode<'a> {
    fn as_symbol(&self) -> Symbol<'a> {
        Symbol { 
            name: self.id.name, 
            typ: SymbolType::from_formal_decl(self)
        }
    }
}

#[derive(Debug, Clone)]
enum SymbolType {
    // Every type in the AST, plus a special function variant which
    // encapsulates arguments
    Int,
    Short,
    Bool,
    Str,
    Void,
    Ptr(Box<Type>),
    Fn { args: Vec<SymbolType>, ret: Box<SymbolType> },
}

impl SymbolType {
    fn from_decl(decl: &DeclNode) -> Self {
        match decl {
            DeclNode::FnDecl(d) => SymbolType::from_fn_decl(d),
            DeclNode::VarDecl(d) => SymbolType::from_var_decl(d),
            DeclNode::FormalDecl(d) => SymbolType::from_formal_decl(d),
        }
    }

    fn from_fn_decl(decl: &FnDeclNode) -> Self {
        let args: Vec<_> = decl
            .formals
            .iter()
            .map(|formal| SymbolType::from_formal_decl(formal))
            .collect();
        let ret = Box::new(decl.typ.as_symbol_type());
        SymbolType::Fn { args, ret }
    }

    fn from_var_decl(decl: &VarDeclNode) -> Self {
        decl.typ.as_symbol_type()
    }

    fn from_formal_decl(decl: &FormalDeclNode) -> Self {
        decl.typ.as_symbol_type()
    }
}

impl ast::Type {
    fn as_symbol_type(&self) -> SymbolType {
        match self {
            Type::Int => SymbolType::Int,
            Type::Short => SymbolType::Short,
            Type::Bool => SymbolType::Bool,
            Type::Str => SymbolType::Str,
            Type::Void => SymbolType::Void,
            Type::Ptr(t) => SymbolType::Ptr(t.clone()),
        }
    }
}

pub struct SymbolTable<'a> {
    table: Vec<HashMap<&'a str, Symbol<'a>>>,
}

impl<'a> SymbolTable<'a> {
    fn new() -> Self {
        SymbolTable { table: Vec::new() }
    }

    fn insert_decl<T: AsSymbol<'a>>(&mut self, decl: &T) -> Result<(),()> {
        let current_scope = self.table.last_mut().unwrap();
        let entry = decl.as_symbol();
        if current_scope.contains_key(entry.name) {
            return Err(());
        }
        current_scope.insert(entry.name, entry);
        Ok(())
    }

    fn enter_scope(&mut self) {
        self.table.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.table.pop();
    }
}

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
 
     
