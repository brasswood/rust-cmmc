// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use crate::ast::{self, *};
use std::vec::Vec;
use std::collections::HashMap;
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
    pub fn new() -> Self {
        SymbolTable { table: Vec::new() }
    }

    pub fn insert_decl<T: AsSymbol<'a>>(
        &mut self,
        decl: &T
    ) -> Result<(),()> {
        let current_scope = self.table.last_mut().unwrap();
        let entry = decl.as_symbol();
        if current_scope.contains_key(entry.name) {
            return Err(());
        }
        current_scope.insert(entry.name, entry);
        Ok(())
    }

    pub fn enter_scope(&mut self) {
        self.table.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.table.pop();
    }
}

