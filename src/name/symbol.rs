// Copyright (c) 2022 Andrew Riachi. Licensed under the 3-Clause BSD License
// (see LICENSE.txt).

use crate::ast::{self, *};
use std::vec::Vec;
use std::rc::Rc;
use std::collections::HashMap;
use enum_dispatch::enum_dispatch;

#[derive(Debug, Clone)]
pub struct Symbol<'a> {
    pub name: &'a str,
    pub typ: SymbolType,
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
pub enum SymbolType {
    // Every type in the AST, plus a special function variant which
    // encapsulates arguments
    Int,
    Short,
    Bool,
    Str,
    Void,
    Ptr(Box<SymbolType>),
    Fn { args: Vec<SymbolType>, ret: Box<SymbolType> },
}

impl SymbolType {
    pub fn from_fn_decl(decl: &FnDeclNode) -> Self {
        let args: Vec<_> = decl
            .formals
            .iter()
            .map(|formal| SymbolType::from_formal_decl(formal))
            .collect();
        let ret = Box::new(decl.typ.as_symbol_type());
        SymbolType::Fn { args, ret }
    }

    pub fn from_var_decl(decl: &VarDeclNode) -> Self {
        decl.typ.as_symbol_type()
    }

    pub fn from_formal_decl(decl: &FormalDeclNode) -> Self {
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
            Type::Ptr(t) => SymbolType::Ptr(Box::new(t.as_symbol_type())),
        }
    }
}

pub struct SymbolTable<'a> {
    table: Vec<HashMap<&'a str, Rc<Symbol<'a>>>>,
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
        current_scope.insert(entry.name, Rc::new(entry));
        Ok(())
    }

    pub fn id_is_in_current_scope(&self, id: &str) -> bool {
        self.table.last().unwrap().contains_key(id)
    }

    pub fn get_symbol(&self, id: &str) -> Result<Rc<Symbol<'a>>, ()> {
        // try the most current scope through the least current scope
        for t in self.table.iter().rev() {
            if let Some(symbol) = t.get(id) {
                return Ok(Rc::clone(symbol));
            }
        }
        Err(())
    }

    pub fn enter_scope(&mut self) {
        self.table.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.table.pop();
    }
}

