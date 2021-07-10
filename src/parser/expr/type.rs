use std::collections::{
    HashMap,
    VecDeque
};

#[derive(Clone, Debug)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Uint,
    Float,
    Double,

    Function(Box<Type>, Box<Type>) //Represents the type a -> b
}

pub struct Scope {
    types: HashMap<String, Type>
}

/// Structure to store type information, support pushing and pop scopes in a stack
pub struct SymbolTable {
    root: Scope,
    scopes: VecDeque<Scope>
}

impl SymbolTable {
}