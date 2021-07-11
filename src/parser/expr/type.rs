use std::{
    collections::{
        HashMap,
        VecDeque
    },
    borrow::Borrow
};

use super::{
    IfExpr,
    BasicExpr,
    TypedExpr,
    ExprValue,
    BinOp
};

use crate::parser::token::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Double,

    Function(Vec<Box<Type>>, Box<Type>)
}

impl Type {
    pub fn is_integer(self) -> bool {
        match self {
            Type::Int => true,
            _ => false
        }
    }

    pub fn is_float(self) -> bool {
        match self {
            Type::Double => true,
            _ => false
        }
    }
}

/// Structure to store type information, support pushing and pop scopes in a stack
pub struct SymbolTable {
    scopes: VecDeque<HashMap<String, Type>>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let mut table = SymbolTable {
            scopes: VecDeque::new()
        };

        //Create the root scope
        table.scopes.push_back(HashMap::new());
        table
    }

    pub fn push_scope(&mut self) {
        self.scopes.push_back(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() < 2 {
            panic!("Attempt to pop root scope from symbol table");
        }

        let _ = self.scopes.pop_back();
    }

    pub fn insert(&mut self, name: &String, kind: &Type) {
        self.scopes.back_mut().unwrap().insert(name.clone(), kind.clone());
    }

    pub fn lookup<'a>(&'a mut self, name: &String) -> Option<&'a Type> {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(name) {
                return scope.get(name)
            }
        }

        None
    }
}

pub struct TypeChecker {
    symbol_table: SymbolTable
}

pub type TypeError = (Type, Type);
pub type TypeResult<T> = Result<T, TypeError>;

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            symbol_table: SymbolTable::new()
        }
    }

    pub fn type_check<'a>(&mut self, value: &BasicExpr<'a>) -> TypeResult<Box<TypedExpr<'a>>> {
        let expr = value.borrow();
        let token = value.token().clone();
        let typed = match expr {
            ExprValue::ConstUnit => TypedExpr::const_unit(token),
            ExprValue::ConstInt(value) => TypedExpr::const_int(token, *value),
            ExprValue::ConstBool(value) => TypedExpr::const_bool(token, *value),
            ExprValue::BinOp(op, lhs, rhs) => self.type_check_bin_op(token, *op, lhs, rhs)?,
            ExprValue::Block(exprs) => self.type_check_block(token, exprs)?,
            ExprValue::If(IfExpr { cond: cond, then_expr: then_expr, else_expr: else_expr }) => self.type_check_conditional(token, cond, then_expr, else_expr)?, 
            ExprValue::Var(iden) => self.type_check_var(token, iden)?,
            _ => panic!("")
        };

        Ok(typed)
    }

    fn type_check_bin_op<'a>(&mut self, token: Token<'a>, op: BinOp, lhs: &BasicExpr<'a>, rhs: &BasicExpr<'a>) -> TypeResult<Box<TypedExpr<'a>>> {
        let typed_lhs = self.type_check(lhs)?;
        let typed_rhs = self.type_check(rhs)?;

        let result_type = match op {
            BinOp::Add |
            BinOp::Sub |
            BinOp::Mul |
            BinOp::Div |
            BinOp::Mod => self.unify_arith(typed_lhs.kind(), typed_rhs.kind()),

            BinOp::Eq |
            BinOp::Ne |
            BinOp::Gt |
            BinOp::Lt |
            BinOp::Gte |
            BinOp::Lte => self.unify_compare(typed_lhs.kind(), typed_rhs.kind())
        }?;

        Ok(TypedExpr::bin_op(token, result_type, op, typed_lhs, typed_rhs))
    }

    fn type_check_block<'a>(&mut self, token: Token<'a>, exprs: &Vec<Box<BasicExpr<'a>>>) -> TypeResult<Box<TypedExpr<'a>>> {
        //Blocks create a new scope
        self.symbol_table.push_scope();
        let typed_exprs = exprs.iter().map(|e| self.type_check(e)).collect::<TypeResult<Vec<Box<TypedExpr<'a>>>>>()?;
        self.symbol_table.pop_scope();

        Ok(TypedExpr::block(token, typed_exprs))
    }

    fn type_check_conditional<'a>(&mut self, token: Token<'a>, cond_expr: &BasicExpr<'a>, then_expr: &BasicExpr<'a>, else_expr: &BasicExpr<'a>) -> TypeResult<Box<TypedExpr<'a>>> {
        let typed_cond = self.type_check(cond_expr)?;
        let typed_then = self.type_check(then_expr)?;
        let typed_else = self.type_check(else_expr)?;

        //Conditional expression must be a boolean
        if *typed_cond.kind() != Type::Bool {
            return Err((typed_cond.kind().clone(), Type::Bool));
        }

        //The then and else expressions must have the same type
        if typed_then.kind() != typed_else.kind() {
            return Err((typed_then.kind().clone(), typed_else.kind().clone()));
        }

        Ok(TypedExpr::conditional(token, typed_cond, typed_then, typed_else))
    }

    fn type_check_var<'a>(&mut self, token: Token<'a>, iden: &String) -> TypeResult<Box<TypedExpr<'a>>> {
        let var_type = self.symbol_table.lookup(iden).expect("Undefined variable").clone();
        Ok(TypedExpr::var(token, iden, var_type))
    }

    fn type_check_func_app<'a>(&mut self, token: Token<'a>, iden: &String, args: &Vec<Box<BasicExpr<'a>>>) -> TypeResult<Box<TypedExpr<'a>>> {
        unimplemented!()
    }

    fn unify_arith<'a>(&self, lhs: &Type, rhs: &Type) -> TypeResult<Type> {
        match (lhs, rhs) {
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Double, Type::Double) => Ok(Type::Double),

            (Type::Int, Type::Double) |
            (Type::Double, Type::Int) => Ok(Type::Double),

            _ => Err((lhs.clone(), rhs.clone()))
        }
    }

    fn unify_compare<'a>(&self, lhs: &Type, rhs: &Type) -> TypeResult<Type> {
        match (lhs, rhs) {
            (Type::Int, Type::Int) |
            (Type::Double, Type::Double) |
            (Type::Int, Type::Double) |
            (Type::Double, Type::Int)  => Ok(Type::Bool),

            _ => Err((lhs.clone(), rhs.clone()))
        }
    }
}