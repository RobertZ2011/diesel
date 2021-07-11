use super::expr::Expr;

#[derive(Debug)]
pub enum Definition<E: Expr> {
    Function(String, Vec<String>, Box<E>)
}

#[derive(Debug)]
pub struct Module<E: Expr> {
    pub definitions: Vec<Definition<E>>
}

impl<E: Expr> Module<E> {
    pub fn new() -> Module<E> {
        Module::<E> {
            definitions: Vec::new()
        }
    }
}