use super::expr::BasicExpr;

#[derive(Clone, Debug)]
pub enum Definition<'a> {
    Function(String, Vec<String>, Box<BasicExpr<'a>>)
}

#[derive(Clone, Debug)]
pub struct Module<'a> {
    pub definitions: Vec<Definition<'a>>
}

impl<'a> Module<'a> {
    pub fn new() -> Module<'a> {
        Module {
            definitions: Vec::new()
        }
    }
}