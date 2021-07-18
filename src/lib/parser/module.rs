use super::expr::{
    Expr,
    BasicExpr,
    TypedExpr,
    Type,
    SymbolTable,
    TypeChecker,
    TypeResult
};

#[derive(Debug)]
pub enum Definition<E: Expr> {
    Function(String, Vec<(String, Type)>, Box<E>)
}

/// Struct that represents a single compilation unit
pub struct Module<E: Expr> {
    symbol_table: SymbolTable,
    pub definitions: Vec<Definition<E>>
}

impl<E: Expr> Module<E> {
    pub fn new() -> Module<E> {
        Module::<E> {
            symbol_table: SymbolTable::new(),
            definitions: Vec::new()
        }
    }
}

pub type TypedModule<'a> = Module<TypedExpr<'a>>;

pub struct BasicModule<'a>(Module<BasicExpr<'a>>);

impl<'a> BasicModule<'a> {
    pub fn new() -> BasicModule<'a> {
        let mut module = Module::new();
        let atoi = "atoi".to_string();
        module.symbol_table.insert(&atoi, &Type::Function(vec![Type::Int], Box::new(Type::Int)));

        BasicModule(module)
    }

    pub fn define_func(&mut self, name: &String, ret_type: &Type, args: &Vec<(String, Type)>, expr: Box<BasicExpr<'a>>) {
        //Create the appropriate function type for the symbol table
        let mut arg_types = args.iter().map(|(_, kind)| kind.clone()).collect::<Vec<Type>>();
        if arg_types.len() == 0 {
            //A function that takes no arguments should have the type Unit -> a
            arg_types.push(Type::Unit);
        }

        let func_type = Type::Function(arg_types, Box::new(ret_type.clone()));
        self.0.symbol_table.insert(name, &func_type);

        let def = Definition::Function(name.clone(), args.clone(), expr);
        self.0.definitions.push(def);
    }

    pub fn type_check(self) -> TypeResult<TypedModule<'a>> {
        let mut typed_module = TypedModule {
            definitions: Vec::new(),
            symbol_table: self.0.symbol_table.clone()
        };

        let mut type_checker = TypeChecker::new(self.0.symbol_table);
        for def in self.0.definitions {
            match def {
                Definition::Function(name, args, expr) => {
                    //Start by creating a new scope and inserting our arguments
                    type_checker.symbol_table().push_scope();
                    for (iden, kind) in &args {
                        type_checker.symbol_table().insert(iden, kind);
                    }

                    let typed_expr = type_checker.type_check(&expr)?;
                    type_checker.symbol_table().pop_scope();

                    typed_module.definitions.push(Definition::Function(name, args, typed_expr));
                }
            }
            
        }

        Ok(typed_module)
    }
}

