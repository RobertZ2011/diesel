mod basic_expr;
mod expr_value;
mod op;
mod r#type;
mod typed_expr;

pub use basic_expr::BasicExpr;
pub use expr_value::{
    ExprValue,
    IfExpr
};
pub use typed_expr::TypedExpr;
pub use r#type::{ 
    SymbolTable,
    Type 
};
pub use op::{
    BinOp,
    UnaryOp
};

pub trait Expr where Self: std::fmt::Debug {}