use inkwell::values::{
    IntValue,
    FunctionValue,
    BasicValueEnum,
};

use std::collections::HashMap;
use inkwell as llvm;

use crate::ast::{
    Expr,
    BinOp
};

pub struct Builder<'ctx, 'md> {
    builder: llvm::builder::Builder<'ctx>,
    context: &'ctx llvm::context::Context,
    args: HashMap<String, BasicValueEnum<'ctx>>,
    block: llvm::basic_block::BasicBlock<'ctx>,
    module: &'md llvm::module::Module<'ctx>
}

impl<'ctx, 'md> Builder<'ctx, 'md> {
    pub fn new(context: &'ctx llvm::context::Context, module: &'md llvm::module::Module<'ctx>, args: &Vec<String>, func: FunctionValue<'ctx>) -> Builder<'ctx, 'md> {
        let mut builder = Builder {
            builder: context.create_builder(),
            context: context,
            args: HashMap::new(),
            block: context.append_basic_block(func, "entry"),
            module: module
        };

        builder.builder.position_at_end(builder.block);

        for (i, name) in args.iter().enumerate() {
            let value = func.get_nth_param(i as u32).unwrap();
            builder.args.insert(name.clone(), value);
        }

        builder
    }

    pub fn build_ret(&self, value: &Expr) {
        let ret = self.build_expr(value);
        self.builder.build_return(Some(&ret));
    }

    fn build_expr(&self, value: &Expr) -> IntValue<'ctx> {
        match value {
            Expr::ConstInt(value) => self.build_const(*value),
            Expr::BinOp(op, lhs, rhs) => self.build_binop(*op, &lhs, &rhs),
            Expr::Var(name) => self.build_var(&name),
            Expr::FunctionApp(name, args) => self.build_function_app(&name, &args),
            Expr::Block(exprs) => self.build_block(&exprs),
            _ => panic!("")
        }
    }

    fn build_binop(&self, op: BinOp, lhs: &Expr, rhs: &Expr) -> IntValue<'ctx> {
        match op {
            BinOp::Add => self.build_add(lhs, rhs),
            BinOp::Sub => self.build_sub(lhs, rhs),
            BinOp::Mul => self.build_mul(lhs, rhs),
            BinOp::Div => self.build_div(lhs, rhs)
        }
    }

    fn build_add(&self, lhs: &Expr, rhs: &Expr) -> IntValue<'ctx> {
        let lvalue = self.build_expr(lhs);
        let rvalue = self.build_expr(rhs);
        self.builder.build_int_add(lvalue, rvalue, "add")
    }

    fn build_sub(&self, lhs: &Expr, rhs: &Expr) -> IntValue<'ctx> {
        let lvalue = self.build_expr(lhs);
        let rvalue = self.build_expr(rhs);
        self.builder.build_int_sub(lvalue, rvalue, "sub")
    }

    fn build_mul(&self, lhs: &Expr, rhs: &Expr) -> IntValue<'ctx> {
        let lvalue = self.build_expr(lhs);
        let rvalue = self.build_expr(rhs);
        self.builder.build_int_mul(lvalue, rvalue, "mul")
    }

    fn build_div(&self, lhs: &Expr, rhs: &Expr) -> IntValue<'ctx> {
        let lvalue = self.build_expr(lhs);
        let rvalue = self.build_expr(rhs);
        self.builder.build_int_signed_div(lvalue, rvalue, "div")
    }

    fn build_const(&self, value: i64) -> IntValue<'ctx> {
        self.context.i64_type().const_int(value as u64, false)
    }

    fn build_var(&self, name: &String) -> IntValue<'ctx> {
        self.args.get(name).unwrap().into_int_value()
    }

    fn build_function_app(&self, name: &str, args: &Vec<Box<Expr>>) -> IntValue<'ctx> {
        let func = self.module.get_function(name).unwrap();
        let arg_values = args.iter().map(|arg| self.build_expr(arg).into()).collect::<Vec<BasicValueEnum<'ctx>>>();
        self.builder.build_call(func, arg_values.as_slice(), "call")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value()
    }

    fn build_block(&self, exprs: &Vec<Box<Expr>>) -> IntValue<'ctx> {
        let results = exprs.iter().map(|expr| self.build_expr(expr)).collect::<Vec<IntValue<'ctx>>>();
        if results.len() == 0 {
            self.context.i64_type().const_int(0, false)
        }
        else {
            results[results.len() - 1]
        }
    }
}