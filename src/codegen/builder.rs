use inkwell::values::{
    IntValue,
    FunctionValue,
    BasicValueEnum,
};

use std::collections::HashMap;
use inkwell as llvm;

use crate::parser::ast::{
    Expr,
    BinOp,
    IfExpr
};

pub struct Builder<'ctx, 'md> {
    builder: llvm::builder::Builder<'ctx>,
    context: &'ctx llvm::context::Context,
    args: HashMap<String, BasicValueEnum<'ctx>>,
    module: &'md llvm::module::Module<'ctx>,
    func: FunctionValue<'ctx>
}

impl<'ctx, 'md> Builder<'ctx, 'md> {
    pub fn new(context: &'ctx llvm::context::Context, module: &'md llvm::module::Module<'ctx>, args: &Vec<String>, func: FunctionValue<'ctx>) -> Builder<'ctx, 'md> {
        let mut builder = Builder {
            builder: context.create_builder(),
            context: context,
            args: HashMap::new(),
            module: module,
            func: func
        };

        let block = context.append_basic_block(func, "entry");
        builder.builder.position_at_end(block);

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
            Expr::UnaryOp(_, expr) => self.build_deref(&expr),
            Expr::If(IfExpr { cond, then_expr, else_expr }) => self.build_if(&cond, &then_expr, &else_expr),
            _ => panic!("")
        }
    }

    fn build_binop(&self, op: BinOp, lhs: &Expr, rhs: &Expr) -> IntValue<'ctx> {
        match op {
            BinOp::Add => self.build_add(lhs, rhs),
            BinOp::Sub => self.build_sub(lhs, rhs),
            BinOp::Mul => self.build_mul(lhs, rhs),
            BinOp::Div => self.build_div(lhs, rhs),
            BinOp::Mod => self.build_mod(lhs, rhs),
            BinOp::Eq => self.build_compare(llvm::IntPredicate::EQ, lhs, rhs),
            BinOp::Ne => self.build_compare(llvm::IntPredicate::NE, lhs, rhs),
            BinOp::Gt => self.build_compare(llvm::IntPredicate::SGT, lhs, rhs),
            BinOp::Lt => self.build_compare(llvm::IntPredicate::SLT, lhs, rhs),
            BinOp::Gte => self.build_compare(llvm::IntPredicate::SGE, lhs, rhs),
            BinOp::Lte => self.build_compare(llvm::IntPredicate::SLE, lhs, rhs),
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

    fn build_mod(&self, lhs: &Expr, rhs: &Expr) -> IntValue<'ctx> {
        let lvalue = self.build_expr(lhs);
        let rvalue = self.build_expr(rhs);
        self.builder.build_int_signed_rem(lvalue, rvalue, "mod")
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

    fn build_deref(&self, expr: &Expr) -> IntValue<'ctx> {
        let ptr_type = self.context.i64_type().ptr_type(llvm::AddressSpace::Generic);
        let res = self.build_expr(expr);
        let ptr = self.builder.build_int_to_ptr(res, ptr_type, "cast");
        self.builder.build_load(ptr, "load").into_int_value()
    }

    fn build_if(&self, cond_expr: &Expr, then_expr: &Expr, else_expr: &Expr) -> IntValue<'ctx> {
        let cond_value = self.build_expr(cond_expr);
        let compare = self.builder.build_int_compare(llvm::IntPredicate::NE, cond_value, self.build_const(0), "cmp");

        let then_block = self.context.append_basic_block(self.func, "then");
        let else_block = self.context.append_basic_block(self.func, "else");
        let cont_block = self.context.append_basic_block(self.func, "cont");

        self.builder.build_conditional_branch(compare, then_block, else_block);

        self.builder.position_at_end(then_block);
        let then_value = self.build_expr(then_expr);
        self.builder.build_unconditional_branch(cont_block);

        self.builder.position_at_end(else_block);
        let else_value = self.build_expr(else_expr);
        self.builder.build_unconditional_branch(cont_block);

        self.builder.position_at_end(cont_block);
        let phi = self.builder.build_phi(self.context.i64_type(), "phi");
        phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
        phi.as_basic_value().into_int_value()
    }

    fn build_compare(&self, op: llvm::IntPredicate, lhs: &Expr, rhs: &Expr) -> IntValue<'ctx> {
        let lhs_expr = self.build_expr(lhs);
        let rhs_expr = self.build_expr(rhs);
        let res = self.builder.build_int_compare(op, lhs_expr, rhs_expr, "cmp");
        self.builder.build_int_z_extend(res, self.context.i64_type(), "cast")
    }
}