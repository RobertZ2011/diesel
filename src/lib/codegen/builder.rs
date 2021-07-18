use inkwell::values::{
    IntValue,
    FloatValue,
    FunctionValue,
    BasicValueEnum,
    BasicValue
};

use std::collections::HashMap;
use inkwell as llvm;

use crate::parser::expr::{
    TypedExpr,
    ExprValue,
    BinOp,
    IfExpr,
    Type
};

use std::borrow::Borrow;

pub struct Builder<'ctx, 'md> {
    builder: llvm::builder::Builder<'ctx>,
    context: &'ctx llvm::context::Context,
    args: HashMap<String, BasicValueEnum<'ctx>>,
    module: &'md llvm::module::Module<'ctx>,
    func: FunctionValue<'ctx>
}

enum CompareOp {
    Eq,
    Ne,
    Gt,
    Lt,
    Gte,
    Lte
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

    pub fn build_ret(&self, value: &TypedExpr) {
        if let Some(ret) = self.build_expr(value) {
            self.builder.build_return(Some(&ret));
        }
    }

    fn build_expr(&self, value: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        let expr = value.borrow();
        match expr {
            ExprValue::ConstInt(value) => self.build_const_int(*value),
            ExprValue::BinOp(op, lhs, rhs) => self.build_binop(*op, value.kind(), &lhs, &rhs),
            ExprValue::Var(name) => self.build_var(&name),
            ExprValue::FunctionApp(name, args) => self.build_function_app(value.kind(), &name, &args),
            ExprValue::Block(exprs) => self.build_block(&exprs),
            ExprValue::UnaryOp(_, expr) => self.build_deref(&expr),
            ExprValue::If(IfExpr { cond, then_expr, else_expr }) => self.build_if(value.kind(), &cond, &then_expr, &else_expr),
            _ => panic!("")
        }
    }

    fn build_binop(&self, op: BinOp, kind: &Type, lhs: &TypedExpr, rhs: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        match op {
            BinOp::Add => self.build_add(kind, lhs, rhs),
            BinOp::Sub => self.build_sub(kind, lhs, rhs),
            BinOp::Mul => self.build_mul(kind, lhs, rhs),
            BinOp::Div => self.build_div(kind, lhs, rhs),
            BinOp::Mod => self.build_mod(kind, lhs, rhs),

            //Compare operations always result in a boolean
            BinOp::Eq => self.build_compare(CompareOp::Eq, lhs, rhs),
            BinOp::Ne => self.build_compare(CompareOp::Ne, lhs, rhs),
            BinOp::Gt => self.build_compare(CompareOp::Gt, lhs, rhs),
            BinOp::Lt => self.build_compare(CompareOp::Lt, lhs, rhs),
            BinOp::Gte => self.build_compare(CompareOp::Gte, lhs, rhs),
            BinOp::Lte => self.build_compare(CompareOp::Lte, lhs, rhs),
        }
    }

    fn build_cast(&self, kind: &Type, expr: &TypedExpr) -> BasicValueEnum<'ctx> {
        let value = self.build_expr(expr).unwrap();
        if kind == expr.kind() {
            //already the proper type, don't need to do anything
            value
        }
        else {
            match (kind, expr.kind()) {
                (Type::Int, Type::Double) => {
                    let int_value = value.into_int_value();
                    self.builder.build_signed_int_to_float(int_value, self.context.f64_type(), "cast")
                        .as_basic_value_enum()
                },
                (Type::Double, Type::Int) => {
                    let float_value = value.into_float_value();
                    self.builder.build_float_to_signed_int(float_value, self.context.i64_type(), "cast")
                        .as_basic_value_enum()

                },
                _ => panic!("Unsupported type cast")
            }
        }
    }

    fn build_add(&self, kind: &Type, lhs: &TypedExpr, rhs: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        let lvalue = self.build_cast(kind, lhs);
        let rvalue = self.build_cast(kind, rhs);
        let res = if kind.is_integer() {
            self.builder.build_int_add(lvalue.into_int_value(), rvalue.into_int_value(), "add").as_basic_value_enum()
        }
        else if kind.is_float() {
            self.builder.build_float_add(lvalue.into_float_value(), rvalue.into_float_value(), "fadd").as_basic_value_enum()
        }
        else {
            panic!("Unsupported type for +")
        };

        Some(res)
    }

    fn build_sub(&self, kind: &Type, lhs: &TypedExpr, rhs: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        let lvalue = self.build_cast(kind, lhs);
        let rvalue = self.build_cast(kind, rhs);
        let res = if kind.is_integer() {
            self.builder.build_int_sub(lvalue.into_int_value(), rvalue.into_int_value(), "sub").as_basic_value_enum()
        }
        else if kind.is_float() {
            self.builder.build_float_sub(lvalue.into_float_value(), rvalue.into_float_value(), "fsub").as_basic_value_enum()
        }
        else {
            panic!("Unsupported type for +")
        };

        Some(res)
    }

    fn build_mul(&self, kind: &Type, lhs: &TypedExpr, rhs: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        let lvalue = self.build_cast(kind, lhs);
        let rvalue = self.build_cast(kind, rhs);
        let res = if kind.is_integer() {
            self.builder.build_int_mul(lvalue.into_int_value(), rvalue.into_int_value(), "mul").as_basic_value_enum()
        }
        else if kind.is_float() {
            self.builder.build_float_mul(lvalue.into_float_value(), rvalue.into_float_value(), "fmul").as_basic_value_enum()
        }
        else {
            panic!("Unsupported type for *")
        };

        Some(res)
    }

    fn build_mod(&self, kind: &Type, lhs: &TypedExpr, rhs: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        let lvalue = self.build_cast(kind, lhs);
        let rvalue = self.build_cast(kind, rhs);
        let res = if kind.is_integer() {
            self.builder.build_int_signed_rem(lvalue.into_int_value(), rvalue.into_int_value(), "mod").as_basic_value_enum()
        }
        else if kind.is_float() {
            self.builder.build_float_rem(lvalue.into_float_value(), rvalue.into_float_value(), "fmod").as_basic_value_enum()
        }
        else {
            panic!("Unsupported type for +")
        };

        Some(res)
    }

    fn build_div(&self, kind: &Type, lhs: &TypedExpr, rhs: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        let lvalue = self.build_cast(kind, lhs);
        let rvalue = self.build_cast(kind, rhs);
        let res = if kind.is_integer() {
            self.builder.build_int_signed_div(lvalue.into_int_value(), rvalue.into_int_value(), "div").as_basic_value_enum()
        }
        else if kind.is_float() {
            self.builder.build_float_div(lvalue.into_float_value(), rvalue.into_float_value(), "fdiv").as_basic_value_enum()
        }
        else {
            panic!("Unsupported type for +")
        };

        Some(res)
    }

    fn build_const_int(&self, value: i64) -> Option<BasicValueEnum<'ctx>> {
        Some(self.context.i64_type().const_int(value as u64, false).as_basic_value_enum())
    }

    fn build_const_bool(&self, value: bool) -> Option<BasicValueEnum<'ctx>> {
        Some(self.context.bool_type().const_int(if value { 1 } else { 0 }, false).as_basic_value_enum())
    }

    fn build_const_double(&self, value: f64) -> Option<BasicValueEnum<'ctx>> {
        Some(self.context.f64_type().const_float(value).as_basic_value_enum())
    }

    fn build_var(&self, name: &String) -> Option<BasicValueEnum<'ctx>> {
        Some(*self.args.get(name).unwrap())
    }

    fn build_function_app(&self, kind: &Type, name: &str, args: &Vec<Box<TypedExpr>>) -> Option<BasicValueEnum<'ctx>> {
        let func = self.module.get_function(name).unwrap();
        let arg_values = args.iter().map(|arg| self.build_expr(arg).unwrap()).collect::<Vec<BasicValueEnum<'ctx>>>();
        let res = self.builder.build_call(func, arg_values.as_slice(), "call")
                    .try_as_basic_value()
                    .left()
                    .unwrap();

        if *kind == Type::Unit {
            None
        }
        else {
            Some(res)
        }
    }

    fn build_block(&self, exprs: &Vec<Box<TypedExpr>>) -> Option<BasicValueEnum<'ctx>> {
        let results = exprs.iter().filter_map(|expr| self.build_expr(expr)).collect::<Vec<BasicValueEnum<'ctx>>>();
        if results.len() == 0 {
            None
        }
        else {
            Some(results[results.len() - 1])
        }
    }

    fn build_deref(&self, expr: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        let ptr_type = self.context.i64_type().ptr_type(llvm::AddressSpace::Generic);
        let res = self.build_expr(expr).unwrap().into_int_value();
        let ptr = self.builder.build_int_to_ptr(res, ptr_type, "cast");
        Some(self.builder.build_load(ptr, "load"))
    }

    fn build_if(&self, kind: &Type, cond_expr: &TypedExpr, then_expr: &TypedExpr, else_expr: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        let cond_value = self.build_expr(cond_expr).unwrap().into_int_value();
        let then_block = self.context.append_basic_block(self.func, "then");
        let else_block = self.context.append_basic_block(self.func, "else");
        let cont_block = self.context.append_basic_block(self.func, "cont");

        self.builder.build_conditional_branch(cond_value, then_block, else_block);

        self.builder.position_at_end(then_block);
        let then_value = self.build_expr(then_expr).unwrap();
        self.builder.build_unconditional_branch(cont_block);

        self.builder.position_at_end(else_block);
        let else_value = self.build_expr(else_expr).unwrap();
        self.builder.build_unconditional_branch(cont_block);

        self.builder.position_at_end(cont_block);
        let phi = self.builder.build_phi(self.context.i64_type(), "phi");
        phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
        Some(phi.as_basic_value())
    }

    fn build_compare(&self, op: CompareOp, lhs: &TypedExpr, rhs: &TypedExpr) -> Option<BasicValueEnum<'ctx>> {
        let compare_type = match (lhs.kind(), rhs.kind()) {
            (Type::Int, Type::Int) => Type::Int,
            (Type::Double, Type::Double) => Type::Double,

            (Type::Int, Type::Double) => Type::Double,
            (Type::Double, Type::Int) => Type::Double,

            _ => panic!("Unsupported type for comparison")
        };

        let lhs_expr = self.build_cast(&compare_type, lhs);
        let rhs_expr = self.build_cast(&compare_type, rhs);

        let res = if compare_type.is_integer() {
            let cmp_op = match op {
                CompareOp::Eq => llvm::IntPredicate::EQ,
                CompareOp::Ne => llvm::IntPredicate::NE,
                CompareOp::Gt => llvm::IntPredicate::SGT,
                CompareOp::Lt => llvm::IntPredicate::SLT,
                CompareOp::Gte => llvm::IntPredicate::SGE,
                CompareOp::Lte => llvm::IntPredicate::SLE,
            };
            self.builder.build_int_compare(cmp_op, lhs_expr.into_int_value(), rhs_expr.into_int_value(), "cmp").as_basic_value_enum()
        }
        else if compare_type.is_float() {
            let cmp_op = match op {
                CompareOp::Eq => llvm::FloatPredicate::OEQ,
                CompareOp::Ne => llvm::FloatPredicate::ONE,
                CompareOp::Gt => llvm::FloatPredicate::OGT,
                CompareOp::Lt => llvm::FloatPredicate::OLT,
                CompareOp::Gte => llvm::FloatPredicate::OGE,
                CompareOp::Lte => llvm::FloatPredicate::OLE
            };
            self.builder.build_float_compare(cmp_op, lhs_expr.into_float_value(), rhs_expr.into_float_value(), "fcmc").as_basic_value_enum()
        }
        else {
            unreachable!()
        };

        Some(res)
    }
}