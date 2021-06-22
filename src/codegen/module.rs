use inkwell as llvm;
use crate::ast::Expr;
use super::builder::Builder;

pub struct Module<'ctx> {
    context: &'ctx llvm::context::Context,
    module: llvm::module::Module<'ctx>
}

impl<'ctx> Module<'ctx> {
    pub fn new(context: &'ctx llvm::context::Context, name: &str) -> Module<'ctx> {
        Module {
            context: context,
            module: context.create_module(name)
        }
    }

    pub fn build_function(&self, name: &str, args: &Vec<String>, expr: &Box<Expr>) {
        let arg_types = vec![self.context.i64_type().into(); args.len()];
        let fn_type = self.context.i64_type().fn_type(arg_types.as_slice(), false);
        let func = self.module.add_function(name, fn_type, None);
        let builder = Builder::new(self.context, &self.module, args, func);

        builder.build_ret(expr);
    }

    pub fn dump(&self) {
        let result = self.module.print_to_string().to_string();
        println!("{}", result);
    }
}