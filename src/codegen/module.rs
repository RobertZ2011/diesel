use inkwell as llvm;
use crate::parser::ast::Expr;
use super::builder::Builder;
use std::path::Path;

pub struct Module<'ctx, 'a> {
    context: &'ctx llvm::context::Context,
    module: &'a llvm::module::Module<'ctx>,
    fpm: &'a llvm::passes::PassManager<llvm::values::FunctionValue<'ctx>>
}

impl<'ctx, 'a> Module<'ctx, 'a> {
    pub fn new(context: &'ctx llvm::context::Context, module: &'a llvm::module::Module<'ctx>, fpm: &'a llvm::passes::PassManager<llvm::values::FunctionValue<'ctx>>) -> Module<'ctx, 'a> {
        let fn_type = context.i64_type().fn_type(&[context.i64_type().into()], false);

        module.add_function("atoi", fn_type, None);
        module.add_function("puts", fn_type, None);
        module.add_function("putchar", fn_type, None);

        Module {
            context: context,
            module: module,
            fpm
        }
    }

    pub fn build_function(&self, name: &str, args: &Vec<String>, expr: &Box<Expr>) {
        let arg_types = vec![self.context.i64_type().into(); args.len()];
        let fn_type = self.context.i64_type().fn_type(arg_types.as_slice(), false);
        let func = self.module.add_function(name, fn_type, None);
        let builder = Builder::new(self.context, &self.module, args, func);

        builder.build_ret(expr);

        if func.verify(false) {
            self.fpm.run_on(&func);
        }
        else {
            panic!("Couldn't verify function");
        }
    }

    pub fn dump(&self) {
        let result = self.module.print_to_string().to_string();
        println!("{}", result);
    }

    pub fn write_to_file(&self, target: &inkwell::targets::TargetMachine, path: &Path) {
        target.write_to_file(&self.module, inkwell::targets::FileType::Object, path).expect("Failed to write object file");
    }
}