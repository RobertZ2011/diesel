mod ast;
mod parser;
mod codegen;

use std::io::stdin;
use inkwell as llvm;

fn main() {
    let mut s = String::new();
    stdin().read_line(&mut s).expect("Expected a line");

    let context = llvm::context::Context::create();
    let codegen = codegen::module::Module::new(&context, "module");

    if let Ok((_, module)) = parser::module(&s) {
        for top in module.top_level.iter() {
            if let ast::TopLevel::FunctionDef(name, args, expr) = top {
                println!("{:?}", expr);
                codegen.build_function(name, args, expr);
            }
            else {
            }
        }
    }
    else {
        println!("Parse error");
    }

    codegen.dump();
}
