use inkwell as llvm;
use crate::parser::{
    ast::{ 
        Module as AModule,
        Definition
    },
    lexer::tokens,
    token::Span,
    Parser
};
use crate::codegen::module::Module as CModule;
use std::path::Path;
use std::fs::read_to_string;

pub fn compile_module(context: &llvm::context::Context, target: &inkwell::targets::TargetMachine, input: &Path, output: &Path) {
    let src = read_to_string(input).expect("Failed to read source file");
    let s = Span::new(&src);

    let (_, token_stream) = tokens(s).expect("Parse error");
    let parser = Parser::new(token_stream);
    let ast_module = parser.parse().expect("Parse error");
    let codegen_module = CModule::new(context, &src);

    for definition in ast_module.definitions {
        let Definition::Function(name, args, expr) = definition;
        codegen_module.build_function(&name, &args, &expr);
    }

    codegen_module.dump();
    codegen_module.write_to_file(target, output);
}