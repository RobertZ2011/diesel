use std::path::Path;
use std::io::stdin;
use inkwell as llvm;
use inkwell::targets::{Target, TargetTriple, TargetMachine, RelocMode, CodeModel, InitializationConfig};
use inkwell::OptimizationLevel;
use std::process::Command;

use diesel_lib::parser::{
    lexer::tokens,
    Parser
};

mod compiler;

use clap::{App, Arg};

fn main() {
    let args = App::new("Diesel Compiler")
        .about("Compiles Diesel Files")
        .arg(Arg::with_name("output")
            .short("o")
            .long("output")
            .value_name("FILE")
            .help("Sets the output file")
            .required(true)
            .takes_value(true)
        )
        .arg(Arg::with_name("emit-ir")
            .long("emit-ir")
            .help("Emit LLVM IR")
        )
        .arg(Arg::with_name("emit-asm")
            .long("emit-asm")
            .help("Emit assembly")
        )
        .arg(Arg::with_name("input")
            .required(true)
            .value_name("INPUT")
            .help("Input source files")
            .required(true)
            .multiple(true)
            .index(1)
        )
    .get_matches();

    let context = llvm::context::Context::create();

    Target::initialize_aarch64(&InitializationConfig::default());
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let target_machine = target.create_target_machine(
        &triple,
        "",
        "",
        OptimizationLevel::None,
        RelocMode::Default,
        CodeModel::Default
    ).unwrap();
 
    let mut obj_files = Vec::new();

    for src in args.values_of("input").unwrap() {
        let input = Path::new(src);
        let output = input.with_extension("o");

        compiler::compile_module(&context, &target_machine, input, &output);
        obj_files.push(output);
    }

    let output = args.value_of("output").unwrap();
    let mut args = vec!["-o", output];
    let mut obj_strs = obj_files.iter().map(|s| s.to_str().unwrap()).collect::<Vec<&str>>();
    args.append(&mut obj_strs);
    Command::new("clang")
        .args(args)
        .spawn()
        .expect("Failed to link");
}
