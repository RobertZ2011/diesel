use clap::{App, Arg};

fn main() {
    let args = App::new("Diesel Compiler")
        .about("Compiles Diesel Files")
    .get_matches();
}