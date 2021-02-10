use std::env;
use std::fs;
use mlisp::interpreter::run_interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    assert!(args.len() > 1, "Must supply a file path.");

    let content = fs::read_to_string(&args[1])
    	.expect("There was an error reading the file.");

    run_interpreter(&content);
}
