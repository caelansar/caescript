#![feature(result_option_inspect)]

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod token;

fn main() {
    println!("Hello, world!");
    repl::repl().unwrap();
}
