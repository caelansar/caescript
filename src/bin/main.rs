use std::io;

use caescript::repl;

fn main() {
    println!("Hello, world!");
    repl::repl(io::stdin().lock(), io::stdout().lock()).unwrap();
}
