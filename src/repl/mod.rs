use std::io::{self, BufRead, Write};

use crate::{lexer, parser};

pub fn repl() -> io::Result<()> {
    loop {
        print!("> ");
        let mut input = String::new();
        let _ = io::stdout().flush();
        let size = io::stdin().read_line(&mut input)?;

        let lexer = lexer::Lexer::new(&input);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();

        if size == 1 {
            break;
        }
        println!("< {}", program);
    }
    println!("exit");
    Ok(())
}
