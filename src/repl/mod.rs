use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::{
    eval::{env::Environment, Evaluator},
    lexer, parser,
};

pub fn repl() -> io::Result<()> {
    let env = Environment::new();
    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(env)));

    loop {
        print!(">>> ");
        let mut input = String::new();
        let _ = io::stdout().flush();
        let size = io::stdin().read_line(&mut input)?;

        let lexer = lexer::Lexer::new(&input);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();

        if parser.errors().len() != 0 {
            print!("syntax error: ");
            parser.errors().iter().for_each(|e| println!("{}", e));
            continue;
        }

        let obj = evaluator.eval(&program);

        if size == 1 {
            break;
        }
        if let Some(obj) = obj {
            println!("< {}", obj);
        }
    }
    println!("exit");
    Ok(())
}
