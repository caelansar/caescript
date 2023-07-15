use caescript::eval::{env::Environment, object::Object, Evaluator};
#[cfg(feature = "vm")]
use caescript::{compiler::Compiler, vm::VM};
use std::{cell::RefCell, env, fs, io, process::exit, rc::Rc};

use caescript::{lexer, parser, repl};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        println!("Welcome to Caescript!");
        return repl::repl(io::stdin().lock(), io::stdout().lock()).unwrap();
    }

    if args.len() != 3 {
        println!("invalid args num");
        exit(1)
    }

    let path = &args[2];

    let content = fs::read_to_string(path).expect("can not read file");

    match args[1].as_str() {
        #[cfg(feature = "vm")]
        "vm" => {
            vm_run(content.as_str());
        }
        "eval" => {
            eval_run(content.as_str());
        }
        arg => {
            println!("unknown arg '{}'", arg);
            exit(1)
        }
    }
}

#[cfg(feature = "vm")]
fn vm_run(input: &str) -> Object {
    let lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);

    let program = parser.parse_program().unwrap();

    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&program).unwrap();

    let mut vm = VM::new(bytecode);
    vm.run();

    vm.last_popped().unwrap()
}

fn eval_run(input: &str) -> Object {
    let lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);

    let program = parser.parse_program().unwrap();

    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Environment::new())));

    evaluator.eval(&program).unwrap()
}
