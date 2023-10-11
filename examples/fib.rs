use std::cell::RefCell;
use std::rc::Rc;
use std::thread;
use std::time::Instant;

#[cfg(feature = "vm")]
use caescript::{compiler::Compiler, vm::VM};

use caescript::{
    eval::{env::Environment, object::Object, Evaluator},
    lexer,
    parser::Parser,
};

fn main() {
    let input = r#"
        fn fib(x) {
            if (x==1 || x==2) {
                1
            } else {
                fib(x-1) + fib(x-2)
            }
        }
        fib(35)
        "#;

    let jh1 = thread::spawn(|| elapsed(input, "eval", eval_run));
    #[cfg(feature = "vm")]
    let jh2 = thread::spawn(|| elapsed(input, "vm", vm_run));
    jh1.join().unwrap();
    #[cfg(feature = "vm")]
    jh2.join().unwrap();
}

fn elapsed<F: Fn(&str) -> Object>(input: &str, engine: &str, f: F) {
    let start = Instant::now();
    let data = f(input);
    let duration = start.elapsed();
    println!("engine: {}, res: {}, elapsed: {:?}", engine, data, duration)
}

fn eval_run(input: &str) -> Object {
    let lexer = lexer::Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program().unwrap();

    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Environment::new())));

    evaluator.eval(&program).unwrap()
}

#[cfg(feature = "vm")]
fn vm_run(input: &str) -> Object {
    let lexer = lexer::Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program().unwrap();

    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&program).unwrap();

    let mut vm = VM::new(bytecode);
    vm.run();

    vm.last_popped().map(|x| ((*x).clone())).unwrap()
}
