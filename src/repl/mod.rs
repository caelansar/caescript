use std::io;

use crate::{lexer, parser};

#[cfg(feature = "vm")]
pub fn repl<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    use crate::{compiler, vm};

    let mut constants = vec![];
    let mut global = vec![];
    let mut symbol_table = compiler::symbol_table::SymbolTable::new();

    writer.write_all(b"engine: vm\n")?;

    loop {
        writer.write_all(b">>> ")?;
        writer.flush()?;

        let mut line = String::new();
        let size = reader.read_line(&mut line)?;
        if size == 1 {
            break;
        }

        let lexer = lexer::Lexer::new(&line);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        let mut compiler =
            compiler::Compiler::new_with_state(symbol_table.clone(), constants.clone());
        let bytecode = match compiler.compile(&program) {
            Ok(bytecode) => bytecode,
            Err(err) => {
                writeln!(writer, "\x1b[41mcompile error: {}\x1b[0m", err)?;
                continue;
            }
        };
        let mut vm = vm::VM::new_with_global(bytecode.clone(), global.clone());
        vm.run();

        let obj = vm.last_popped();

        global = vm.global.clone();
        constants = bytecode.consts.to_vec();
        symbol_table = compiler.symbol_table;

        if let Some(obj) = obj {
            writeln!(writer, "< {}", obj)?;
        }
    }
    writer.write_all(b"exit\n")?;
    Ok(())
}

#[cfg(not(feature = "vm"))]
pub fn repl<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    use std::{cell::RefCell, rc::Rc};

    use crate::eval::{env::Environment, object, Evaluator};

    let env = Environment::new();
    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(env)));

    writer.write_all(b"engine: interpreter\n")?;

    loop {
        writer.write_all(b">>> ")?;
        writer.flush()?;

        let mut input = String::new();
        let size = reader.read_line(&mut input)?;

        let lexer = lexer::Lexer::new(&input);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        let obj = evaluator.eval(&program);

        if size == 1 {
            break;
        }
        match obj {
            Some(object::Object::Error(err)) => writeln!(writer, "\x1b[41merror: {}\x1b[0m", err)?,
            Some(obj) => writeln!(writer, "< {}", obj)?,
            _ => {}
        }
    }
    writer.write_all(b"exit\n")?;
    Ok(())
}
