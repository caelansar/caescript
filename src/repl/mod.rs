use std::io;

use crate::{compiler, lexer, parser, vm};

pub fn repl<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    let mut constants = vec![];
    let mut global = vec![];
    let mut symbol_table = compiler::symbol_table::SymbolTable::new();

    loop {
        writer.write(b">>> ")?;
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
        let bytecode = compiler.compile(&program).unwrap();
        let mut vm = vm::VM::new_with_global(bytecode.clone(), global.clone());
        vm.run();

        global = vm.global.clone();
        symbol_table = compiler.symbol_table;
        constants = bytecode.consts;

        let obj = vm.last_popped();

        if let Some(obj) = obj {
            write!(writer, "< {}\n", obj)?;
        }
    }
    println!("exit");
    Ok(())
}
