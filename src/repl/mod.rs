use std::borrow::Cow;
use std::io;

use crate::{lexer, parser};

use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::hint::HistoryHinter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Completer, Helper, Hinter, Validator};

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

    use rustyline::error::ReadlineError;

    use rustyline::{Cmd, Config, Editor, EventHandler, KeyEvent};

    let env = Environment::new();
    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(env)));

    writer.write_all(b"engine: interpreter\n")?;

    let config = Config::builder().history_ignore_space(true).build();
    let h = MyHelper {
        highlighter: MatchingBracketHighlighter::new(),
        hinter: HistoryHinter {},
        colored_prompt: "".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let mut rl = Editor::with_config(config).unwrap();
    rl.set_helper(Some(h));
    rl.bind_sequence(KeyEvent::ctrl('n'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyEvent::ctrl('p'), Cmd::HistorySearchBackward);
    rl.bind_sequence(
        KeyEvent::from('\t'),
        EventHandler::Simple(Cmd::Insert(1, "\t".into())),
    );

    let mut count = 1;
    loop {
        let p = format!("{count}> ");
        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{p}\x1b[0m");
        let readline = rl.readline(&p);
        match readline {
            Ok(input) => {
                rl.add_history_entry(input.as_str()).unwrap();

                let lexer = lexer::Lexer::new(&input);
                let mut parser = parser::Parser::new(lexer);
                let program = parser.parse_program().unwrap();

                let obj = evaluator.eval(&program);

                match obj {
                    Some(object::Object::Error(err)) => {
                        writeln!(writer, "\x1b[41merror: {}\x1b[0m", err)?
                    }
                    Some(obj) => writeln!(writer, "< {}", obj)?,
                    _ => {}
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Goodbye!");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
        count += 1;
    }

    Ok(())
}

#[derive(Helper, Completer, Hinter, Validator)]
struct MyHelper {
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
    colored_prompt: String,
}

impl Highlighter for MyHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Cow::Borrowed(&self.colored_prompt)
        } else {
            Cow::Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned("\x1b[2m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}
