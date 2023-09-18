use std::borrow::Cow;
use std::io;

use crate::{lexer, parser};

use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::config::OutputStreamType;
use rustyline::error::ReadlineError;
use rustyline::highlight::{Highlighter, PromptInfo};
use rustyline::hint::{Hinter, HistoryHinter};
use rustyline::validate::{self, MatchingBracketValidator, Validator};
use rustyline::{Cmd, CompletionType, Config, Context, EditMode, Editor, Helper, KeyPress};

#[cfg(feature = "vm")]
pub fn repl<R: io::BufRead, W: io::Write>(_reader: R, mut writer: W) -> io::Result<()> {
    use crate::{compiler, vm};

    let mut constants = vec![];
    let mut global = vec![];
    let mut symbol_table = compiler::symbol_table::SymbolTable::new();

    writer.write_all(b"engine: vm\n")?;

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .output_stream(OutputStreamType::Stdout)
        .build();
    let h = MyHelper {
        completer: FilenameCompleter::new(),
        hinter: HistoryHinter {},
        colored_prompt: "  0> ".to_owned(),
        continuation_prompt: "\x1b[1;32m.> \x1b[0m".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));
    rl.bind_sequence(KeyPress::Meta('N'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyPress::Meta('P'), Cmd::HistorySearchBackward);
    rl.bind_sequence(KeyPress::Tab, Cmd::Insert(1, "    ".into()));

    let mut count = 1;
    loop {
        let p = format!("{:>1}> ", count);
        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{}\x1b[0m", p);
        let readline = rl.readline(&p);
        match readline {
            Ok(input) => {
                rl.add_history_entry(input.as_str());
                let lexer = lexer::Lexer::new(&input);
                let mut parser = parser::Parser::new(lexer);
                let program = match parser.parse_program() {
                    Ok(program) => program,
                    Err(err) => {
                        writeln!(writer, "\x1b[41msyntax error: {}\x1b[0m", err)?;
                        continue;
                    }
                };

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

#[cfg(not(feature = "vm"))]
pub fn repl<R: io::BufRead, W: io::Write>(_reader: R, mut writer: W) -> io::Result<()> {
    use std::{cell::RefCell, rc::Rc};

    use crate::eval::{env::Environment, object, Evaluator};

    let env = Environment::new();
    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(env)));

    writer.write_all(b"engine: interpreter\n")?;

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .output_stream(OutputStreamType::Stdout)
        .build();
    let h = MyHelper {
        completer: FilenameCompleter::new(),
        hinter: HistoryHinter {},
        colored_prompt: "  0> ".to_owned(),
        continuation_prompt: "\x1b[1;32m.> \x1b[0m".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));
    rl.bind_sequence(KeyPress::Meta('N'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyPress::Meta('P'), Cmd::HistorySearchBackward);
    rl.bind_sequence(KeyPress::Tab, Cmd::Insert(1, "    ".into()));

    let mut count = 1;
    loop {
        let p = format!("{:>1}> ", count);
        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{}\x1b[0m", p);
        let readline = rl.readline(&p);
        match readline {
            Ok(input) => {
                rl.add_history_entry(input.as_str());

                let lexer = lexer::Lexer::new(&input);
                let mut parser = parser::Parser::new(lexer);
                let program = match parser.parse_program() {
                    Ok(program) => program,
                    Err(err) => {
                        writeln!(writer, "\x1b[41msyntax error: {}\x1b[0m", err)?;
                        continue;
                    }
                };

                let obj = evaluator.eval(&program);

                match obj {
                    Some(object::Object::Error(err)) => {
                        writeln!(writer, "\x1b[41merror: {}\x1b[0m", err)?
                    }
                    Some(obj) => writeln!(writer, "{}", obj)?,
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

struct MyHelper {
    completer: FilenameCompleter,
    validator: MatchingBracketValidator,
    hinter: HistoryHinter,
    colored_prompt: String,
    continuation_prompt: String,
}

impl Helper for MyHelper {}

impl Completer for MyHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Pair>), ReadlineError> {
        self.completer.complete(line, pos, ctx)
    }
}

impl Hinter for MyHelper {
    fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl Highlighter for MyHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        info: PromptInfo<'_>,
    ) -> Cow<'b, str> {
        if info.is_default() {
            if info.line_no() > 0 {
                Cow::Borrowed(&self.continuation_prompt)
            } else {
                Cow::Borrowed(&self.colored_prompt)
            }
        } else {
            Cow::Borrowed(prompt)
        }
    }

    fn has_continuation_prompt(&self) -> bool {
        true
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned("\x1b[2m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        Cow::Borrowed(line)
    }

    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        false
    }
}

impl Validator for MyHelper {
    fn validate(
        &self,
        ctx: &mut validate::ValidationContext,
    ) -> rustyline::Result<validate::ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}
