use std::borrow::Cow;
use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use crate::{lexer, parser};

mod completer;

use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter as TSHighlighter};

// Static ANSI color codes to avoid repeated allocations
static ANSI_KEYWORD: &str = "\x1b[94m"; // bright blue
static ANSI_FUNCTION: &str = "\x1b[96m"; // bright cyan
static ANSI_STRING: &str = "\x1b[92m"; // green
static ANSI_COMMENT: &str = "\x1b[90m"; // gray
static ANSI_NUMBER: &str = "\x1b[93m"; // yellow
static ANSI_CONSTANT: &str = "\x1b[95m"; // magenta
static ANSI_OPERATOR: &str = "\x1b[97m"; // bright white
static ANSI_DEFAULT: &str = "\x1b[37m"; // white
static ANSI_RESET: &str = "\x1b[0m";

use rustyline::completion::{Completer, Pair};
use rustyline::config::OutputStreamType;
use rustyline::error::ReadlineError;
use rustyline::highlight::{Highlighter, PromptInfo};
use rustyline::hint::{Hinter, HistoryHinter};
use rustyline::validate::{self, MatchingBracketValidator, Validator};
use rustyline::{Cmd, CompletionType, Config, Context, EditMode, Editor, Helper, KeyPress};

use self::completer::CaescriptCompleter;

#[cfg(feature = "vm")]
pub fn repl<W: io::Write>(mut writer: W) -> io::Result<()> {
    use crate::{compiler, vm};

    let mut constants = vec![];
    let mut global = vec![];
    let mut symbol_table = compiler::symbol_table::SymbolTable::new();
    let symbol_table_ref = Rc::new(RefCell::new(symbol_table.clone()));

    writer.write_all(b"engine: vm\n")?;

    // Use completion type from environment variable or default based on platform
    let completion_type = match std::env::var("CAESCRIPT_COMPLETION_TYPE").as_deref() {
        Ok("circular") => CompletionType::Circular,
        Ok("list") => CompletionType::List,
        #[cfg(all(unix, feature = "fuzzy-completion"))]
        Ok("fuzzy") => CompletionType::Fuzzy,
        _ => {
            #[cfg(all(unix, feature = "fuzzy-completion"))]
            {
                CompletionType::Fuzzy
            }
            #[cfg(not(all(unix, feature = "fuzzy-completion")))]
            {
                CompletionType::Circular
            }
        }
    };

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(completion_type)
        .edit_mode(EditMode::Emacs)
        .output_stream(OutputStreamType::Stdout)
        .build();
    let h = MyHelper {
        completer: CaescriptCompleter::new_vm(symbol_table_ref.clone()),
        hinter: HistoryHinter {},
        colored_prompt: "  0> ".to_owned(),
        continuation_prompt: "\x1b[1;32m.> \x1b[0m".to_owned(),
        validator: MatchingBracketValidator::new(),
        highlighter: create_highlight_config(),
        last_highlighted_line: RefCell::new(None),
    };
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));
    rl.bind_sequence(KeyPress::Meta('N'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyPress::Meta('P'), Cmd::HistorySearchBackward);

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

                global.clone_from(&vm.global);
                constants = bytecode.consts.to_vec();
                symbol_table = compiler.symbol_table;
                *symbol_table_ref.borrow_mut() = symbol_table.clone();

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
pub fn repl<W: io::Write>(mut writer: W) -> io::Result<()> {
    use crate::eval::{Evaluator, env::Environment, object};

    let env = Environment::new();
    let env_ref = Rc::new(RefCell::new(env));
    let mut evaluator = Evaluator::new(env_ref.clone());

    writer.write_all(b"engine: interpreter\n")?;

    // Use completion type from environment variable or default based on platform
    let completion_type = match std::env::var("CAESCRIPT_COMPLETION_TYPE").as_deref() {
        Ok("circular") => CompletionType::Circular,
        Ok("list") => CompletionType::List,
        #[cfg(all(unix, feature = "fuzzy-completion"))]
        Ok("fuzzy") => CompletionType::Fuzzy,
        _ => {
            #[cfg(all(unix, feature = "fuzzy-completion"))]
            {
                CompletionType::Fuzzy
            }
            #[cfg(not(all(unix, feature = "fuzzy-completion")))]
            {
                CompletionType::Circular
            }
        }
    };

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(completion_type)
        .edit_mode(EditMode::Emacs)
        .output_stream(OutputStreamType::Stdout)
        .build();
    let h = MyHelper {
        completer: CaescriptCompleter::new_interpreter(env_ref.clone()),
        hinter: HistoryHinter {},
        colored_prompt: "  0> ".to_owned(),
        continuation_prompt: "\x1b[1;32m.> \x1b[0m".to_owned(),
        validator: MatchingBracketValidator::new(),
        highlighter: create_highlight_config(),
        last_highlighted_line: RefCell::new(None),
    };
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));
    rl.bind_sequence(KeyPress::Meta('N'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyPress::Meta('P'), Cmd::HistorySearchBackward);

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

fn create_highlight_config() -> Option<(RefCell<TSHighlighter>, HighlightConfiguration)> {
    let language = tree_sitter_caescript::LANGUAGE;

    let mut config = HighlightConfiguration::new(
        language.into(),
        "caescript",
        include_str!("../../tree-sitter-caescript/queries/highlights.scm"),
        "", // No injection query
        "", // No locals query
    )
    .ok()?;

    // Configure recognized highlight names
    let highlight_names = vec![
        "keyword",
        "function",
        "function.builtin",
        "function.call",
        "string",
        "comment",
        "number",
        "constant.builtin",
        "parameter",
        "variable",
        "variable.definition",
        "operator",
        "punctuation",
    ];

    config.configure(&highlight_names);

    Some((RefCell::new(TSHighlighter::new()), config))
}

struct MyHelper<C> {
    completer: C,
    validator: MatchingBracketValidator,
    hinter: HistoryHinter,
    colored_prompt: String,
    continuation_prompt: String,
    highlighter: Option<(RefCell<TSHighlighter>, HighlightConfiguration)>,
    // Cache for syntax highlighting
    last_highlighted_line: RefCell<Option<(String, String)>>, // (input, highlighted_output)
}

impl<C: Completer<Candidate = Pair>> Helper for MyHelper<C> {}

impl<C: Completer<Candidate = Pair>> Completer for MyHelper<C> {
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

impl<C> Hinter for MyHelper<C> {
    fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl<C> Highlighter for MyHelper<C> {
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
        // Early exit for very short inputs
        if line.len() < 2 {
            return Cow::Borrowed(line);
        }

        // Check cache first
        {
            let cache = self.last_highlighted_line.borrow();
            if let Some((cached_input, cached_output)) = cache.as_ref() {
                if cached_input == line {
                    return Cow::Owned(cached_output.clone());
                }
            }
        }

        if let Some((highlighter, config)) = &self.highlighter {
            let mut highlighter = highlighter.borrow_mut();

            // Try to highlight the input
            if let Ok(events) = highlighter.highlight(config, line.as_bytes(), None, |_| None) {
                // Pre-allocate with estimated capacity
                let mut highlighted = String::with_capacity(line.len() * 2);
                let mut current_style: Option<usize> = None;

                for event in events {
                    match event.unwrap() {
                        HighlightEvent::Source { start, end } => {
                            let text = &line[start..end];

                            // Apply current style if any
                            if let Some(style_idx) = current_style {
                                let ansi_code = match style_idx {
                                    0 => ANSI_KEYWORD,   // keyword
                                    1 => ANSI_FUNCTION,  // function
                                    2 => ANSI_FUNCTION,  // function.builtin
                                    3 => ANSI_FUNCTION,  // function.call
                                    4 => ANSI_STRING,    // string
                                    5 => ANSI_COMMENT,   // comment
                                    6 => ANSI_NUMBER,    // number
                                    7 => ANSI_CONSTANT,  // constant.builtin
                                    8 => ANSI_DEFAULT,   // parameter
                                    9 => ANSI_DEFAULT,   // variable
                                    10 => ANSI_DEFAULT,  // variable.definition
                                    11 => ANSI_OPERATOR, // operator
                                    12 => ANSI_DEFAULT,  // punctuation
                                    _ => "",
                                };

                                if !ansi_code.is_empty() {
                                    highlighted.push_str(ansi_code);
                                    highlighted.push_str(text);
                                    highlighted.push_str(ANSI_RESET);
                                } else {
                                    highlighted.push_str(text);
                                }
                            } else {
                                highlighted.push_str(text);
                            }
                        }
                        HighlightEvent::HighlightStart(s) => {
                            current_style = Some(s.0);
                        }
                        HighlightEvent::HighlightEnd => {
                            current_style = None;
                        }
                    }
                }

                // Update cache
                *self.last_highlighted_line.borrow_mut() =
                    Some((line.to_string(), highlighted.clone()));

                return Cow::Owned(highlighted);
            }
        }

        // Fallback to unhighlighted text
        Cow::Borrowed(line)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        if pos == line.len() {
            // 行尾
            true
        } else if pos > 0 {
            let prev_ch = line.chars().nth(pos - 1).unwrap_or(' ');
            let current_ch = line.chars().nth(pos).unwrap_or(' ');
            matches!(
                prev_ch,
                ' ' | '\t'
                    | '('
                    | ')'
                    | '{'
                    | '}'
                    | '['
                    | ']'
                    | ';'
                    | ','
                    | ':'
                    | '"'
                    | '\''
                    | '='
                    | '+'
                    | '-'
                    | '*'
                    | '/'
                    | '!'
                    | '<'
                    | '>'
                    | '&'
                    | '|'
            ) || matches!(
                current_ch,
                ' ' | '\t'
                    | '('
                    | ')'
                    | '{'
                    | '}'
                    | '['
                    | ']'
                    | ';'
                    | ','
                    | ':'
                    | '"'
                    | '\''
                    | '='
                    | '+'
                    | '-'
                    | '*'
                    | '/'
                    | '!'
                    | '<'
                    | '>'
                    | '&'
                    | '|'
            )
        } else {
            false
        }
    }
}

impl<C> Validator for MyHelper<C> {
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
