use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use rustyline::Context;
use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;

#[cfg(feature = "vm")]
use crate::compiler::symbol_table::SymbolTable;

#[cfg(not(feature = "vm"))]
use crate::eval::env::Environment;

use tree_sitter::{Parser, Point};

pub enum CompletionState {
    #[cfg(feature = "vm")]
    Vm {
        symbol_table: Rc<RefCell<SymbolTable>>,
    },
    #[cfg(not(feature = "vm"))]
    Interpreter {
        environment: Rc<RefCell<Environment>>,
    },
}

pub struct CaescriptCompleter {
    state: CompletionState,
    parser: Parser,
    keywords: HashSet<&'static str>,
    builtin_functions: HashSet<&'static str>,
}

impl CaescriptCompleter {
    #[cfg(feature = "vm")]
    pub fn new_vm(symbol_table: Rc<RefCell<SymbolTable>>) -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_caescript::LANGUAGE.into())
            .expect("Error loading Caescript parser");

        Self {
            state: CompletionState::Vm { symbol_table },
            parser,
            keywords: Self::get_keywords(),
            builtin_functions: Self::get_builtin_functions(),
        }
    }

    #[cfg(not(feature = "vm"))]
    pub fn new_interpreter(environment: Rc<RefCell<Environment>>) -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_caescript::LANGUAGE.into())
            .expect("Error loading Caescript parser");

        Self {
            state: CompletionState::Interpreter { environment },
            parser,
            keywords: Self::get_keywords(),
            builtin_functions: Self::get_builtin_functions(),
        }
    }

    fn get_keywords() -> HashSet<&'static str> {
        let keywords = vec![
            "let", "return", "for", "break", "continue", "fn", "if", "else", "true", "false",
            "null", "and", "or",
        ];
        keywords.into_iter().collect()
    }

    fn get_builtin_functions() -> HashSet<&'static str> {
        let functions = vec!["puts", "len", "first", "last", "rest", "push"];
        functions.into_iter().collect()
    }

    fn get_completion_context(&mut self, line: &str, pos: usize) -> CompletionContext {
        // Parse the current line to understand context
        if let Some(tree) = self.parser.parse(line, None) {
            let root = tree.root_node();
            let point = self.byte_offset_to_point(line, pos);

            // Find the node at cursor position
            let node = root.descendant_for_point_range(point, point);

            if let Some(node) = node {
                match node.kind() {
                    "identifier" => {
                        // Check if we're after a let keyword
                        if let Some(parent) = node.parent() {
                            if parent.kind() == "let_statement" {
                                // Check if this is the variable name position
                                if let Some(let_node) = parent.child(0) {
                                    if let_node.kind() == "let"
                                        && node.start_position().column
                                            > let_node.end_position().column
                                    {
                                        return CompletionContext::NewVariable;
                                    }
                                }
                            }
                        }
                        CompletionContext::Identifier
                    }
                    "ERROR" => {
                        // Handle incomplete input
                        if pos > 0 {
                            let prefix = &line[..pos];
                            if prefix.ends_with(' ') || prefix.is_empty() {
                                CompletionContext::StatementStart
                            } else {
                                CompletionContext::Identifier
                            }
                        } else {
                            CompletionContext::StatementStart
                        }
                    }
                    _ => {
                        // Check if we're at the beginning of a statement
                        if line[..pos].trim().is_empty() {
                            CompletionContext::StatementStart
                        } else {
                            CompletionContext::Expression
                        }
                    }
                }
            } else {
                CompletionContext::StatementStart
            }
        } else {
            // Fallback for parse errors
            if line[..pos].trim().is_empty() {
                CompletionContext::StatementStart
            } else {
                CompletionContext::Identifier
            }
        }
    }

    fn byte_offset_to_point(&self, text: &str, byte_offset: usize) -> Point {
        let mut row = 0;
        let mut column = 0;

        for (i, ch) in text.char_indices() {
            if i >= byte_offset {
                break;
            }
            if ch == '\n' {
                row += 1;
                column = 0;
            } else {
                column += ch.len_utf8();
            }
        }

        Point { row, column }
    }

    fn get_prefix<'a>(&self, line: &'a str, pos: usize) -> &'a str {
        let start = line[..pos]
            .rfind(|c: char| !c.is_alphanumeric() && c != '_')
            .map(|i| i + 1)
            .unwrap_or(0);
        &line[start..pos]
    }

    fn collect_candidates(&self, prefix: &str, context: CompletionContext) -> Vec<TypedCompletion> {
        let mut candidates = Vec::new();

        match context {
            CompletionContext::StatementStart => {
                // Suggest keywords that can start statements
                candidates.extend(
                    self.keywords
                        .iter()
                        .filter(|&&k| {
                            k == "let" || k == "return" || k == "for" || k == "fn" || k == "if"
                        })
                        .filter(|&&k| k.starts_with(prefix))
                        .map(|&k| TypedCompletion {
                            text: k.to_string(),
                            completion_type: CompletionType::Keyword,
                        }),
                );

                // Also suggest identifiers (functions and variables)
                self.add_identifiers(&mut candidates, prefix);
            }
            CompletionContext::Identifier | CompletionContext::Expression => {
                // Suggest all identifiers, keywords, and built-in functions
                self.add_identifiers(&mut candidates, prefix);

                candidates.extend(
                    self.keywords
                        .iter()
                        .filter(|&&k| k.starts_with(prefix))
                        .map(|&k| TypedCompletion {
                            text: k.to_string(),
                            completion_type: CompletionType::Keyword,
                        }),
                );

                candidates.extend(
                    self.builtin_functions
                        .iter()
                        .filter(|&&f| f.starts_with(prefix))
                        .map(|&f| TypedCompletion {
                            text: f.to_string(),
                            completion_type: CompletionType::BuiltinFunction,
                        }),
                );
            }
            CompletionContext::NewVariable => {
                // Don't suggest existing variables for new variable names
                // Could potentially suggest naming conventions
            }
        }

        // Sort by type first, then alphabetically
        candidates.sort_by(|a, b| match (&a.completion_type, &b.completion_type) {
            (CompletionType::Keyword, CompletionType::Keyword) => a.text.cmp(&b.text),
            (CompletionType::Keyword, _) => std::cmp::Ordering::Less,
            (_, CompletionType::Keyword) => std::cmp::Ordering::Greater,
            (CompletionType::BuiltinFunction, CompletionType::BuiltinFunction) => {
                a.text.cmp(&b.text)
            }
            (CompletionType::BuiltinFunction, _) => std::cmp::Ordering::Less,
            (_, CompletionType::BuiltinFunction) => std::cmp::Ordering::Greater,
            _ => a.text.cmp(&b.text),
        });

        // Remove duplicates
        candidates.dedup_by(|a, b| a.text == b.text);
        candidates
    }

    fn add_identifiers(&self, candidates: &mut Vec<TypedCompletion>, prefix: &str) {
        match &self.state {
            #[cfg(feature = "vm")]
            CompletionState::Vm { symbol_table } => {
                // Extract symbols from symbol table
                let table = symbol_table.borrow();
                self.collect_symbols_from_table(&table, candidates, prefix);
            }
            #[cfg(not(feature = "vm"))]
            CompletionState::Interpreter { environment } => {
                // Extract symbols from environment
                let env = environment.borrow();
                self.collect_symbols_from_env(&env, candidates, prefix);
            }
        }
    }

    #[cfg(feature = "vm")]
    fn collect_symbols_from_table(
        &self,
        table: &SymbolTable,
        candidates: &mut Vec<TypedCompletion>,
        prefix: &str,
    ) {
        // Get all symbols from the symbol table
        let symbols = table.get_all_symbols();

        for symbol in symbols {
            if symbol.starts_with(prefix) {
                // Determine if it's a function or variable
                // For now, we'll mark all user symbols as variables
                // This could be enhanced if symbol table tracks types
                let completion_type = if self.builtin_functions.contains(symbol.as_str()) {
                    CompletionType::BuiltinFunction
                } else {
                    CompletionType::Variable
                };

                candidates.push(TypedCompletion {
                    text: symbol,
                    completion_type,
                });
            }
        }
    }

    #[cfg(not(feature = "vm"))]
    fn collect_symbols_from_env(
        &self,
        env: &Environment,
        candidates: &mut Vec<TypedCompletion>,
        prefix: &str,
    ) {
        // Get all symbols from the environment
        let symbols = env.get_all_symbols();

        for symbol in symbols {
            if symbol.starts_with(prefix) {
                // Determine if it's a function or variable
                let completion_type = if self.builtin_functions.contains(symbol.as_str()) {
                    CompletionType::BuiltinFunction
                } else {
                    CompletionType::Variable
                };

                candidates.push(TypedCompletion {
                    text: symbol,
                    completion_type,
                });
            }
        }
    }
}

#[derive(Debug, PartialEq)]
enum CompletionContext {
    StatementStart,
    Identifier,
    Expression,
    NewVariable,
}

#[derive(Debug, Clone, PartialEq)]
enum CompletionType {
    Keyword,
    BuiltinFunction,
    Variable,
}

#[derive(Debug, Clone)]
struct TypedCompletion {
    text: String,
    completion_type: CompletionType,
}

impl Completer for CaescriptCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Pair>), ReadlineError> {
        let mut completer = self.clone();
        let context = completer.get_completion_context(line, pos);
        let prefix = self.get_prefix(line, pos);
        let candidates = self.collect_candidates(prefix, context);

        let pairs: Vec<Pair> = candidates
            .into_iter()
            .map(|c| {
                let type_indicator = match &c.completion_type {
                    CompletionType::Keyword => "[keyword]",
                    CompletionType::BuiltinFunction => "[builtin]",
                    CompletionType::Variable => "[var]",
                };

                Pair {
                    display: format!("{:<5} {}", type_indicator, c.text),
                    replacement: c.text,
                }
            })
            .collect();

        Ok((pos - prefix.len(), pairs))
    }
}

// Clone implementation required for mutable borrow in complete()
impl Clone for CaescriptCompleter {
    fn clone(&self) -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_caescript::LANGUAGE.into())
            .expect("Error loading Caescript parser");

        Self {
            state: match &self.state {
                #[cfg(feature = "vm")]
                CompletionState::Vm { symbol_table } => CompletionState::Vm {
                    symbol_table: symbol_table.clone(),
                },
                #[cfg(not(feature = "vm"))]
                CompletionState::Interpreter { environment } => CompletionState::Interpreter {
                    environment: environment.clone(),
                },
            },
            parser,
            keywords: self.keywords.clone(),
            builtin_functions: self.builtin_functions.clone(),
        }
    }
}
