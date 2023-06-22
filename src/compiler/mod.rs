use anyhow::{anyhow, Result};

use crate::{ast, code};

use crate::eval::object;

use self::symbol_table::SymbolTable;

mod symbol_table;

#[derive(Debug, Default)]
pub struct Compiler {
    instructions: code::Instructions,
    consts: Vec<object::Object>,
    last_instruction: Option<EmittedInstruction>,
    prev_instruction: Option<EmittedInstruction>,
    symbol_table: SymbolTable,
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: code::Instructions,
    pub consts: Vec<object::Object>,
}

#[derive(Debug, Clone)]
pub struct EmittedInstruction {
    op: code::Op,
    pos: usize,
}

impl EmittedInstruction {
    fn new(op: code::Op, pos: usize) -> Self {
        Self { op, pos }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn compile(&mut self, program: &ast::Program) -> Result<Bytecode> {
        for stmt in program.iter() {
            self.compile_statement(stmt)?
        }
        Ok(self.bytecode())
    }

    fn compile_statement(&mut self, stmt: &ast::Statement) -> Result<()> {
        match stmt {
            ast::Statement::Expression(expr) => {
                self.compile_expression(expr)?;
                if !matches!(expr, ast::Expression::Assign(_, _, _)) {
                    self.emit(code::Op::Pop, &vec![]);
                }
            }
            ast::Statement::Let(ident, expr) => {
                self.compile_expression(expr)?;

                let symbol = self.symbol_table.define(ident.0.clone());
                self.emit(code::Op::SetGlobal, &vec![symbol.index]);
            }
            _ => todo!(),
        }
        Ok(())
    }

    fn compile_expression(&mut self, expr: &ast::Expression) -> Result<()> {
        match expr {
            ast::Expression::Literal(ast::Literal::Int(i)) => {
                let int = object::Object::Int(*i);
                let pos = self.add_const(int);
                self.emit(code::Op::Const, &vec![pos]);
            }
            ast::Expression::Literal(ast::Literal::Float(f)) => {
                let float = object::Object::Float(*f);
                let pos = self.add_const(float);
                self.emit(code::Op::Const, &vec![pos]);
            }
            ast::Expression::Literal(ast::Literal::String(ref s)) => {
                let s = object::Object::String(s.as_str().into());
                let pos = self.add_const(s);
                self.emit(code::Op::Const, &vec![pos]);
            }
            ast::Expression::Literal(ast::Literal::Bool(true)) => {
                self.emit(code::Op::True, &vec![]);
            }
            ast::Expression::Literal(ast::Literal::Bool(false)) => {
                self.emit(code::Op::False, &vec![]);
            }
            ast::Expression::Null => {
                self.emit(code::Op::Null, &vec![]);
            }
            ast::Expression::Ident(ident) => {
                let symbol = self
                    .symbol_table
                    .resolve(&ident)
                    .ok_or(anyhow!("undefined variable {}", &ident.0))?;
                self.emit(code::Op::GetGlobal, &vec![symbol.index]);
            }
            ast::Expression::Prefix(prefix, expr) => {
                self.compile_expression(expr)?;
                match prefix {
                    ast::Prefix::Minus => self.emit(code::Op::Minus, &vec![]),
                    ast::Prefix::Not => self.emit(code::Op::Not, &vec![]),
                };
            }
            ast::Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                self.compile_expression(condition)?;

                // Emit an `JumpNotTruthy` with a bogus value. After compiling the consequence, we will know
                // how far to jump and can "back-patching" it. Because the compiler is a single pass compiler
                // this is the solution, however more complex compilers may not come back to change it on first
                // pass and instead fill it in on another traversal
                let pos = self.emit(code::Op::JumpNotTruthy, &vec![9999]);
                self.compile(consequence)?;

                // evict redundant `Pop`
                if self.last_instruction_is(code::Op::Pop) {
                    self.remove_last();
                }

                let jump_pos = self.emit(code::Op::Jump, &vec![9999]);

                let mut after_pos = self.instructions.len();
                self.change_operand(pos, after_pos);

                if alternative.is_none() {
                    self.emit(code::Op::Null, &vec![]);
                } else {
                    self.compile(alternative.as_ref().unwrap())?;

                    if self.last_instruction_is(code::Op::Pop) {
                        self.remove_last();
                    }
                }
                after_pos = self.instructions.len();
                self.change_operand(jump_pos, after_pos);
            }
            ast::Expression::Infix(infix, lhs, rhs) => match infix {
                ast::Infix::Plus
                | ast::Infix::Minus
                | ast::Infix::Multiply
                | ast::Infix::Divide
                | ast::Infix::Mod
                | ast::Infix::And
                | ast::Infix::Or
                | ast::Infix::Eq
                | ast::Infix::Ne
                | ast::Infix::Gt
                | ast::Infix::GtEq => {
                    self.compile_expression(lhs)?;
                    self.compile_expression(rhs)?;
                    self.emit(infix.into(), &vec![]);
                }
                ast::Infix::Lt => {
                    self.compile_expression(rhs)?;
                    self.compile_expression(lhs)?;
                    self.emit(code::Op::Gt, &vec![]);
                }
                ast::Infix::LtEq => {
                    self.compile_expression(rhs)?;
                    self.compile_expression(lhs)?;
                    self.emit(code::Op::GtEq, &vec![]);
                }
            },
            ast::Expression::Assign(assign, ident, expr) => match assign {
                ast::Assign::Assign => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;
                    self.emit(code::Op::GetGlobal, &vec![symbol.index]);

                    self.compile_expression(expr)?;
                    let symbol = self.symbol_table.define(ident.0.clone());
                    self.emit(code::Op::SetGlobal, &vec![symbol.index]);
                }
                ast::Assign::PlusEq => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;

                    self.emit(code::Op::GetGlobal, &vec![symbol.index]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Add, &vec![]);

                    let symbol = self.symbol_table.define(ident.0.clone());
                    self.emit(code::Op::SetGlobal, &vec![symbol.index]);
                }
                ast::Assign::MinusEq => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;

                    self.emit(code::Op::GetGlobal, &vec![symbol.index]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Sub, &vec![]);

                    let symbol = self.symbol_table.define(ident.0.clone());
                    self.emit(code::Op::SetGlobal, &vec![symbol.index]);
                }
                ast::Assign::MultiplyEq => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;

                    self.emit(code::Op::GetGlobal, &vec![symbol.index]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Mul, &vec![]);

                    let symbol = self.symbol_table.define(ident.0.clone());
                    self.emit(code::Op::SetGlobal, &vec![symbol.index]);
                }
                ast::Assign::DivideEq => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;

                    self.emit(code::Op::GetGlobal, &vec![symbol.index]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Div, &vec![]);

                    let symbol = self.symbol_table.define(ident.0.clone());
                    self.emit(code::Op::SetGlobal, &vec![symbol.index]);
                }
                ast::Assign::ModEq => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;

                    self.emit(code::Op::GetGlobal, &vec![symbol.index]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Mod, &vec![]);

                    let symbol = self.symbol_table.define(ident.0.clone());
                    self.emit(code::Op::SetGlobal, &vec![symbol.index]);
                }
            },
            ast::Expression::Array(elems) => {
                elems
                    .iter()
                    .try_for_each(|elem| self.compile_expression(elem))?;

                self.emit(code::Op::Array, &vec![elems.len()]);
            }
            ast::Expression::Hash(kvs) => {
                kvs.iter().try_for_each(|(k, v)| {
                    self.compile_expression(k)?;
                    self.compile_expression(v)
                })?;

                self.emit(code::Op::Hash, &vec![kvs.len() * 2]);
            }
            ast::Expression::Index(expr, idx) => {
                self.compile_expression(&expr)?;
                self.compile_expression(&idx)?;
                self.emit(code::Op::Index, &vec![]);
            }
            _ => panic!("unknown expr: {}", expr),
        }
        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            consts: self.consts.clone(),
        }
    }

    fn add_const(&mut self, obj: object::Object) -> usize {
        self.consts.push(obj);
        self.consts.len() - 1
    }

    fn emit(&mut self, op: code::Op, operands: &Vec<usize>) -> usize {
        let ins = code::make(op, operands);
        let pos = self.add_instruction(ins);

        self.set_last_instruction(op, pos);
        pos
    }

    fn set_last_instruction(&mut self, op: code::Op, pos: usize) {
        self.prev_instruction = self.last_instruction.take();
        self.last_instruction = Some(EmittedInstruction::new(op, pos));
    }

    fn last_instruction_is(&mut self, op: code::Op) -> bool {
        self.last_instruction.as_ref().is_some_and(|i| i.op == op)
    }

    fn remove_last(&mut self) {
        self.last_instruction.as_ref().inspect(|ins| {
            *self.instructions = self.instructions[..ins.pos].into();
        });
        self.last_instruction = self.prev_instruction.take();
    }

    fn add_instruction(&mut self, ins: code::Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.extend_from_slice(ins.as_slice());
        pos
    }

    fn change_operand(&mut self, pos: usize, operand: usize) {
        let op: code::Op = unsafe { std::mem::transmute(self.instructions[pos]) };
        let new_instruction = code::make(op, &vec![operand]);

        new_instruction
            .into_iter()
            .enumerate()
            .for_each(|(idx, d)| self.instructions[pos + idx] = d)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        code::{concat_instructions, Instructions},
        lexer, parser,
    };

    use super::*;

    fn compile(input: &str, instructions: Vec<Instructions>) {
        let program = parser::Parser::new(lexer::Lexer::new(input))
            .parse_program()
            .unwrap();
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&program).unwrap();
        let res = concat_instructions(instructions);
        assert_eq!(
            res, bytecode.instructions,
            "test {} expect {}, got {} instead",
            input, res, bytecode.instructions
        );
    }

    #[test]
    fn compile_should_work() {
        let tests = [
            (
                "1;2",
                vec![
                    code::make(code::Op::Const, &vec![0]),
                    code::make(code::Op::Pop, &vec![]),
                    code::make(code::Op::Const, &vec![1]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![object::Object::Int(1), object::Object::Int(2)],
            ),
            (
                "true",
                vec![
                    code::make(code::Op::True, &vec![1]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![],
            ),
            (
                "false",
                vec![
                    code::make(code::Op::False, &vec![1]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![],
            ),
            (
                "if (true) {10} else {20}; 3333",
                vec![
                    // 0000
                    code::make(code::Op::True, &vec![]),
                    // 0001
                    code::make(code::Op::JumpNotTruthy, &vec![10]),
                    // 0004
                    code::make(code::Op::Const, &vec![0]),
                    // 0007
                    code::make(code::Op::Jump, &vec![13]),
                    // 0010
                    code::make(code::Op::Const, &vec![1]),
                    // 0013
                    code::make(code::Op::Pop, &vec![]),
                    // 0014
                    code::make(code::Op::Const, &vec![2]),
                    // 0017
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![
                    object::Object::Int(10),
                    object::Object::Int(20),
                    object::Object::Int(3333),
                ],
            ),
            (
                "if (true) {10}; 3333",
                vec![
                    // 0000
                    code::make(code::Op::True, &vec![]),
                    // 0001
                    code::make(code::Op::JumpNotTruthy, &vec![10]),
                    // 0004
                    code::make(code::Op::Const, &vec![0]),
                    // 0007
                    code::make(code::Op::Jump, &vec![11]),
                    // 0010
                    code::make(code::Op::Null, &vec![]),
                    // 0011
                    code::make(code::Op::Pop, &vec![]),
                    // 0012
                    code::make(code::Op::Const, &vec![1]),
                    // 0015
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![object::Object::Int(10), object::Object::Int(3333)],
            ),
        ];

        tests.into_iter().for_each(|test| compile(test.0, test.1))
    }

    #[test]
    fn change_operand_should_work() {
        let tests = [(
            "1;2",
            vec![
                // 0000
                code::make(code::Op::Const, &vec![0]),
                // 0003
                code::make(code::Op::Pop, &vec![]),
                // 0004
                code::make(code::Op::Const, &vec![10]),
                // 0007
                code::make(code::Op::Pop, &vec![]),
            ],
        )];

        tests.into_iter().for_each(|test| {
            let program = parser::Parser::new(lexer::Lexer::new(test.0))
                .parse_program()
                .unwrap();
            let mut compiler = Compiler::new();
            compiler.compile(&program).unwrap();

            compiler.change_operand(4, 10);

            let bytecode = compiler.bytecode();
            let res = concat_instructions(test.1);
            assert_eq!(
                res, bytecode.instructions,
                "expect {}, got {} instead",
                res, bytecode.instructions
            );
        })
    }

    #[test]
    fn compile_array_should_work() {
        let tests = [
            (
                "[]",
                vec![
                    code::make(code::Op::Array, &vec![0]),
                    code::make(code::Op::Pop, &vec![]),
                ],
            ),
            (
                "[1,2,3]",
                vec![
                    code::make(code::Op::Const, &vec![0]),
                    code::make(code::Op::Const, &vec![1]),
                    code::make(code::Op::Const, &vec![2]),
                    code::make(code::Op::Array, &vec![3]),
                    code::make(code::Op::Pop, &vec![]),
                ],
            ),
            (
                "[1,2,1+2]",
                vec![
                    code::make(code::Op::Const, &vec![0]),
                    code::make(code::Op::Const, &vec![1]),
                    code::make(code::Op::Const, &vec![2]),
                    code::make(code::Op::Const, &vec![3]),
                    code::make(code::Op::Add, &vec![]),
                    code::make(code::Op::Array, &vec![3]),
                    code::make(code::Op::Pop, &vec![]),
                ],
            ),
        ];

        tests.into_iter().for_each(|test| compile(test.0, test.1))
    }

    #[test]
    fn compile_hash_should_work() {
        let tests = [
            (
                "{}",
                vec![
                    code::make(code::Op::Hash, &vec![0]),
                    code::make(code::Op::Pop, &vec![]),
                ],
            ),
            (
                "{1: 2}",
                vec![
                    code::make(code::Op::Const, &vec![0]),
                    code::make(code::Op::Const, &vec![1]),
                    code::make(code::Op::Hash, &vec![2]),
                    code::make(code::Op::Pop, &vec![]),
                ],
            ),
            (
                "{1: 2, 4: 1+2}",
                vec![
                    code::make(code::Op::Const, &vec![0]),
                    code::make(code::Op::Const, &vec![1]),
                    code::make(code::Op::Const, &vec![2]),
                    code::make(code::Op::Const, &vec![3]),
                    code::make(code::Op::Const, &vec![4]),
                    code::make(code::Op::Add, &vec![]),
                    code::make(code::Op::Hash, &vec![4]),
                    code::make(code::Op::Pop, &vec![]),
                ],
            ),
        ];

        tests.into_iter().for_each(|test| compile(test.0, test.1))
    }
}
