use anyhow::{anyhow, Result};

use crate::{ast, code};

use crate::eval::object;

use self::scope::CompilationScope;
use self::symbol_table::{Scope, SymbolTable};

mod scope;
mod symbol_table;

#[derive(Debug, Default)]
pub struct Compiler {
    consts: Vec<object::Object>,
    scopes: Vec<CompilationScope>,
    scope_idx: usize,
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
        let main_scope = CompilationScope::default();
        Self {
            scopes: vec![main_scope],
            ..Self::default()
        }
    }

    pub fn compile(&mut self, program: &ast::Program) -> Result<Bytecode> {
        for stmt in program.iter() {
            self.compile_statement(stmt)?
        }

        // check invalid opcode
        let mut i = 0;
        while i < self.current_instructions().len() {
            let op: u8 = *self.current_instructions().get(i).unwrap();
            let op: code::Op = unsafe { std::mem::transmute(op) };

            if op == code::Op::Break || op == code::Op::Continue {
                Err(anyhow!("{} is only allowed in for loop", op))?
            }
            let offset: usize = op.operand_widths().iter().sum();
            i += 1 + offset;
        }

        Ok(self.bytecode())
    }

    fn compile_statements(&mut self, program: &ast::BlockStatement) -> Result<Bytecode> {
        for stmt in program.iter() {
            self.compile_statement(stmt)?
        }

        Ok(self.bytecode())
    }

    fn compile_statement(&mut self, stmt: &ast::Statement) -> Result<()> {
        match stmt {
            ast::Statement::Expression(expr) => {
                self.compile_expression(expr)?;
                // these expressions do not genreate value, so we do not
                // need to emit pop
                if !matches!(expr, ast::Expression::Assign(_, _, _))
                    && !matches!(expr, ast::Expression::For { .. })
                {
                    self.emit(code::Op::Pop, &vec![]);
                }
            }
            ast::Statement::Let(ident, expr) => {
                self.compile_expression(expr)?;

                let symbol = self.symbol_table.define(ident.0.clone());
                match symbol.scope {
                    Scope::Global => self.emit(code::Op::SetGlobal, &vec![symbol.index]),
                    Scope::Local => self.emit(code::Op::SetLocal, &vec![symbol.index]),
                };
            }
            ast::Statement::Return(expr) => {
                self.compile_expression(expr)?;
                self.emit(code::Op::ReturnValue, &vec![]);
            }
            // do not really need Break/Continue opcode, but we still have
            // some placeholder opcode and it will be replaced with `Jump`
            // during compiling for loop expression
            ast::Statement::Break => {
                self.emit(code::Op::Break, &vec![9999]);
            }
            ast::Statement::Continue => {
                self.emit(code::Op::Continue, &vec![9999]);
            }
            ast::Statement::Function(ident, params, body) => {
                // assamble a func expr manually
                self.compile_expression(&ast::Expression::Func {
                    params: params.clone(),
                    body: body.clone(),
                })?;

                let symbol = self.symbol_table.define(ident.0.clone());
                self.emit(code::Op::SetGlobal, &vec![symbol.index]);
            }
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
                match symbol.scope {
                    Scope::Global => self.emit(code::Op::GetGlobal, &vec![symbol.index]),
                    Scope::Local => self.emit(code::Op::GetLocal, &vec![symbol.index]),
                };
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

                // emit an `JumpNotTruthy` with a bogus value. After compiling the consequence, we will know
                // how far to jump and can "back-patching" it. Because the compiler is a single pass compiler
                // this is the solution, however more complex compilers may not come back to change it on first
                // pass and instead fill it in on another traversal
                let pos = self.emit(code::Op::JumpNotTruthy, &vec![9999]);
                self.compile_statements(consequence)?;

                // evict redundant `Pop`
                if self.last_instruction_is(code::Op::Pop) {
                    self.remove_last();
                }

                let jump_pos = self.emit(code::Op::Jump, &vec![9999]);

                let mut after_pos = self.current_instructions().len();
                self.change_operand(pos, after_pos);

                if alternative.is_none() {
                    self.emit(code::Op::Null, &vec![]);
                } else {
                    self.compile_statements(alternative.as_ref().unwrap())?;

                    if self.last_instruction_is(code::Op::Pop) {
                        self.remove_last();
                    }
                }
                after_pos = self.current_instructions().len();
                self.change_operand(jump_pos, after_pos);
            }
            ast::Expression::For {
                condition,
                consequence,
            } => {
                let start = self.current_instructions().len();
                self.compile_expression(condition)?;

                let pos = self.emit(code::Op::JumpNotTruthy, &vec![9999]);
                self.compile_statements(consequence)?;

                // evict redundant `Pop`
                if self.last_instruction_is(code::Op::Pop) {
                    self.remove_last();
                }

                self.change_op(code::Op::Continue, code::Op::Jump, &vec![start]);

                // go back to the start
                self.emit(code::Op::Jump, &vec![start]);

                let after_pos = self.current_instructions().len();
                self.change_operand(pos, after_pos);
                self.change_op(code::Op::Break, code::Op::Jump, &vec![after_pos]);
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

                    let idx = symbol.index;

                    self.emit(code::Op::GetGlobal, &vec![idx]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Add, &vec![]);

                    self.emit(code::Op::SetGlobal, &vec![idx]);
                }
                ast::Assign::MinusEq => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;

                    let idx = symbol.index;

                    self.emit(code::Op::GetGlobal, &vec![idx]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Sub, &vec![]);

                    self.emit(code::Op::SetGlobal, &vec![idx]);
                }
                ast::Assign::MultiplyEq => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;

                    let idx = symbol.index;

                    self.emit(code::Op::GetGlobal, &vec![idx]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Mul, &vec![]);

                    self.emit(code::Op::SetGlobal, &vec![idx]);
                }
                ast::Assign::DivideEq => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;

                    let idx = symbol.index;

                    self.emit(code::Op::GetGlobal, &vec![idx]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Div, &vec![]);

                    self.emit(code::Op::SetGlobal, &vec![idx]);
                }
                ast::Assign::ModEq => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident)
                        .ok_or(anyhow!("undefined variable {}", &ident.0))?;

                    let idx = symbol.index;

                    self.emit(code::Op::GetGlobal, &vec![idx]);
                    self.compile_expression(expr)?;
                    self.emit(code::Op::Mod, &vec![]);

                    self.emit(code::Op::SetGlobal, &vec![idx]);
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
            ast::Expression::Func { params, body } => {
                self.enter_scope();
                self.compile_statements(body)?;

                if self.last_instruction_is(code::Op::Pop) {
                    self.replace_last_op(code::Op::ReturnValue)
                }
                if !self.last_instruction_is(code::Op::ReturnValue) {
                    self.emit(code::Op::Return, &vec![]);
                }

                let num_local = self.symbol_table.count;
                let ins = self.leave_scope();

                let operand = self.add_const(object::Object::CompiledFunction(ins, num_local));
                self.emit(code::Op::Const, &vec![operand]);
            }
            ast::Expression::Call { func, args } => {
                self.compile_expression(func)?;

                self.emit(code::Op::Call, &vec![]);
            }
        }
        Ok(())
    }

    pub fn bytecode(&mut self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
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
        self.scopes[self.scope_idx].prev = self.scopes[self.scope_idx].last.take();
        self.scopes[self.scope_idx].last = Some(EmittedInstruction::new(op, pos));
    }

    fn last_instruction_is(&mut self, op: code::Op) -> bool {
        self.scopes[self.scope_idx]
            .last
            .as_ref()
            .is_some_and(|i| i.op == op)
    }

    fn remove_last(&mut self) {
        self.scopes[self.scope_idx]
            .last
            .clone()
            .map(|ins| self.scopes[self.scope_idx].instructions.truncate(ins.pos));

        self.scopes[self.scope_idx].last = self.scopes[self.scope_idx].prev.take();
    }

    fn current_instructions(&mut self) -> &mut code::Instructions {
        &mut self.scopes[self.scope_idx].instructions
    }

    fn add_instruction(&mut self, ins: code::Instructions) -> usize {
        let pos = self.current_instructions().len();

        self.current_instructions()
            .extend_from_slice(ins.as_slice());

        pos
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::default();

        self.scopes.push(scope);
        self.scope_idx += 1;
        self.symbol_table = SymbolTable::enclosed(self.symbol_table.clone());
    }

    fn leave_scope(&mut self) -> code::Instructions {
        let ins = self.current_instructions().clone();

        self.scopes.pop();
        self.scope_idx -= 1;
        self.symbol_table = self.symbol_table.outer.clone().unwrap().take();

        ins
    }

    fn change_operand(&mut self, pos: usize, operand: usize) {
        let op: code::Op = unsafe { std::mem::transmute(self.current_instructions()[pos]) };
        let new_instruction = code::make(op, &vec![operand]);

        new_instruction
            .iter()
            .enumerate()
            .for_each(|(idx, d)| self.current_instructions()[pos + idx] = *d)
    }

    fn replace_last_op(&mut self, to: code::Op) {
        let last = self.scopes[self.scope_idx].last.clone().unwrap().pos;

        let new_instruction = code::make(code::Op::ReturnValue, &vec![]);

        new_instruction
            .iter()
            .enumerate()
            .for_each(|(idx, d)| self.current_instructions()[last + idx] = *d);

        self.scopes[self.scope_idx]
            .last
            .as_mut()
            .map(|x| x.op = code::Op::ReturnValue);
    }

    fn change_op(&mut self, from: code::Op, to: code::Op, operands: &Vec<usize>) {
        let new_instruction = code::make(to, operands);

        let mut idxs = vec![];
        let mut i = 0;

        while i < self.current_instructions().len() {
            let op: u8 = *self.current_instructions().get(i).unwrap();
            let op: code::Op = unsafe { std::mem::transmute(op) };

            if op == from {
                idxs.push(i)
            }
            let offset: usize = op.operand_widths().iter().sum();
            i += 1 + offset;
        }

        idxs.iter().for_each(|pos| {
            new_instruction
                .iter()
                .enumerate()
                .for_each(|(idx, d)| self.current_instructions()[pos + idx] = *d)
        })
    }
}

#[cfg(test)]
mod test {
    use crate::{
        code::{concat_instructions, Instructions},
        lexer, parser,
    };

    use super::*;

    fn compile(input: &str, instructions: Vec<Instructions>, consts: Vec<object::Object>) {
        let program = parser::Parser::new(lexer::Lexer::new(input))
            .parse_program()
            .unwrap();
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&program).unwrap();
        let res = concat_instructions(instructions);
        assert_eq!(
            res, bytecode.instructions,
            "test {} expect instructions to be {}, got {} instead",
            input, res, bytecode.instructions
        );
        assert_eq!(
            consts, bytecode.consts,
            "test {} expect consts to be {:?}, got {:?} instead",
            input, consts, bytecode.consts
        )
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

        tests
            .into_iter()
            .for_each(|test| compile(test.0, test.1, test.2))
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
    fn change_op_should_work() {
        let tests = [(
            "1 + 2",
            vec![
                code::make(code::Op::Const, &vec![0]),
                code::make(code::Op::Const, &vec![1]),
                code::make(code::Op::Sub, &vec![]),
                code::make(code::Op::Pop, &vec![]),
            ],
        )];

        tests.into_iter().for_each(|test| {
            let program = parser::Parser::new(lexer::Lexer::new(test.0))
                .parse_program()
                .unwrap();
            let mut compiler = Compiler::new();
            compiler.compile(&program).unwrap();

            compiler.change_op(code::Op::Add, code::Op::Sub, &vec![]);

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
                vec![],
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
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::Int(3),
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
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::Int(1),
                    object::Object::Int(2),
                ],
            ),
        ];

        tests
            .into_iter()
            .for_each(|test| compile(test.0, test.1, test.2))
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
                vec![],
            ),
            (
                "{1: 2}",
                vec![
                    code::make(code::Op::Const, &vec![0]),
                    code::make(code::Op::Const, &vec![1]),
                    code::make(code::Op::Hash, &vec![2]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![object::Object::Int(1), object::Object::Int(2)],
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
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::Int(4),
                    object::Object::Int(1),
                    object::Object::Int(2),
                ],
            ),
        ];

        tests
            .into_iter()
            .for_each(|test| compile(test.0, test.1, test.2))
    }

    #[test]
    fn compile_for_loop_should_work() {
        let tests = [(
            "let a = 1; for (a<10) {a+=2}; 3333",
            vec![
                // 0000
                code::make(code::Op::Const, &vec![0]),
                // 0003
                code::make(code::Op::SetGlobal, &vec![0]),
                // 0006
                code::make(code::Op::Const, &vec![1]),
                // 0009
                code::make(code::Op::GetGlobal, &vec![0]),
                // 0012
                code::make(code::Op::Gt, &vec![]),
                // 0013
                code::make(code::Op::JumpNotTruthy, &vec![29]),
                // 0016
                code::make(code::Op::GetGlobal, &vec![0]),
                // 0019
                code::make(code::Op::Const, &vec![2]),
                // 0022
                code::make(code::Op::Add, &vec![]),
                // 0023
                code::make(code::Op::SetGlobal, &vec![0]),
                // 0026
                code::make(code::Op::Jump, &vec![6]),
                // 0029
                code::make(code::Op::Const, &vec![3]),
                // 0032
                code::make(code::Op::Pop, &vec![]),
            ],
            vec![
                object::Object::Int(1),
                object::Object::Int(10),
                object::Object::Int(2),
                object::Object::Int(3333),
            ],
        )];

        tests
            .into_iter()
            .for_each(|test| compile(test.0, test.1, test.2))
    }

    #[test]
    fn compile_break_continue_should_fail() {
        let tests = [
            ("break;", anyhow!("Break is only allowed in for loop")),
            ("continue;", anyhow!("Continue is only allowed in for loop")),
        ];

        tests.into_iter().for_each(|test| {
            let program = parser::Parser::new(lexer::Lexer::new(test.0))
                .parse_program()
                .unwrap();
            let mut compiler = Compiler::new();
            let bytecode = compiler.compile(&program);
            assert!(bytecode.is_err_and(|x| x.to_string() == test.1.to_string()));
        })
    }

    #[test]
    fn compile_fn_should_work() {
        let tests = [
            (
                "fn() { return 1+2 }",
                vec![
                    code::make(code::Op::Const, &vec![2]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &vec![0]),
                            code::make(code::Op::Const, &vec![1]),
                            code::make(code::Op::Add, &vec![]),
                            code::make(code::Op::ReturnValue, &vec![]),
                        ]),
                        0,
                    ),
                ],
            ),
            (
                "fn() { 1+2 }",
                vec![
                    code::make(code::Op::Const, &vec![2]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &vec![0]),
                            code::make(code::Op::Const, &vec![1]),
                            code::make(code::Op::Add, &vec![]),
                            code::make(code::Op::ReturnValue, &vec![]),
                        ]),
                        0,
                    ),
                ],
            ),
            (
                "fn() { 1;2 }",
                vec![
                    code::make(code::Op::Const, &vec![2]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &vec![0]),
                            code::make(code::Op::Pop, &vec![]),
                            code::make(code::Op::Const, &vec![1]),
                            code::make(code::Op::ReturnValue, &vec![]),
                        ]),
                        0,
                    ),
                ],
            ),
            (
                "fn() {}",
                vec![
                    code::make(code::Op::Const, &vec![0]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![object::Object::CompiledFunction(
                    concat_instructions(vec![code::make(code::Op::Return, &vec![])]),
                    0,
                )],
            ),
            (
                "fn() { 1;2 }()",
                vec![
                    code::make(code::Op::Const, &vec![2]),
                    code::make(code::Op::Call, &vec![]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &vec![0]),
                            code::make(code::Op::Pop, &vec![]),
                            code::make(code::Op::Const, &vec![1]),
                            code::make(code::Op::ReturnValue, &vec![]),
                        ]),
                        0,
                    ),
                ],
            ),
            (
                "fn() { let a = 1; a }",
                vec![
                    code::make(code::Op::Const, &vec![1]),
                    code::make(code::Op::Pop, &vec![]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &vec![0]),
                            code::make(code::Op::SetLocal, &vec![0]),
                            code::make(code::Op::GetLocal, &vec![0]),
                            code::make(code::Op::ReturnValue, &vec![]),
                        ]),
                        1,
                    ),
                ],
            ),
        ];

        tests
            .into_iter()
            .for_each(|test| compile(test.0, test.1, test.2))
    }
}
