use std::rc::Rc;

use anyhow::{anyhow, Result};

use crate::{ast, code};

use crate::eval::{builtin, object};

use self::scope::{CompilationScope, EmittedInstruction};
use self::symbol_table::{Scope, SymbolTable};

mod scope;
pub mod symbol_table;

/// Compiler is responsible for compiling the AST into bytecode.
///
/// It maintains:
/// - A list of constants encountered during compilation
/// - A stack of compilation scopes to handle nested blocks
/// - The current scope index
/// - A symbol table for tracking variables and their scopes
///
/// The compiler traverses the AST, emitting bytecode instructions and
/// managing scopes and symbols as it goes. It produces a Bytecode struct
/// containing the compiled instructions and constants.
#[derive(Debug, Default)]
pub struct Compiler {
    consts: Vec<Rc<object::Object>>,
    scopes: Vec<CompilationScope>,
    scope_idx: usize,
    pub symbol_table: SymbolTable,
}

/// Bytecode is the output of the compiler, which contains the compiled
/// instructions and constants.
#[derive(Debug, Clone)]
pub struct Bytecode<'a> {
    pub instructions: &'a object::Instructions,
    pub consts: &'a [Rc<object::Object>],
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope::default();
        Self {
            scopes: vec![main_scope],
            symbol_table: SymbolTable::new(),
            ..Self::default()
        }
    }

    pub fn new_with_builtins(f: impl FnOnce() -> Vec<(String, builtin::BuiltinFn)>) -> Self {
        let main_scope = CompilationScope::default();
        Self {
            scopes: vec![main_scope],
            symbol_table: SymbolTable::new_with_builtins(f),
            ..Self::default()
        }
    }

    pub fn new_with_state(symbol_table: SymbolTable, consts: Vec<Rc<object::Object>>) -> Self {
        let main_scope = CompilationScope::default();
        Self {
            scopes: vec![main_scope],
            symbol_table,
            consts,
            ..Self::default()
        }
    }

    pub fn compile(&mut self, program: &ast::Program) -> Result<Bytecode> {
        for stmt in program.iter() {
            self.compile_statement(stmt)?
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
                // these expressions do not generate value, so we do not
                // need to emit pop
                if !matches!(expr, ast::Expression::Assign(_, _, _))
                    && !matches!(expr, ast::Expression::For { .. })
                {
                    self.emit(code::Op::Pop, &[]);
                }
            }
            ast::Statement::Let(ident, expr) => {
                self.compile_expression(expr)?;

                let symbol = self.symbol_table.define(ident.0.clone());
                self.emit_set(&symbol);
            }
            ast::Statement::Return(expr) => {
                self.compile_expression(expr)?;
                self.emit(code::Op::ReturnValue, &[]);
            }
            // do not really need Break/Continue opcode, but we still have
            // some placeholder opcode and it will be replaced with `Jump`
            // during compiling for loop expression
            ast::Statement::Break => {
                if !self.is_in_loop() {
                    return Err(anyhow!("Break is only allowed in for loop"));
                }
                self.emit(code::Op::Break, &[9999]);
            }
            ast::Statement::Continue => {
                if !self.is_in_loop() {
                    return Err(anyhow!("Continue is only allowed in for loop"));
                }
                self.emit(code::Op::Continue, &[9999]);
            }
            ast::Statement::Function(ident, params, body) => {
                // assemble a func expr manually
                self.compile_expression(&ast::Expression::Func {
                    name: Some(ident.0.clone()),
                    params: params.clone(),
                    body: body.clone(),
                })?;

                let symbol = self.symbol_table.define(ident.0.clone());
                self.emit_set(&symbol);
            }
        }
        Ok(())
    }

    fn compile_expression(&mut self, expr: &ast::Expression) -> Result<()> {
        match expr {
            ast::Expression::Literal(ast::Literal::Int(i)) => {
                let int = object::Object::Int(*i);
                let pos = self.add_const(int);
                self.emit(code::Op::Const, &[pos]);
            }
            ast::Expression::Literal(ast::Literal::Float(f)) => {
                let float = object::Object::Float(*f);
                let pos = self.add_const(float);
                self.emit(code::Op::Const, &[pos]);
            }
            ast::Expression::Literal(ast::Literal::String(ref s)) => {
                let s = object::Object::String(s.as_str().into());
                let pos = self.add_const(s);
                self.emit(code::Op::Const, &[pos]);
            }
            ast::Expression::Literal(ast::Literal::Bool(true)) => {
                self.emit(code::Op::True, &[]);
            }
            ast::Expression::Literal(ast::Literal::Bool(false)) => {
                self.emit(code::Op::False, &[]);
            }
            ast::Expression::Null => {
                self.emit(code::Op::Null, &[]);
            }
            ast::Expression::Ident(ident) => {
                let symbol = self
                    .symbol_table
                    .resolve(ident)
                    .ok_or_else(|| anyhow!("undefined variable {}", &ident.0))?;
                self.emit_get(&symbol);
            }
            ast::Expression::Prefix(prefix, expr) => {
                self.compile_expression(expr)?;
                match prefix {
                    ast::Prefix::Minus => self.emit(code::Op::Minus, &[]),
                    ast::Prefix::Not => self.emit(code::Op::Not, &[]),
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
                let pos = self.emit(code::Op::JumpNotTruthy, &[9999]);
                self.compile_statements(consequence)?;

                // evict redundant `Pop`
                if self.last_instruction_is(code::Op::Pop) {
                    self.remove_last();
                }

                let jump_pos = self.emit(code::Op::Jump, &[9999]);

                let mut after_pos = self.current_instructions_mut().len();
                self.change_operand(pos, after_pos);

                if alternative.is_none() {
                    self.emit(code::Op::Null, &[]);
                } else {
                    self.compile_statements(alternative.as_ref().unwrap())?;

                    if self.last_instruction_is(code::Op::Pop) {
                        self.remove_last();
                    }
                }
                after_pos = self.current_instructions_mut().len();
                self.change_operand(jump_pos, after_pos);
            }
            ast::Expression::For {
                condition,
                consequence,
            } => {
                let start = self.current_instructions_mut().len();
                self.compile_expression(condition)?;

                let pos = self.emit(code::Op::JumpNotTruthy, &[9999]);

                self.enter_loop();
                self.compile_statements(consequence)?;
                self.leave_loop();

                // evict redundant `Pop`
                if self.last_instruction_is(code::Op::Pop) {
                    self.remove_last();
                }

                self.change_op(code::Op::Continue, code::Op::Jump, &[start]);

                // go back to the start
                self.emit(code::Op::Jump, &[start]);

                let after_pos = self.current_instructions_mut().len();
                self.change_operand(pos, after_pos);
                self.change_op(code::Op::Break, code::Op::Jump, &[after_pos]);
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
                | ast::Infix::GtEq
                | ast::Infix::LeftShift
                | ast::Infix::RightShift
                | ast::Infix::BitAnd
                | ast::Infix::BitOr
                | ast::Infix::BitXor => {
                    self.compile_expression(lhs)?;
                    self.compile_expression(rhs)?;
                    self.emit(infix.into(), &[]);
                }
                ast::Infix::Lt => {
                    self.compile_expression(rhs)?;
                    self.compile_expression(lhs)?;
                    self.emit(code::Op::Gt, &[]);
                }
                ast::Infix::LtEq => {
                    self.compile_expression(rhs)?;
                    self.compile_expression(lhs)?;
                    self.emit(code::Op::GtEq, &[]);
                }
            },
            ast::Expression::Assign(assign, ident, expr) => {
                let symbol = self
                    .symbol_table
                    .resolve(ident)
                    .ok_or_else(|| anyhow!("undefined variable {}", &ident.0))?;
                match assign {
                    ast::Assign::Assign => {
                        self.compile_expression(expr)?;
                    }
                    ast::Assign::PlusEq => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::Add, &[]);
                    }
                    ast::Assign::MinusEq => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::Sub, &[]);
                    }
                    ast::Assign::MultiplyEq => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::Mul, &[]);
                    }
                    ast::Assign::DivideEq => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::Div, &[]);
                    }
                    ast::Assign::ModEq => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::Mod, &[]);
                    }
                    ast::Assign::ShrAssign => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::RightShift, &[]);
                    }
                    ast::Assign::ShlAssign => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::LeftShift, &[]);
                    }
                    ast::Assign::BitOrAssign => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::BitOr, &[]);
                    }
                    ast::Assign::BitAndAssign => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::BitAnd, &[]);
                    }
                    ast::Assign::BitXorAssign => {
                        self.emit_get(&symbol);

                        self.compile_expression(expr)?;
                        self.emit(code::Op::BitXor, &[]);
                    }
                }
                self.emit_set(&symbol);
            }
            ast::Expression::Array(elems) => {
                elems
                    .iter()
                    .try_for_each(|elem| self.compile_expression(elem))?;

                self.emit(code::Op::Array, &[elems.len()]);
            }
            ast::Expression::Hash(kvs) => {
                kvs.iter().try_for_each(|(k, v)| {
                    self.compile_expression(k)?;
                    self.compile_expression(v)
                })?;

                self.emit(code::Op::Hash, &[kvs.len() * 2]);
            }
            ast::Expression::Index(expr, idx) => {
                self.compile_expression(expr)?;
                self.compile_expression(idx)?;
                self.emit(code::Op::Index, &[]);
            }
            ast::Expression::Func { name, params, body } => {
                self.enter_scope();

                name.as_ref()
                    .map(|name| self.symbol_table.define_function(name.clone()));

                params.iter().for_each(|param| {
                    self.symbol_table.define(param.0.clone());
                });

                self.compile_statements(body)?;

                if self.last_instruction_is(code::Op::Pop) {
                    self.replace_last_op(code::Op::ReturnValue)
                }
                if !self.last_instruction_is(code::Op::ReturnValue) {
                    self.emit(code::Op::Return, &[]);
                }

                let free = self.symbol_table.free.clone();
                let num_local = self.symbol_table.count;
                let ins = self.leave_scope();

                free.iter().for_each(|f| self.emit_get(f));

                let operand = self.add_const(object::Object::CompiledFunction(
                    Rc::new(ins),
                    num_local,
                    params.len(),
                ));
                self.emit(code::Op::Closure, &[operand, free.len()]);
            }
            ast::Expression::Call { func, args } => {
                self.compile_expression(func)?;

                args.iter()
                    .try_for_each(|arg| self.compile_expression(arg))?;

                self.emit(code::Op::Call, &[args.len()]);
            }
        }
        Ok(())
    }

    pub fn bytecode(&mut self) -> Bytecode<'_> {
        Bytecode {
            instructions: self.current_instructions(),
            consts: &self.consts,
        }
    }

    fn add_const(&mut self, obj: object::Object) -> usize {
        self.consts.push(Rc::new(obj));
        self.consts.len() - 1
    }

    fn emit(&mut self, op: code::Op, operands: &[usize]) -> usize {
        let ins = code::make(op, operands);
        let pos = self.add_instruction(ins);

        self.set_last_instruction(op, pos);
        pos
    }

    fn emit_get(&mut self, symbol: &symbol_table::Symbol) {
        match symbol.scope {
            Scope::Global => self.emit(code::Op::GetGlobal, &[symbol.index]),
            Scope::Local => self.emit(code::Op::GetLocal, &[symbol.index]),
            Scope::Builtin => self.emit(code::Op::GetBuiltin, &[symbol.index]),
            Scope::Free => self.emit(code::Op::GetFree, &[symbol.index]),
            Scope::Function => self.emit(code::Op::GetCurrentClosure, &[symbol.index]),
        };
    }

    fn emit_set(&mut self, symbol: &symbol_table::Symbol) {
        match symbol.scope {
            Scope::Global => self.emit(code::Op::SetGlobal, &[symbol.index]),
            Scope::Local => self.emit(code::Op::SetLocal, &[symbol.index]),
            Scope::Free => self.emit(code::Op::SetFree, &[symbol.index]),
            _ => unreachable!(),
        };
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
        let mut pos = 0;
        if let Some(ref ins) = self.scopes[self.scope_idx].last {
            pos = ins.pos;
        }
        if pos != 0 {
            self.scopes[self.scope_idx].instructions.truncate(pos);
        }

        self.scopes[self.scope_idx].last = self.scopes[self.scope_idx].prev.take();
    }

    fn current_instructions_mut(&mut self) -> &mut object::Instructions {
        &mut self.scopes[self.scope_idx].instructions
    }

    fn current_instructions(&self) -> &object::Instructions {
        &self.scopes[self.scope_idx].instructions
    }

    fn add_instruction(&mut self, ins: object::Instructions) -> usize {
        let pos = self.current_instructions_mut().len();

        self.current_instructions_mut()
            .extend_from_slice(ins.as_slice());

        pos
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::default();

        self.scopes.push(scope);
        self.scope_idx += 1;
        self.symbol_table = SymbolTable::enclosed(self.symbol_table.clone());
    }

    fn enter_loop(&mut self) {
        self.scopes[self.scope_idx].is_loop = true;
    }

    fn leave_loop(&mut self) {
        self.scopes[self.scope_idx].is_loop = false;
    }

    fn is_in_loop(&mut self) -> bool {
        self.scopes[self.scope_idx].is_loop
    }

    fn leave_scope(&mut self) -> object::Instructions {
        let ins = self.current_instructions_mut().clone();

        self.scopes.pop();
        self.scope_idx -= 1;

        self.symbol_table = self
            .symbol_table
            .outer
            .as_ref()
            .map(|outer| outer.as_ref().clone())
            .expect("top-level scope");

        ins
    }

    fn change_operand(&mut self, pos: usize, operand: usize) {
        let op = code::Op::from_repr(self.current_instructions_mut()[pos]).unwrap();
        let new_instruction = code::make(op, &[operand]);

        new_instruction
            .iter()
            .enumerate()
            .for_each(|(idx, d)| self.current_instructions_mut()[pos + idx] = *d)
    }

    fn replace_last_op(&mut self, to: code::Op) {
        let last = self.scopes[self.scope_idx].last.clone().unwrap().pos;

        let new_instruction = code::make(code::Op::ReturnValue, &[]);

        new_instruction
            .iter()
            .enumerate()
            .for_each(|(idx, d)| self.current_instructions_mut()[last + idx] = *d);

        if let Some(x) = self.scopes[self.scope_idx].last.as_mut() {
            x.op = to
        }
    }

    fn change_op(&mut self, from: code::Op, to: code::Op, operands: &[usize]) {
        let new_instruction = code::make(to, operands);

        let mut idxs = vec![];
        let mut i = 0;

        while i < self.current_instructions_mut().len() {
            let op: u8 = *self.current_instructions_mut().get(i).unwrap();
            let op = code::Op::from_repr(op).unwrap();

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
                .for_each(|(idx, d)| self.current_instructions_mut()[pos + idx] = *d)
        })
    }
}

#[cfg(test)]
mod test {
    use crate::{code::concat_instructions, eval::object::Instructions, lexer, parser};

    use super::*;

    fn compile(input: &str, instructions: Vec<Instructions>, consts: Vec<object::Object>) {
        let consts = consts
            .into_iter()
            .map(Rc::new)
            .collect::<Vec<Rc<object::Object>>>();
        let program = parser::Parser::new(lexer::Lexer::new(input))
            .parse_program()
            .unwrap();
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&program).unwrap();
        let res = concat_instructions(instructions);
        assert_eq!(
            &*res, bytecode.instructions,
            "test {} expect instructions to be {}, got {} instead",
            input, res, bytecode.instructions
        );
        assert_eq!(
            &consts, bytecode.consts,
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
                    code::make(code::Op::Const, &[0]),
                    code::make(code::Op::Pop, &[]),
                    code::make(code::Op::Const, &[1]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![object::Object::Int(1), object::Object::Int(2)],
            ),
            (
                "true",
                vec![
                    code::make(code::Op::True, &[1]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![],
            ),
            (
                "false",
                vec![
                    code::make(code::Op::False, &[1]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![],
            ),
            (
                "if (true) {10} else {20}; 3333",
                vec![
                    // 0000
                    code::make(code::Op::True, &[]),
                    // 0001
                    code::make(code::Op::JumpNotTruthy, &[10]),
                    // 0004
                    code::make(code::Op::Const, &[0]),
                    // 0007
                    code::make(code::Op::Jump, &[13]),
                    // 0010
                    code::make(code::Op::Const, &[1]),
                    // 0013
                    code::make(code::Op::Pop, &[]),
                    // 0014
                    code::make(code::Op::Const, &[2]),
                    // 0017
                    code::make(code::Op::Pop, &[]),
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
                    code::make(code::Op::True, &[]),
                    // 0001
                    code::make(code::Op::JumpNotTruthy, &[10]),
                    // 0004
                    code::make(code::Op::Const, &[0]),
                    // 0007
                    code::make(code::Op::Jump, &[11]),
                    // 0010
                    code::make(code::Op::Null, &[]),
                    // 0011
                    code::make(code::Op::Pop, &[]),
                    // 0012
                    code::make(code::Op::Const, &[1]),
                    // 0015
                    code::make(code::Op::Pop, &[]),
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
                code::make(code::Op::Const, &[0]),
                // 0003
                code::make(code::Op::Pop, &[]),
                // 0004
                code::make(code::Op::Const, &[10]),
                // 0007
                code::make(code::Op::Pop, &[]),
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
                &*res, bytecode.instructions,
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
                code::make(code::Op::Const, &[0]),
                code::make(code::Op::Const, &[1]),
                code::make(code::Op::Sub, &[]),
                code::make(code::Op::Pop, &[]),
            ],
        )];

        tests.into_iter().for_each(|test| {
            let program = parser::Parser::new(lexer::Lexer::new(test.0))
                .parse_program()
                .unwrap();
            let mut compiler = Compiler::new();
            compiler.compile(&program).unwrap();

            compiler.change_op(code::Op::Add, code::Op::Sub, &[]);

            let bytecode = compiler.bytecode();
            let res = concat_instructions(test.1);
            assert_eq!(
                &*res, bytecode.instructions,
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
                    code::make(code::Op::Array, &[0]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![],
            ),
            (
                "[1,2,3]",
                vec![
                    code::make(code::Op::Const, &[0]),
                    code::make(code::Op::Const, &[1]),
                    code::make(code::Op::Const, &[2]),
                    code::make(code::Op::Array, &[3]),
                    code::make(code::Op::Pop, &[]),
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
                    code::make(code::Op::Const, &[0]),
                    code::make(code::Op::Const, &[1]),
                    code::make(code::Op::Const, &[2]),
                    code::make(code::Op::Const, &[3]),
                    code::make(code::Op::Add, &[]),
                    code::make(code::Op::Array, &[3]),
                    code::make(code::Op::Pop, &[]),
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
                    code::make(code::Op::Hash, &[0]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![],
            ),
            (
                "{1: 2}",
                vec![
                    code::make(code::Op::Const, &[0]),
                    code::make(code::Op::Const, &[1]),
                    code::make(code::Op::Hash, &[2]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![object::Object::Int(1), object::Object::Int(2)],
            ),
            (
                "{1: 2, 4: 1+2}",
                vec![
                    code::make(code::Op::Const, &[0]),
                    code::make(code::Op::Const, &[1]),
                    code::make(code::Op::Const, &[2]),
                    code::make(code::Op::Const, &[3]),
                    code::make(code::Op::Const, &[4]),
                    code::make(code::Op::Add, &[]),
                    code::make(code::Op::Hash, &[4]),
                    code::make(code::Op::Pop, &[]),
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
                code::make(code::Op::Const, &[0]),
                // 0003
                code::make(code::Op::SetGlobal, &[0]),
                // 0006
                code::make(code::Op::Const, &[1]),
                // 0009
                code::make(code::Op::GetGlobal, &[0]),
                // 0012
                code::make(code::Op::Gt, &[]),
                // 0013
                code::make(code::Op::JumpNotTruthy, &[29]),
                // 0016
                code::make(code::Op::GetGlobal, &[0]),
                // 0019
                code::make(code::Op::Const, &[2]),
                // 0022
                code::make(code::Op::Add, &[]),
                // 0023
                code::make(code::Op::SetGlobal, &[0]),
                // 0026
                code::make(code::Op::Jump, &[6]),
                // 0029
                code::make(code::Op::Const, &[3]),
                // 0032
                code::make(code::Op::Pop, &[]),
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
                    code::make(code::Op::Closure, &[2, 0]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &[0]),
                            code::make(code::Op::Const, &[1]),
                            code::make(code::Op::Add, &[]),
                            code::make(code::Op::ReturnValue, &[]),
                        ]),
                        0,
                        0,
                    ),
                ],
            ),
            (
                "fn() { 1+2 }",
                vec![
                    code::make(code::Op::Closure, &[2, 0]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &[0]),
                            code::make(code::Op::Const, &[1]),
                            code::make(code::Op::Add, &[]),
                            code::make(code::Op::ReturnValue, &[]),
                        ]),
                        0,
                        0,
                    ),
                ],
            ),
            (
                "fn() { 1;2 }",
                vec![
                    code::make(code::Op::Closure, &[2, 0]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &[0]),
                            code::make(code::Op::Pop, &[]),
                            code::make(code::Op::Const, &[1]),
                            code::make(code::Op::ReturnValue, &[]),
                        ]),
                        0,
                        0,
                    ),
                ],
            ),
            (
                "fn() {}",
                vec![
                    code::make(code::Op::Closure, &[0, 0]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![object::Object::CompiledFunction(
                    concat_instructions(vec![code::make(code::Op::Return, &[])]),
                    0,
                    0,
                )],
            ),
            (
                "fn() { 1;2 }()",
                vec![
                    code::make(code::Op::Closure, &[2, 0]),
                    code::make(code::Op::Call, &[0]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &[0]),
                            code::make(code::Op::Pop, &[]),
                            code::make(code::Op::Const, &[1]),
                            code::make(code::Op::ReturnValue, &[]),
                        ]),
                        0,
                        0,
                    ),
                ],
            ),
            (
                "fn() { let a = 1; a }",
                vec![
                    code::make(code::Op::Closure, &[1, 0]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![
                    object::Object::Int(1),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::Const, &[0]),
                            code::make(code::Op::SetLocal, &[0]),
                            code::make(code::Op::GetLocal, &[0]),
                            code::make(code::Op::ReturnValue, &[]),
                        ]),
                        1,
                        0,
                    ),
                ],
            ),
            (
                "fn(a) {  }(1)",
                vec![
                    code::make(code::Op::Closure, &[0, 0]),
                    code::make(code::Op::Const, &[1]),
                    code::make(code::Op::Call, &[1]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![
                    object::Object::CompiledFunction(
                        concat_instructions(vec![code::make(code::Op::Return, &[])]),
                        1,
                        1,
                    ),
                    object::Object::Int(1),
                ],
            ),
            (
                "fn(a) { a }(1)",
                vec![
                    code::make(code::Op::Closure, &[0, 0]),
                    code::make(code::Op::Const, &[1]),
                    code::make(code::Op::Call, &[1]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::GetLocal, &[0]),
                            code::make(code::Op::ReturnValue, &[]),
                        ]),
                        1,
                        1,
                    ),
                    object::Object::Int(1),
                ],
            ),
            (
                r#"fn(a) { fn(b) {a + b} }"#,
                vec![
                    code::make(code::Op::Closure, &[1, 0]),
                    code::make(code::Op::Pop, &[]),
                ],
                vec![
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::GetFree, &[0]),
                            code::make(code::Op::GetLocal, &[0]),
                            code::make(code::Op::Add, &[]),
                            code::make(code::Op::ReturnValue, &[]),
                        ]),
                        1,
                        1,
                    ),
                    object::Object::CompiledFunction(
                        concat_instructions(vec![
                            code::make(code::Op::GetLocal, &[0]),
                            code::make(code::Op::Closure, &[0, 1]),
                            code::make(code::Op::ReturnValue, &[]),
                        ]),
                        1,
                        1,
                    ),
                ],
            ),
        ];

        tests
            .into_iter()
            .for_each(|test| compile(test.0, test.1, test.2))
    }

    #[test]
    fn compile_builtin_fn_should_work() {
        let tests = [(
            r#"len("1")"#,
            vec![
                code::make(code::Op::GetBuiltin, &[0]),
                code::make(code::Op::Const, &[0]),
                code::make(code::Op::Call, &[1]),
                code::make(code::Op::Pop, &[]),
            ],
            vec![object::Object::String("1".into())],
        )];

        tests
            .into_iter()
            .for_each(|test| compile(test.0, test.1, test.2))
    }

    #[test]
    fn compile_recursive_fn_should_work() {
        let tests = [(
            r#"
            let recursion = fn(x) { recursion(x-1) };
            recursion(1)
            "#,
            vec![
                code::make(code::Op::Closure, &[1, 0]),
                code::make(code::Op::SetGlobal, &[0]),
                code::make(code::Op::GetGlobal, &[0]),
                code::make(code::Op::Const, &[2]),
                code::make(code::Op::Call, &[1]),
                code::make(code::Op::Pop, &[]),
            ],
            vec![
                object::Object::Int(1),
                object::Object::CompiledFunction(
                    concat_instructions(vec![
                        code::make(code::Op::GetCurrentClosure, &[]),
                        code::make(code::Op::GetLocal, &[0]),
                        code::make(code::Op::Const, &[0]),
                        code::make(code::Op::Sub, &[]),
                        code::make(code::Op::Call, &[1]),
                        code::make(code::Op::ReturnValue, &[]),
                    ]),
                    1,
                    1,
                ),
                object::Object::Int(1),
            ],
        )];

        tests
            .into_iter()
            .for_each(|test| compile(test.0, test.1, test.2))
    }
}
