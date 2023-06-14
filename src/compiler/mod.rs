use crate::{ast, code};

use crate::eval::object;

#[derive(Debug)]
pub struct Compiler {
    instructions: code::Instructions,
    consts: Vec<object::Object>,
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: code::Instructions,
    pub consts: Vec<object::Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: code::Instructions(vec![]),
            consts: vec![],
        }
    }

    pub fn compile(&mut self, program: ast::Program) {
        for stmt in program.iter() {
            self.compile_statement(stmt)
        }
    }

    fn compile_statement(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::Expression(expr) => {
                self.compile_expression(expr);
                self.emit(code::Op::Pop, &vec![]);
            }
            _ => todo!(),
        }
    }

    fn compile_expression(&mut self, expr: &ast::Expression) {
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
            ast::Expression::Prefix(prefix, expr) => {
                self.compile_expression(expr);
                match prefix {
                    ast::Prefix::Minus => self.emit(code::Op::Minus, &vec![]),
                    ast::Prefix::Not => self.emit(code::Op::Not, &vec![]),
                };
            }
            ast::Expression::Infix(infix, lhs, rhs) => match infix {
                ast::Infix::Plus => {
                    self.compile_expression(lhs);
                    self.compile_expression(rhs);
                    self.emit(code::Op::Add, &vec![]);
                }
                ast::Infix::Minus => {
                    self.compile_expression(lhs);
                    self.compile_expression(rhs);
                    self.emit(code::Op::Sub, &vec![]);
                }
                ast::Infix::Multiply => {
                    self.compile_expression(lhs);
                    self.compile_expression(rhs);
                    self.emit(code::Op::Mul, &vec![]);
                }
                ast::Infix::Divide => {
                    self.compile_expression(lhs);
                    self.compile_expression(rhs);
                    self.emit(code::Op::Div, &vec![]);
                }
                ast::Infix::Mod => {
                    self.compile_expression(lhs);
                    self.compile_expression(rhs);
                    self.emit(code::Op::Mod, &vec![]);
                }
                ast::Infix::Eq => {
                    self.compile_expression(lhs);
                    self.compile_expression(rhs);
                    self.emit(code::Op::Eq, &vec![]);
                }
                ast::Infix::Ne => {
                    self.compile_expression(lhs);
                    self.compile_expression(rhs);
                    self.emit(code::Op::Ne, &vec![]);
                }
                ast::Infix::Gt => {
                    self.compile_expression(lhs);
                    self.compile_expression(rhs);
                    self.emit(code::Op::Gt, &vec![]);
                }
                ast::Infix::GtEq => {
                    self.compile_expression(lhs);
                    self.compile_expression(rhs);
                    self.emit(code::Op::GtEq, &vec![]);
                }
                ast::Infix::Lt => {
                    self.compile_expression(rhs);
                    self.compile_expression(lhs);
                    self.emit(code::Op::Gt, &vec![]);
                }
                ast::Infix::LtEq => {
                    self.compile_expression(rhs);
                    self.compile_expression(lhs);
                    self.emit(code::Op::GtEq, &vec![]);
                }
                _ => todo!(),
            },
            _ => todo!("unknown expr: {}", expr),
        }
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
        self.add_instruction(ins)
    }

    fn add_instruction(&mut self, ins: code::Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.extend_from_slice(ins.as_slice());
        pos
    }
}

#[cfg(test)]
mod test {
    use crate::{code::concat_instructions, lexer, parser};

    use super::*;

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
        ];

        tests.into_iter().for_each(|test| {
            let program = parser::Parser::new(lexer::Lexer::new(test.0))
                .parse_program()
                .unwrap();
            let mut compiler = Compiler::new();
            compiler.compile(program);
            let bytecode = compiler.bytecode();
            assert_eq!(concat_instructions(test.1), bytecode.instructions);
            assert_eq!(test.2, bytecode.consts);
        })
    }
}
