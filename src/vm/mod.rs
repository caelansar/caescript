use crate::{code, compiler, eval::object};

const STACK_SIZE: usize = 2048;
const GLOBAL_SIZE: usize = 65535;

// VM is a stack-based Virtual Machine
pub struct VM {
    consts: Vec<object::Object>,
    instructions: code::Instructions,
    stack: Vec<object::Object>, // stack pointer
    sp: usize,
    global: Vec<object::Object>,
}

impl VM {
    pub fn new(bytecode: compiler::Bytecode) -> Self {
        let stack = Vec::with_capacity(STACK_SIZE);
        let global = Vec::with_capacity(GLOBAL_SIZE);

        Self {
            consts: bytecode.consts,
            instructions: bytecode.instructions,
            stack,
            global,
            sp: 0,
        }
    }

    fn peek(&self) -> Option<object::Object> {
        if self.sp == 0 {
            None
        } else {
            self.stack.get(self.sp - 1).map(|o| o.clone())
        }
    }

    fn push(&mut self, obj: object::Object) {
        if self.sp >= STACK_SIZE {
            panic!("stack overflow")
        }

        if self.stack.len() > self.sp {
            self.stack[self.sp] = obj
        } else {
            self.stack.push(obj);
        }
        self.sp += 1;
    }

    fn pop(&mut self) -> Option<object::Object> {
        self.sp -= 1;
        self.stack.get(self.sp).map(|x| x.clone())
    }

    fn last_popped(&self) -> Option<object::Object> {
        self.stack.get(self.sp).map(|x| x.clone())
    }

    pub fn run(&mut self) {
        let mut ip = 0; // instruction pointer
        while ip < self.instructions.len() {
            let op = unsafe { std::mem::transmute(self.instructions[ip]) };

            ip += 1;
            match op {
                code::Op::Const => {
                    let const_idx = code::read_u16(&self.instructions[ip..]);
                    ip += 2;
                    self.push(self.consts[const_idx].clone());
                }
                code::Op::Add => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(l + r);
                }
                code::Op::Sub => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(l - r);
                }
                code::Op::Mul => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(l * r);
                }
                code::Op::Div => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(l / r);
                }
                code::Op::Mod => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(l % r);
                }
                code::Op::Pop => {
                    self.pop();
                }
                code::Op::False => self.push(object::Object::Bool(false)),
                code::Op::True => self.push(object::Object::Bool(true)),
                code::Op::Eq => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push((l == r).into());
                }
                code::Op::Ne => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push((l != r).into());
                }
                code::Op::Gt => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l, r) {
                        (object::Object::Int(l), object::Object::Int(r)) => {
                            self.push((l > r).into());
                        }
                        (object::Object::Float(l), object::Object::Float(r)) => {
                            self.push((l > r).into());
                        }
                        _ => todo!(),
                    }
                }
                code::Op::GtEq => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l, r) {
                        (object::Object::Int(l), object::Object::Int(r)) => {
                            self.push((l >= r).into());
                        }
                        (object::Object::Float(l), object::Object::Float(r)) => {
                            self.push((l >= r).into());
                        }
                        _ => todo!(),
                    }
                }
                code::Op::Minus => {
                    let operand = self.pop().unwrap();
                    match operand {
                        object::Object::Float(f) => self.push(object::Object::Float(-f)),
                        object::Object::Int(i) => self.push(object::Object::Int(-i)),
                        _ => todo!(),
                    }
                }
                code::Op::And => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l, r) {
                        (object::Object::Bool(l), object::Object::Bool(r)) => {
                            self.push((l && r).into());
                        }
                        _ => todo!(),
                    }
                }
                code::Op::Or => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l, r) {
                        (object::Object::Bool(l), object::Object::Bool(r)) => {
                            self.push((l || r).into());
                        }
                        _ => todo!(),
                    }
                }
                code::Op::Not => {
                    let operand = self.pop().unwrap();
                    match operand {
                        object::Object::Bool(false) => self.push(object::BOOL_OBJ_TRUE),
                        object::Object::Bool(true) => self.push(object::BOOL_OBJ_FALSE),
                        object::Object::Null => self.push(object::BOOL_OBJ_TRUE),
                        _ => self.push(object::BOOL_OBJ_FALSE),
                    }
                }
                code::Op::JumpNotTruthy => {
                    let pos = code::read_u16(&self.instructions[ip..]);
                    ip += 2;

                    let cond = self.pop().unwrap();
                    if !<object::Object as Into<bool>>::into(cond) {
                        ip = pos
                    }
                }
                code::Op::Jump => {
                    ip = code::read_u16(&self.instructions[ip..]);
                }
                code::Op::Null => self.push(object::Object::Null),
                code::Op::SetGlobal => {
                    let pos = code::read_u16(&self.instructions[ip..]);
                    ip += 2;

                    let val = self.pop().unwrap();
                    self.global.insert(pos, val);
                }
                code::Op::GetGlobal => {
                    let pos = code::read_u16(&self.instructions[ip..]);
                    ip += 2;

                    let val = self.global.get(pos);
                    self.push(val.unwrap().clone());
                }
                _ => todo!(),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{compiler::Compiler, lexer, parser};

    use super::*;

    #[test]
    fn vm_should_work() {
        let tests = [
            ("1", Some(object::Object::Int(1))),
            ("2", Some(object::Object::Int(2))),
            ("1;2", Some(object::Object::Int(2))),
            ("1+2", Some(object::Object::Int(3))),
            ("2-1", Some(object::Object::Int(1))),
            ("2*2", Some(object::Object::Int(4))),
            ("2/2", Some(object::Object::Int(1))),
            ("1.0+2.1", Some(object::Object::Float(3.1))),
            (
                r#""hello "+"world""#,
                Some(object::Object::String("hello world".into())),
            ),
            ("1%2", Some(object::Object::Int(1))),
            ("true", Some(object::Object::Bool(true))),
            ("false", Some(object::Object::Bool(false))),
            ("1>2", Some(object::Object::Bool(false))),
            ("2>=2", Some(object::Object::Bool(true))),
            ("1<2", Some(object::Object::Bool(true))),
            ("2<=2", Some(object::Object::Bool(true))),
            ("1==2", Some(object::Object::Bool(false))),
            ("1==1", Some(object::Object::Bool(true))),
            ("2.1>=2.0", Some(object::Object::Bool(true))),
            ("-1", Some(object::Object::Int(-1))),
            ("!true", Some(object::Object::Bool(false))),
            ("!!true", Some(object::Object::Bool(true))),
            ("-1.1", Some(object::Object::Float(-1.1))),
            ("-(1+2) * -3", Some(object::Object::Int(9))),
            ("false || true", Some(object::Object::Bool(true))),
            ("false && true", Some(object::Object::Bool(false))),
        ];

        tests.into_iter().for_each(|test| {
            let program = parser::Parser::new(lexer::Lexer::new(test.0))
                .parse_program()
                .unwrap();
            let mut compiler = Compiler::new();
            compiler.compile(&program).unwrap();

            let mut vm = VM::new(compiler.bytecode());
            vm.run();

            assert_eq!(
                test.1,
                vm.last_popped(),
                "{} expect latest pop to be {:?}, got {:?} instead",
                test.0,
                test.1,
                vm.last_popped()
            )
        })
    }

    #[test]
    fn vm_conditional_work() {
        let tests = [
            ("if (true) {10}", Some(object::Object::Int(10))),
            ("if (true) {10} else {20}", Some(object::Object::Int(10))),
            ("if (false) {10} else {20}", Some(object::Object::Int(20))),
            ("if (false) {10}", Some(object::Object::Null)),
            ("!(if (false) {10})", Some(object::Object::Bool(true))),
        ];
        tests.into_iter().for_each(|test| {
            let program = parser::Parser::new(lexer::Lexer::new(test.0))
                .parse_program()
                .unwrap();
            let mut compiler = Compiler::new();
            compiler.compile(&program).unwrap();

            let bytecode = compiler.bytecode();

            let mut vm = VM::new(bytecode);
            vm.run();

            assert_eq!(
                test.1,
                vm.last_popped(),
                "{} expect latest pop to be {:?}, got {:?} instead",
                test.0,
                test.1,
                vm.last_popped()
            )
        })
    }

    #[test]
    fn vm_let_work() {
        let tests = [
            ("let a = 10; a", Some(object::Object::Int(10))),
            ("let a = 10; let b = 20; a+b", Some(object::Object::Int(30))),
            (
                "let a = 10; let b = a+a; a+b",
                Some(object::Object::Int(30)),
            ),
        ];
        tests.into_iter().for_each(|test| {
            let program = parser::Parser::new(lexer::Lexer::new(test.0))
                .parse_program()
                .unwrap();
            let mut compiler = Compiler::new();
            compiler.compile(&program).unwrap();

            let bytecode = compiler.bytecode();

            let mut vm = VM::new(bytecode);
            vm.run();

            assert_eq!(
                test.1,
                vm.last_popped(),
                "{} expect latest pop to be {:?}, got {:?} instead",
                test.0,
                test.1,
                vm.last_popped()
            )
        })
    }
}
