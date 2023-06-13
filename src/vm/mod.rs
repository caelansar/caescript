use crate::{code, compiler, eval::object};

const STACK_SIZE: usize = 2048;

pub struct VM {
    consts: Vec<object::Object>,
    instructions: code::Instructions,
    stack: Vec<object::Object>, // stack pointer
    sp: usize,
}

impl VM {
    pub fn new(bytecode: compiler::Bytecode) -> Self {
        let stack = Vec::with_capacity(STACK_SIZE);

        Self {
            consts: bytecode.consts,
            instructions: bytecode.instructions,
            stack,
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
        ];

        tests.into_iter().for_each(|test| {
            let program = parser::Parser::new(lexer::Lexer::new(test.0)).parse_program();
            let mut compiler = Compiler::new();
            compiler.compile(program);

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
}
