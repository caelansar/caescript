use std::{borrow::Borrow, cell::RefCell, rc::Rc, str::FromStr};

use crate::{
    code, compiler,
    eval::{builtin, object},
};

const STACK_SIZE: usize = 2048;
const GLOBAL_SIZE: usize = 65535;
const MAX_FRAME: usize = 1024;

mod frame;

// VM is a stack-based Virtual Machine
pub struct VM<'a> {
    consts: &'a [Rc<object::Object>],
    stack: Vec<Rc<object::Object>>,
    sp: usize, // stack pointer
    pub global: Vec<Rc<object::Object>>,
    frames: Vec<frame::Frame>,
    frame_idx: usize,
}

impl<'a> VM<'a> {
    pub fn new(bytecode: compiler::Bytecode<'a>) -> Self {
        // this initialization is reuqired, because the stack is not linear growth
        let null = Rc::new(object::Object::Null);
        let stack = vec![null; GLOBAL_SIZE];
        let global = Vec::with_capacity(GLOBAL_SIZE);

        let ins = bytecode.instructions;
        let main_frame = frame::Frame::new(
            object::Closure {
                func: object::CompiledFunction {
                    instructions: Rc::new(ins.clone()),
                    num_locals: 0,
                    num_params: 0,
                },
                free: Rc::new(RefCell::new(vec![])),
            },
            0,
        );

        let mut frames = Vec::with_capacity(MAX_FRAME);
        frames.insert(0, main_frame);

        Self {
            consts: bytecode.consts,
            stack,
            global,
            sp: 0,
            frames,
            frame_idx: 1,
        }
    }

    pub fn new_with_global(
        byteorder: compiler::Bytecode<'a>,
        global: Vec<Rc<object::Object>>,
    ) -> Self {
        let mut vm = Self::new(byteorder);
        if !global.is_empty() {
            vm.global = global;
        }
        vm
    }

    fn current_frame_mut(&mut self) -> &mut frame::Frame {
        self.frames.get_mut(self.frame_idx - 1).unwrap()
    }

    fn current_frame(&self) -> &frame::Frame {
        self.frames.get(self.frame_idx - 1).unwrap()
    }

    fn push_frame(&mut self, frame: frame::Frame) {
        self.frames.insert(self.frame_idx, frame);
        self.frame_idx += 1;
    }

    fn pop_frame(&mut self) -> Option<frame::Frame> {
        self.frame_idx -= 1;
        self.frames.pop()
    }

    #[allow(dead_code)]
    fn peek(&self) -> Option<Rc<object::Object>> {
        if self.sp == 0 {
            None
        } else {
            self.stack.get(self.sp - 1).cloned()
        }
    }

    fn push(&mut self, obj: Rc<object::Object>) {
        if self.sp >= STACK_SIZE {
            panic!("stack overflow")
        }

        self.stack[self.sp] = obj;
        self.sp += 1;
    }

    fn pop(&mut self) -> Option<Rc<object::Object>> {
        self.sp -= 1;
        self.stack.get(self.sp).cloned()
    }

    pub fn last_popped(&self) -> Option<Rc<object::Object>> {
        self.stack.get(self.sp).cloned()
    }

    pub fn run(&mut self) {
        while self.current_frame().ip < self.current_frame().instructions().len() {
            let mut ip = self.current_frame().ip;
            let op = code::Op::from_repr(self.current_frame().instructions()[ip]).unwrap();

            self.current_frame_mut().ip += 1;
            ip += 1;
            match op {
                code::Op::Const => {
                    let const_idx = code::read_u16(&self.current_frame().instructions()[ip..]);
                    self.current_frame_mut().ip += 2;
                    self.push(self.consts[const_idx].clone());
                }
                code::Op::Add => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(Rc::new(l.as_ref() + r.as_ref()));
                }
                code::Op::Sub => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(Rc::new(l.as_ref() - r.as_ref()));
                }
                code::Op::Mul => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(Rc::new(l.as_ref() * r.as_ref()));
                }
                code::Op::Div => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(Rc::new(l.as_ref() / r.as_ref()));
                }
                code::Op::Mod => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(Rc::new(l.as_ref() % r.as_ref()));
                }
                code::Op::Pop => {
                    self.pop();
                }
                code::Op::False => self.push(Rc::new(object::BOOL_OBJ_FALSE)),
                code::Op::True => self.push(Rc::new(object::BOOL_OBJ_TRUE)),
                code::Op::Eq => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(Rc::new((l == r).into()));
                }
                code::Op::Ne => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    self.push(Rc::new((l != r).into()));
                }
                code::Op::Gt => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l.as_ref(), r.as_ref()) {
                        (object::Object::Int(l), object::Object::Int(r)) => {
                            self.push(Rc::new((l > r).into()));
                        }
                        (object::Object::Float(l), object::Object::Float(r)) => {
                            self.push(Rc::new((l > r).into()));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            "> not supported between '{}' and '{}'",
                            l, r
                        )))),
                    }
                }
                code::Op::GtEq => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l.as_ref(), r.as_ref()) {
                        (object::Object::Int(l), object::Object::Int(r)) => {
                            self.push(Rc::new((l >= r).into()));
                        }
                        (object::Object::Float(l), object::Object::Float(r)) => {
                            self.push(Rc::new((l >= r).into()));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            ">= not supported between '{}' and '{}'",
                            l, r
                        )))),
                    }
                }
                code::Op::Minus => {
                    let operand = self.pop().unwrap();
                    self.push(Rc::new(-operand.as_ref()));
                }
                code::Op::And => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l.as_ref(), r.as_ref()) {
                        (object::Object::Bool(l), object::Object::Bool(r)) => {
                            self.push(Rc::new((*l && *r).into()));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            "&& not supported between '{}' and '{}'",
                            l, r
                        )))),
                    }
                }
                code::Op::Or => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l.as_ref(), r.as_ref()) {
                        (object::Object::Bool(l), object::Object::Bool(r)) => {
                            self.push(Rc::new((*l || *r).into()));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            "|| not supported between '{}' and '{}'",
                            l, r
                        )))),
                    }
                }
                code::Op::BitAnd => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l.as_ref(), r.as_ref()) {
                        (object::Object::Int(_), object::Object::Int(_)) => {
                            self.push(Rc::new(l.as_ref() & r.as_ref()));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            "& not supported between '{}' and '{}'",
                            l, r
                        )))),
                    }
                }
                code::Op::BitOr => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l.as_ref(), r.as_ref()) {
                        (object::Object::Int(_), object::Object::Int(_)) => {
                            self.push(Rc::new(l.as_ref() | r.as_ref()));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            "| not supported between '{}' and '{}'",
                            l, r
                        )))),
                    }
                }
                code::Op::BitXor => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l.as_ref(), r.as_ref()) {
                        (object::Object::Int(_), object::Object::Int(_)) => {
                            self.push(Rc::new(l.as_ref() ^ r.as_ref()));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            "^ not supported between '{}' and '{}'",
                            l, r
                        )))),
                    }
                }
                code::Op::LeftShift => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l.as_ref(), r.as_ref()) {
                        (object::Object::Int(_), object::Object::Int(_)) => {
                            self.push(Rc::new(l.as_ref() << r.as_ref()));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            "<< not supported between '{}' and '{}'",
                            l, r
                        )))),
                    }
                }
                code::Op::RightShift => {
                    let r = self.pop().unwrap();
                    let l = self.pop().unwrap();
                    match (l.as_ref(), r.as_ref()) {
                        (object::Object::Int(_), object::Object::Int(_)) => {
                            self.push(Rc::new(l.as_ref() >> r.as_ref()));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            ">> not supported between '{}' and '{}'",
                            l, r
                        )))),
                    }
                }
                code::Op::Not => {
                    let operand = self.pop().unwrap();
                    self.push(Rc::new(!operand.as_ref()));
                }
                code::Op::JumpNotTruthy => {
                    let pos = code::read_u16(&self.current_frame().instructions()[ip..]);
                    self.current_frame_mut().ip += 2;

                    let cond: bool = self.pop().map(|x| x.as_ref().into()).unwrap();
                    if !cond {
                        self.current_frame_mut().ip = pos
                    }
                }
                code::Op::Jump => {
                    let pos = code::read_u16(&self.current_frame().instructions()[ip..]);
                    self.current_frame_mut().ip = pos;
                }
                code::Op::Null => self.push(Rc::new(object::Object::Null)),
                code::Op::SetGlobal => {
                    let pos = code::read_u16(&self.current_frame().instructions()[ip..]);
                    self.current_frame_mut().ip += 2;

                    let val = self.pop().unwrap();
                    if pos >= self.global.len() {
                        self.global.insert(pos, val)
                    } else {
                        self.global[pos] = val
                    }
                }
                code::Op::SetLocal => {
                    let pos = self.current_frame().instructions()[ip] as usize;
                    self.current_frame_mut().ip += 1;

                    let val = self.pop().unwrap();
                    let frame = self.current_frame_mut();
                    let idx = frame.bp + pos;
                    if idx >= self.stack.len() {
                        panic!("stack overflow")
                    } else {
                        self.stack[idx] = val
                    }
                }
                code::Op::GetGlobal => {
                    let pos = code::read_u16(&self.current_frame().instructions()[ip..]);
                    self.current_frame_mut().ip += 2;

                    let val = self.global.get(pos);
                    self.push(val.unwrap().clone());
                }
                code::Op::GetLocal => {
                    let pos = self.current_frame().instructions()[ip] as usize;
                    self.current_frame_mut().ip += 1;

                    let frame = self.current_frame_mut();
                    let idx = frame.bp + pos;
                    let val = self.stack.get(idx);
                    self.push(val.unwrap().clone());
                }
                code::Op::Array => {
                    let len = code::read_u16(&self.current_frame().instructions()[ip..]);
                    self.current_frame_mut().ip += 2;

                    let mut elems = Vec::with_capacity(len);
                    (self.sp - len..self.sp).for_each(|idx| {
                        elems.push(self.stack[idx].as_ref().clone());
                    });

                    self.sp -= len;
                    self.push(Rc::new(object::Object::Array(elems)));
                }
                code::Op::Hash => {
                    let len = code::read_u16(&self.current_frame().instructions()[ip..]);
                    self.current_frame_mut().ip += 2;

                    let mut elems = Vec::with_capacity(len);
                    (self.sp - len..self.sp).step_by(2).for_each(|idx| {
                        elems.push((
                            self.stack[idx].as_ref().clone(),
                            self.stack[idx + 1].as_ref().clone(),
                        ));
                    });

                    self.sp -= len;
                    self.push(Rc::new(object::Object::Hash(Iterator::collect(
                        IntoIterator::into_iter(elems),
                    ))));
                }
                code::Op::Index => {
                    let idx = self.pop().unwrap();
                    let expr = self.pop().unwrap();

                    match (expr.as_ref(), idx.as_ref()) {
                        (object::Object::Hash(hash), key) => self.push(Rc::new(
                            hash.get(key).cloned().unwrap_or(object::Object::Null),
                        )),
                        (object::Object::Array(array), object::Object::Int(i)) => {
                            self.push(Rc::new(
                                array
                                    .get(*i as usize)
                                    .cloned()
                                    .unwrap_or(object::Object::Null),
                            ))
                        }
                        _ => unreachable!(),
                    }
                }
                code::Op::Call => {
                    let num_args = self.current_frame().instructions()[ip] as usize;
                    self.current_frame_mut().ip += 1;

                    let func = self
                        .stack
                        .get(self.sp - 1 - num_args)
                        .expect("func not found");
                    match func.borrow() {
                        object::Object::Closure(closure) => {
                            assert_eq!(
                                closure.func.num_params, num_args,
                                "wrong number of argument"
                            );
                            let frame = frame::Frame::new(closure.clone(), self.sp - num_args);
                            self.sp = frame.bp + closure.func.num_locals;
                            self.push_frame(frame);
                        }
                        object::Object::Builtin(builtin) => {
                            let args = &self.stack[self.sp - num_args..self.sp];
                            self.sp = self.sp - num_args - 1;
                            self.push(Rc::new(builtin.call(args.to_vec())));
                        }
                        _ => self.push(Rc::new(object::Object::Error(format!(
                            "{} not a function",
                            func
                        )))),
                    }
                }
                code::Op::GetBuiltin => {
                    let builtin_idx = self.current_frame().instructions()[ip] as usize;
                    self.current_frame_mut().ip += 1;

                    let builtin = builtin::BUILTINS
                        .get()
                        .unwrap()
                        .get(builtin_idx)
                        .unwrap()
                        .0
                        .clone();

                    self.push(Rc::new(object::Object::Builtin(
                        builtin::Builtin::from_str(&builtin)
                            .unwrap_or_else(|_| panic!("{builtin} not found")),
                    )));
                }
                code::Op::ReturnValue => {
                    let rv = self.pop().unwrap();

                    let frame = self.pop_frame();
                    self.sp = frame.unwrap().bp - 1;

                    self.push(rv);
                }
                code::Op::Return => {
                    let frame = self.pop_frame();
                    self.sp = frame.unwrap().bp - 1;

                    self.push(Rc::new(object::Object::Null));
                }
                code::Op::Closure => {
                    let idx = code::read_u16(&self.current_frame().instructions()[ip..]);
                    let free_num = self.current_frame().instructions()[ip + 2] as usize;
                    self.current_frame_mut().ip += 3;

                    let mut free = vec![];
                    (0..free_num).for_each(|i| {
                        let idx = self.sp - free_num + i;
                        free.push(self.stack[idx].as_ref().clone())
                    });

                    if let object::Object::CompiledFunction(ins, num_locals, num_params) =
                        self.consts.get(idx).expect("not a function").borrow()
                    {
                        self.push(Rc::new(object::Object::Closure(object::Closure {
                            func: object::CompiledFunction {
                                instructions: ins.clone(),
                                num_locals: *num_locals,
                                num_params: *num_params,
                            },
                            free: Rc::new(RefCell::new(free)),
                        })))
                    }
                }

                code::Op::GetFree => {
                    let free_idx = self.current_frame().instructions()[ip] as usize;
                    let current_frame = self.current_frame_mut();
                    current_frame.ip += 1;

                    let closure = current_frame.closure.clone();

                    let free = RefCell::borrow(&closure.free);
                    let obj = free.get(free_idx).unwrap();
                    self.push(Rc::new(obj.clone()))
                }
                code::Op::SetFree => {
                    let free_idx = self.current_frame().instructions()[ip] as usize;

                    let val = self.pop().unwrap().clone();

                    let current_frame = self.current_frame_mut();
                    current_frame.ip += 1;

                    if let Some(f) = current_frame.closure.free.borrow_mut().get_mut(free_idx) {
                        *f = val.as_ref().clone()
                    }
                }
                code::Op::GetCurrentClosure => {
                    let closure = self.current_frame().closure.clone();
                    self.push(Rc::new(object::Object::Closure(closure)));
                }
                _ => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{compiler::Compiler, lexer, map, parser};

    use super::*;

    fn run(input: &str, expect: Option<object::Object>) {
        let program = parser::Parser::new(lexer::Lexer::new(input))
            .parse_program()
            .unwrap();
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&program).unwrap();

        let expect = expect.map(Rc::new);

        println!("{}", bytecode.instructions);
        bytecode
            .consts
            .iter()
            .for_each(|c| println!("const: {}", c));
        // println!("const {:?}", bytecode.consts);

        let mut vm = VM::new(bytecode);
        vm.run();

        println!("vm global: {:?}", vm.global);

        assert_eq!(
            expect,
            vm.last_popped(),
            "{} expect latest pop to be {:?}, got {:?} instead",
            input,
            expect,
            vm.last_popped()
        )
    }

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
            ("1!=2", Some(object::Object::Bool(true))),
            ("1!=1", Some(object::Object::Bool(false))),
            ("1.0>2.0", Some(object::Object::Bool(false))),
            ("2>=2", Some(object::Object::Bool(true))),
            ("2.0>=2.0", Some(object::Object::Bool(true))),
            ("1<2", Some(object::Object::Bool(true))),
            ("1.0<2.0", Some(object::Object::Bool(true))),
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
            ("1 & 2", Some(object::Object::Int(0))),
            ("1 | 2", Some(object::Object::Int(3))),
            ("1 ^ 2", Some(object::Object::Int(3))),
            ("1 << 2", Some(object::Object::Int(4))),
            ("4 >> 2", Some(object::Object::Int(1))),
            ("null", Some(object::Object::Null)),
            ("10 | 2", Some(object::Object::Int(10))),
            ("10 ^ 2", Some(object::Object::Int(8))),
            ("10 & 2", Some(object::Object::Int(2))),
            ("10 << 2", Some(object::Object::Int(40))),
            ("10 >> 2", Some(object::Object::Int(2))),
            (
                "1 && 1",
                Some(object::Object::Error(
                    "&& not supported between '1' and '1'".into(),
                )),
            ),
            (
                "1 || 1",
                Some(object::Object::Error(
                    "|| not supported between '1' and '1'".into(),
                )),
            ),
            (
                "1 < 2.1",
                Some(object::Object::Error(
                    "> not supported between '2.1' and '1'".into(),
                )),
            ),
            (
                "1 <= 2.1",
                Some(object::Object::Error(
                    ">= not supported between '2.1' and '1'".into(),
                )),
            ),
            (
                "1 > 2.1",
                Some(object::Object::Error(
                    "> not supported between '1' and '2.1'".into(),
                )),
            ),
            (
                "1 >= 2.1",
                Some(object::Object::Error(
                    ">= not supported between '1' and '2.1'".into(),
                )),
            ),
            (
                "1 | 2.1",
                Some(object::Object::Error(
                    "| not supported between '1' and '2.1'".into(),
                )),
            ),
            (
                "1 ^ 2.1",
                Some(object::Object::Error(
                    "^ not supported between '1' and '2.1'".into(),
                )),
            ),
            (
                "1 & 2.1",
                Some(object::Object::Error(
                    "& not supported between '1' and '2.1'".into(),
                )),
            ),
            (
                "1 << 2.1",
                Some(object::Object::Error(
                    "<< not supported between '1' and '2.1'".into(),
                )),
            ),
            (
                "1 >> 2.1",
                Some(object::Object::Error(
                    ">> not supported between '1' and '2.1'".into(),
                )),
            ),
        ];

        tests.into_iter().for_each(|test| run(test.0, test.1))
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
        tests.into_iter().for_each(|test| run(test.0, test.1))
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
            ("let a = 10 + 1; a = 100; a", Some(object::Object::Int(100))),
            ("let a = 10 + 1; a += 1; a", Some(object::Object::Int(12))),
            ("let a = 10 + 2; a /= 2; a", Some(object::Object::Int(6))),
            ("let a = 10 + 2; a *= 1+1; a", Some(object::Object::Int(24))),
            ("let a = 10 + 2; a -= 2; a", Some(object::Object::Int(10))),
            ("let a = 10 + 2; a %= 2; a", Some(object::Object::Int(0))),
            ("let a = 10; a |= 2; a", Some(object::Object::Int(10))),
            ("let a = 10; a ^= 2; a", Some(object::Object::Int(8))),
            ("let a = 10; a &= 2; a", Some(object::Object::Int(2))),
            ("let a = 10; a <<= 2; a", Some(object::Object::Int(40))),
            ("let a = 10; a >>= 2; a", Some(object::Object::Int(2))),
        ];
        tests.into_iter().for_each(|test| run(test.0, test.1))
    }

    #[test]
    fn vm_array_work() {
        let tests = [
            ("[]", Some(object::Object::Array(vec![]))),
            (
                "[1,2,3]",
                Some(object::Object::Array(vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::Int(3),
                ])),
            ),
            ("[1][0]", Some(object::Object::Int(1))),
            ("[1,2,3,4][3]", Some(object::Object::Int(4))),
            ("let arr = [1,2,3,4]; arr[4]", Some(object::Object::Null)),
        ];
        tests.into_iter().for_each(|test| run(test.0, test.1))
    }

    #[test]
    fn vm_hash_work() {
        let tests = [
            ("{}", Some(object::Object::Hash(map! {}))),
            (
                "{1: 2, 3: 4+5}",
                Some(object::Object::Hash(map! {
                    object::Object::Int(1) => object::Object::Int(2),
                    object::Object::Int(3) => object::Object::Int(9),
                })),
            ),
            ("{1: 2, 3: 4+5}[3]", Some(object::Object::Int(9))),
            ("let hash = {1: 2}; hash[1]", Some(object::Object::Int(2))),
        ];
        tests.into_iter().for_each(|test| run(test.0, test.1))
    }

    #[test]
    fn vm_for_loop_should_work() {
        let tests = [
            (
                "let a = 1; for (a<6) {a+=2}; a",
                Some(object::Object::Int(7)),
            ),
            (
                "let i = 10; for (i>0) {i-=1}; i",
                Some(object::Object::Int(0)),
            ),
            (
                "let i = 2; let sum = 0; for (i>0) {sum+=i; i-=1}; sum",
                Some(object::Object::Int(3)),
            ),
            (
                "let i = 3; let sum = 0; for (i>0) {if(i==2) {break;} sum+=i; i-=1}; sum",
                Some(object::Object::Int(3)),
            ),
            (
                "let i = 3; let sum = 0; for (i>0) {if(i==2) {i-=1; continue;} sum+=i; i-=1}; sum",
                Some(object::Object::Int(4)),
            ),
        ];
        tests.into_iter().for_each(|test| run(test.0, test.1));
    }

    #[test]
    fn vm_fn_should_work() {
        let tests = [
            ("fn() {1+2}()", Some(object::Object::Int(3))),
            ("fn add() {1+2}; add()", Some(object::Object::Int(3))),
            ("let add = fn () {1+2}; add()", Some(object::Object::Int(3))),
            ("let empty = fn () {}; empty()", Some(object::Object::Null)),
            ("fn empty() {}; empty()", Some(object::Object::Null)),
            ("fn() {}()", Some(object::Object::Null)),
            (
                "let one = fn() {1}; let two = fn() {one()}; let three = fn() {two()}; three()",
                Some(object::Object::Int(1)),
            ),
            (
                "let r = fn() {1}; let r1 = fn() {r}; r1()()",
                Some(object::Object::Int(1)),
            ),
            (
                r#"
                let no_function = "aa";
                no_function()
                "#,
                Some(object::Object::Error("aa not a function".into())),
            ),
            (
                r#"
                let f = fn() {
                    let a = 1;
                    let b = 2;
                    a + b
                }
                f()
                "#,
                Some(object::Object::Int(3)),
            ),
            (
                r#"
                let global = 10;
                let f1 = fn() {
                    let a = 1;
                    global + a
                };
                let f2 = fn() {
                    let a = 2;
                    global + a
                };
                f1() + f2()
                "#,
                Some(object::Object::Int(23)),
            ),
            (
                r#"
                let f = fn() {
                    let a = 1;
                    a += 2;
                    a
                }
                f()
                "#,
                Some(object::Object::Int(3)),
            ),
            (
                r#"
                let f = fn() {
                    let a = 1;
                    a -= 2;
                    a
                }
                f()
                "#,
                Some(object::Object::Int(-1)),
            ),
            (
                r#"
                let f = fn() {
                    let a = 1;
                    a *= 2;
                    a
                }
                f()
                "#,
                Some(object::Object::Int(2)),
            ),
            (
                r#"
                let f = fn() {
                    let a = 2;
                    a /= 2;
                    a
                }
                f()
                "#,
                Some(object::Object::Int(1)),
            ),
            (
                r#"
                let f = fn() {
                    let a = 2;
                    a %= 2;
                    a
                }
                f()
                "#,
                Some(object::Object::Int(0)),
            ),
            (
                r#"
                let f = fn() {
                    let f1 = fn() {1};
                    f1
                }
                f()()
                "#,
                Some(object::Object::Int(1)),
            ),
            (
                r#"
                let sum = fn(a, b) {
                    a + b
                }
                sum(1,2)
                "#,
                Some(object::Object::Int(3)),
            ),
            (
                r#"
                let global = 10;
                let sum = fn(a, b) {
                    let c = a + b;
                    c + global
                }
                fn outer() {
                    sum(1,2) + sum(3,4) + global
                }
                outer() + global
                "#,
                Some(object::Object::Int(50)),
            ),
            (
                r#"fn(a) { fn(b) {a + b} }(1)(2)"#,
                Some(object::Object::Int(3)),
            ),
            (
                r#"let closure = fn(a) { fn(b) {a + b} }; closure(1)(2)"#,
                Some(object::Object::Int(3)),
            ),
            (
                r#"fn closure(a) { fn(b) {a + b} }; closure(1)(2)"#,
                Some(object::Object::Int(3)),
            ),
            (
                r#"
                fn closure(){
                    let x= 1;
                    fn(){
                        x+=2;
                        x
                    }
                }
                let c = closure();
                c()
                "#,
                Some(object::Object::Int(3)),
            ),
            (
                r#"
                fn closure(){
                    let x= 1;
                    fn(){
                        x+=2;
                        x
                    }
                }
                let c = closure();
                c();
                c()
                "#,
                Some(object::Object::Int(5)),
            ),
            (
                r#"
                fn closure(){
                    let x= 1;
                    fn(){
                        x+=2;
                        x
                    }
                }
                let c = closure();
                c();
                c();
                c()
                "#,
                Some(object::Object::Int(7)),
            ),
        ];
        tests.into_iter().for_each(|test| run(test.0, test.1));
    }

    #[test]
    fn vm_built_fn_should_work() {
        let tests = [
            ("len([])", Some(object::Object::Int(0))),
            (r#"len("1")"#, Some(object::Object::Int(1))),
            (r#"len("123")"#, Some(object::Object::Int(3))),
            (
                r#"push([1,2], 4)"#,
                Some(object::Object::Array(vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::Int(4),
                ])),
            ),
            (r#"first([1,2])"#, Some(object::Object::Int(1))),
            (r#"last([1,2])"#, Some(object::Object::Int(2))),
            (
                r#"rest([1,2,3])"#,
                Some(object::Object::Array(vec![
                    object::Object::Int(2),
                    object::Object::Int(3),
                ])),
            ),
            (r#"rest([1])"#, Some(object::Object::Array(vec![]))),
            (
                r#"push([1,2], first([3,4]))"#,
                Some(object::Object::Array(vec![
                    object::Object::Int(1),
                    object::Object::Int(2),
                    object::Object::Int(3),
                ])),
            ),
            (
                r#"
                let map = fn(arr, f) {
                    let iter = fn(arr, accumlated) {
                       if (len(arr) == 0 ) {
                            accumlated
                       } else {
                            iter(rest(arr), push(accumlated, f(first(arr))))
                       }
                    };
                    iter(arr, [])
                }
                map([1,2], fn(x) {x*2})
                "#,
                Some(object::Object::Array(vec![
                    object::Object::Int(2),
                    object::Object::Int(4),
                ])),
            ),
        ];
        tests.into_iter().for_each(|test| run(test.0, test.1));
    }

    #[test]
    fn vm_recursive_fn_should_work() {
        let tests = [
            (
                r#"
                let recursion = fn(x) { if (x==0) { 0 } else { recursion(x-1) } };
                recursion(1)
             "#,
                Some(object::Object::Int(0)),
            ),
            (
                r#"
                fn wrapper() {
                    fn recursion(x) {
                        if (x==0) { 1 } else { recursion(x-1) }
                    }
                    recursion(10)
                };
                wrapper()
             "#,
                Some(object::Object::Int(1)),
            ),
        ];
        tests.into_iter().for_each(|test| run(test.0, test.1));
    }

    #[test]
    fn test_fix_issue() {
        let tests = [(
            r#"
            fn f(n, n1) {
                for (true) {
                    if(n == 0) {
                        return n1;
                    }
                    n = n - 1;
                    n1 += 1;
              }
            }

            f(10, 0)
            "#,
            Some(object::Object::Int(10)),
            r#"
            fn f(n, n1) {
                for (true) {
                    if(n == 0) {
                        return n1;
                    }
                    n -= 1;
                    n1 += 1;
              }
            }

            f(10, 0)
            "#,
            Some(object::Object::Int(10)),
        )];
        tests.into_iter().for_each(|test| run(test.0, test.1));
    }
}
