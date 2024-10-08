use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast;

use self::{env::Environment, object::*};

pub mod builtin;
pub mod env;
mod macros;
pub mod object;

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Environment>>) -> Self {
        Self { env }
    }

    pub fn eval(&mut self, program: &ast::Program) -> Option<Object> {
        let mut rv = None;

        for stmt in program.iter() {
            match self.eval_statement(stmt) {
                Some(Object::Return(r)) => return Some(*r),
                Some(Object::Error(e)) => return Some(Object::Error(e)),
                obj => rv = obj,
            }
        }

        rv
    }

    fn eval_block_statements(&mut self, program: &ast::BlockStatement) -> Option<Object> {
        let mut rv = None;

        for stmt in program.iter() {
            match self.eval_statement(stmt) {
                Some(Object::Return(r)) => return Some(Object::Return(r)),
                Some(Object::Error(e)) => return Some(Object::Error(e)),
                Some(Object::Break) => return Some(Object::Break),
                Some(Object::Continue) => return Some(Object::Continue),
                obj => rv = obj,
            }
        }

        rv
    }

    fn eval_statement(&mut self, stmt: &ast::Statement) -> Option<Object> {
        match stmt {
            ast::Statement::Let(ident, expr) => self.eval_let(ident, expr),
            ast::Statement::Expression(expr) => self.eval_expression(expr),
            ast::Statement::Return(ret) => self
                .eval_expression(ret)
                .map(|x| Object::Return(Box::new(x))),
            ast::Statement::Function(ast::Ident(ident), params, body) => {
                let val = Object::Function(params.clone(), body.clone(), self.env.clone());
                self.env.borrow_mut().set(ident.clone(), val);
                None
            }
            ast::Statement::Break => Some(Object::Break),
            ast::Statement::Continue => Some(Object::Continue),
        }
    }

    fn eval_let(
        &mut self,
        ast::Ident(ident): &ast::Ident,
        expr: &ast::Expression,
    ) -> Option<Object> {
        let rv = self.eval_expression(expr)?;
        if let Object::Error(r) = rv {
            Some(Object::Error(r))
        } else {
            self.env.borrow_mut().set_self(ident.clone(), rv);
            None
        }
    }

    fn eval_array(&mut self, elements: Vec<ast::Expression>) -> Option<Object> {
        Some(Object::Array(
            elements
                .iter()
                .map(|e| self.eval_expression(e).unwrap_or(Object::Null))
                .collect(),
        ))
    }

    fn eval_hash(&mut self, hash: Vec<(ast::Expression, ast::Expression)>) -> Option<Object> {
        #[allow(clippy::mutable_key_type)]
        let mut h = HashMap::new();
        for (k, v) in hash.iter().map(|(k, v)| {
            (
                self.eval_expression(k).unwrap_or(Object::Null),
                self.eval_expression(v).unwrap_or(Object::Null),
            )
        }) {
            match k {
                Object::Int(_) | Object::String(_) | Object::Bool(_) => h.insert(k, v),
                _ => return Some(Object::Error(format!("invalid hash key {}", k))),
            };
        }

        Some(Object::Hash(h))
    }

    fn eval_index(&mut self, lhs: &ast::Expression, idx: &ast::Expression) -> Option<Object> {
        let obj = self.eval_expression(lhs)?;
        match obj {
            Object::Array(elements) => {
                let idx = match self.eval_expression(idx)? {
                    Object::Int(i) => i,
                    _ => return Some(Object::Error("invalid array index".to_string())),
                };
                elements.get(idx as usize).cloned().or(Some(Object::Null))
            }
            Object::Hash(hash) => {
                let key = match self.eval_expression(idx)? {
                    Object::Int(i) => Object::Int(i),
                    Object::String(s) => Object::String(s),
                    Object::Bool(b) => Object::Bool(b),
                    _ => return Some(Object::Error("invalid hash key".to_string())),
                };
                hash.get(&key).cloned().or(Some(Object::Null))
            }
            _ => Some(Object::Error(format!("unsupported type {} to index", obj))),
        }
    }

    fn eval_assign(
        &mut self,
        op: &ast::Assign,
        ident: &ast::Ident,
        expr: &ast::Expression,
    ) -> Option<Object> {
        let ast::Ident(ident) = ident;

        let curr = match self.env.borrow().get(ident.as_str()) {
            Some(obj) => obj,
            None => {
                return Some(Object::Error(format!(
                    "undefined variable {}",
                    ident.as_str()
                )));
            }
        };
        let exp_val = self.eval_expression(expr)?;

        let val = match op {
            ast::Assign::Assign => exp_val,
            ast::Assign::PlusEq => &curr + &exp_val,
            ast::Assign::MinusEq => &curr - &exp_val,
            ast::Assign::MultiplyEq => &curr * &exp_val,
            ast::Assign::DivideEq => &curr / &exp_val,
            ast::Assign::ModEq => &curr % &exp_val,
            ast::Assign::ShlAssign => &curr << &exp_val,
            ast::Assign::ShrAssign => &curr >> &exp_val,
            ast::Assign::BitAndAssign => &curr & &exp_val,
            ast::Assign::BitOrAssign => &curr | &exp_val,
            ast::Assign::BitXorAssign => &curr ^ &exp_val,
        };

        self.env.borrow_mut().set(ident.clone(), val);
        None
    }

    fn eval_expression(&mut self, expr: &ast::Expression) -> Option<Object> {
        match expr {
            ast::Expression::Literal(lit) => self.eval_literal(lit),
            ast::Expression::Prefix(prefix, rhs) => {
                let obj = self.eval_expression(rhs)?;
                self.eval_prefix_expression(prefix, &obj)
            }
            ast::Expression::Infix(infix, lhs, rhs) => {
                let lhs = self.eval_expression(lhs)?;
                let rhs = self.eval_expression(rhs)?;
                self.eval_infix_expression(infix, &lhs, &rhs)
            }
            ast::Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                let cond = self.eval_expression(condition)?;
                self.eval_if_expression(&cond, consequence, alternative)
            }
            ast::Expression::For {
                condition,
                consequence,
            } => self.eval_for_expression(condition, consequence),
            ast::Expression::Ident(ast::Ident(ident)) => self.eval_identifier(ident),
            ast::Expression::Func {
                name: _,
                params,
                body,
            } => Some(Object::Function(
                params.clone(),
                body.clone(),
                self.env.clone(),
            )),
            ast::Expression::Call { func, args } => {
                let obj = self.eval_expression(func)?;
                self.eval_function_call(obj, args)
            }
            ast::Expression::Assign(op, ident, expr) => self.eval_assign(op, ident, expr),
            ast::Expression::Array(elements) => self.eval_array(elements.clone()),
            ast::Expression::Hash(hash) => self.eval_hash(hash.clone()),
            ast::Expression::Index(lhs, idx) => self.eval_index(lhs, idx),
            ast::Expression::Null => Some(Object::Null),
        }
    }

    fn eval_identifier(&self, ident: &str) -> Option<Object> {
        Some(
            self.env
                .borrow()
                .get(ident)
                .unwrap_or_else(|| Object::Error(format!("undefined variable {}", ident))),
        )
    }

    fn eval_function_call(&mut self, func: Object, args: &[ast::Expression]) -> Option<Object> {
        let args: Vec<_> = args
            .iter()
            .map(|arg| self.eval_expression(arg).unwrap_or(Object::Null))
            .map(Rc::new)
            .collect();
        let (params, ref body, env) = match func {
            Object::Function(params, body, env) => (params, body, env),
            Object::Builtin(f) => return Some(f.call(args)),
            _ => {
                println!("{}", func);
                return Some(Object::Error(format!("{} is not a function", func)));
            }
        };

        if params.len() != args.len() {
            return Some(Object::Error("invalid args number".to_string()));
        }

        let current_env = self.env.clone();

        // set function env as outer scope
        let mut call_env = Environment::enclosed(env);

        // set our args
        params
            .iter()
            .zip(args)
            .for_each(|(ast::Ident(param), arg)| call_env.set_self(param.clone(), (*arg).clone()));

        self.env = Rc::new(RefCell::new(call_env));
        let rv = self.eval_block_statements(body);

        self.env = current_env;

        rv
    }

    fn eval_if_expression(
        &mut self,
        cond: &Object,
        consequence: &ast::BlockStatement,
        alternative: &Option<ast::BlockStatement>,
    ) -> Option<Object> {
        let mut rv = Some(Object::Null);

        if cond.into() {
            rv = self.eval_block_statements(consequence)
        } else if let Some(alternative) = alternative.as_ref() {
            rv = self.eval_block_statements(alternative);
        }

        rv
    }

    fn eval_for_expression(
        &mut self,
        condition: &ast::Expression,
        consequence: &ast::BlockStatement,
    ) -> Option<Object> {
        let mut rv = Some(Object::Null);

        let mut cond = self.eval_expression(condition)?;

        while (&cond).into() {
            rv = self.eval_block_statements(consequence);
            match rv {
                Some(Object::Return(_)) => return rv,
                Some(Object::Continue) => continue,
                Some(Object::Break) => break,
                _ => (),
            }
            cond = self.eval_expression(condition)?;
        }
        rv
    }

    #[inline]
    fn eval_prefix_expression(&self, prefix: &ast::Prefix, obj: &Object) -> Option<Object> {
        match prefix {
            ast::Prefix::Minus => Some(-obj),
            ast::Prefix::Not => Some(!obj),
        }
    }

    fn eval_infix_expression(
        &self,
        infix: &ast::Infix,
        lhs: &Object,
        rhs: &Object,
    ) -> Option<Object> {
        match (lhs, rhs) {
            (Object::Int(l), Object::Int(r)) => match infix {
                ast::Infix::Plus => Some(lhs + rhs),
                ast::Infix::Minus => Some(lhs - rhs),
                ast::Infix::Divide => Some(lhs / rhs),
                ast::Infix::Multiply => Some(lhs * rhs),
                ast::Infix::Mod => Some(lhs % rhs),
                ast::Infix::Eq => Some((l == r).into()),
                ast::Infix::Ne => Some((l != r).into()),
                ast::Infix::Gt => Some((l > r).into()),
                ast::Infix::GtEq => Some((l >= r).into()),
                ast::Infix::Lt => Some((l < r).into()),
                ast::Infix::LtEq => Some((l <= r).into()),
                ast::Infix::And => Some(Object::Int(*r)),
                ast::Infix::Or => Some(Object::Int(*l)),
                ast::Infix::LeftShift => Some(lhs << rhs),
                ast::Infix::RightShift => Some(lhs >> rhs),
                ast::Infix::BitAnd => Some(lhs & rhs),
                ast::Infix::BitOr => Some(lhs | rhs),
                ast::Infix::BitXor => Some(lhs ^ rhs),
            },
            (Object::Float(l), Object::Float(r)) => match infix {
                ast::Infix::Plus => Some(lhs + rhs),
                ast::Infix::Minus => Some(lhs - rhs),
                ast::Infix::Divide => Some(lhs / rhs),
                ast::Infix::Multiply => Some(lhs * rhs),
                ast::Infix::Mod => Some(lhs % rhs),
                ast::Infix::Eq => Some(Object::Bool(l == r)),
                ast::Infix::Ne => Some(Object::Bool(l != r)),
                ast::Infix::Gt => Some(Object::Bool(l > r)),
                ast::Infix::GtEq => Some(Object::Bool(l >= r)),
                ast::Infix::Lt => Some(Object::Bool(l < r)),
                ast::Infix::LtEq => Some(Object::Bool(l <= r)),
                ast::Infix::And => Some(Object::Float(*r)),
                ast::Infix::Or => Some(Object::Float(*l)),
                _ => Some(Object::Error(format!(
                    "unsupported operator {} for {:?}",
                    infix, rhs,
                ))),
            },
            (Object::Bool(l), Object::Bool(r)) => match infix {
                ast::Infix::Eq => Some(Object::Bool(l == r)),
                ast::Infix::Ne => Some(Object::Bool(l != r)),
                ast::Infix::And => Some(Object::Bool(lhs.into() && rhs.into())),
                ast::Infix::Or => Some(Object::Bool(lhs.into() || rhs.into())),
                _ => Some(Object::Error(format!(
                    "unsupported operator {} for {:?}",
                    infix, rhs,
                ))),
            },
            (Object::String(l), Object::String(r)) => match infix {
                ast::Infix::Plus => Some(Object::String(l + r)),
                ast::Infix::Eq => Some(Object::Bool(l == r)),
                ast::Infix::Ne => Some(Object::Bool(l != r)),
                ast::Infix::Gt => Some(Object::Bool(l > r)),
                ast::Infix::GtEq => Some(Object::Bool(l >= r)),
                ast::Infix::Lt => Some(Object::Bool(l < r)),
                ast::Infix::LtEq => Some(Object::Bool(l <= r)),
                ast::Infix::And => Some(Object::String(r.clone())),
                ast::Infix::Or => Some(Object::String(l.clone())),
                _ => Some(Object::Error(format!(
                    "unsupported operator {} for {:?}",
                    infix,
                    Object::String(r.clone()),
                ))),
            },
            (Object::Null, rhs) => match infix {
                ast::Infix::Or => Some(rhs.clone()),
                ast::Infix::And => Some(Object::Null),
                _ => Some(Object::Error(format!(
                    "unsupported operator {} for {:?}",
                    infix,
                    Object::Null,
                ))),
            },
            _ => Some(Object::Error(format!(
                "unknown operator: {} {} {}",
                lhs, infix, rhs
            ))),
        }
    }

    #[inline(always)]
    fn eval_literal(&self, literal: &ast::Literal) -> Option<Object> {
        match literal {
            ast::Literal::Int(i) => Some(Object::Int(*i)),
            ast::Literal::Float(f) => Some(Object::Float(*f)),
            ast::Literal::Bool(b) => Some((*b).into()),
            ast::Literal::String(s) => Some(Object::String(object::CString(Rc::from(s.as_str())))),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer, map, parser::Parser};

    use super::*;

    fn eval(input: &str) -> Option<Object> {
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        assert!(
            parser.errors().is_empty(),
            "parse error: {:?}",
            parser.errors()
        );

        let program = parser.parse_program();
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Environment::new())));
        evaluator.eval(&program.unwrap())
    }

    #[test]
    fn eval_literal_should_work() {
        let tests = vec![
            ("1", Some(Object::Int(1))),
            ("1.1", Some(Object::Float(1.1))),
            ("true", Some(Object::Bool(true))),
            ("false", Some(Object::Bool(false))),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "want literal {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_expr_should_work() {
        let tests = vec![
            ("!true", Some(Object::Bool(false))),
            ("!false", Some(Object::Bool(true))),
            ("!!false", Some(Object::Bool(false))),
            ("!!true", Some(Object::Bool(true))),
            ("-1", Some(Object::Int(-1))),
            ("!-1", Some(Object::Int(0))),
            ("!!-1", Some(Object::Int(-1))),
            ("true == true", Some(Object::Bool(true))),
            ("false == false", Some(Object::Bool(true))),
            ("1 == 1", Some(Object::Bool(true))),
            ("1 < 2", Some(Object::Bool(true))),
            ("1 > 0", Some(Object::Bool(true))),
            ("1 >= 1", Some(Object::Bool(true))),
            ("1 <= 1", Some(Object::Bool(true))),
            ("1 - 1", Some(Object::Int(0))),
            ("1 + 1", Some(Object::Int(2))),
            ("1 * 1", Some(Object::Int(1))),
            ("1 / 1", Some(Object::Int(1))),
            ("1 % 2", Some(Object::Int(1))),
            ("2 + 1 % 2", Some(Object::Int(3))),
            ("1 + 4 == 5", Some(Object::Bool(true))),
            (
                r#""hello "+"world""#,
                Some(Object::String(CString(Rc::from("hello world")))),
            ),
            (r#""hello" == "hello""#, Some(Object::Bool(true))),
            (r#""hello" != "hello""#, Some(Object::Bool(false))),
            (r#""a" < "b""#, Some(Object::Bool(true))),
            (r#""a" > "b""#, Some(Object::Bool(false))),
            (r#""a" >= "a""#, Some(Object::Bool(true))),
            (r#""a" <= "a""#, Some(Object::Bool(true))),
            ("1.0+2.2", Some(Object::Float(3.2))),
            ("1.0 < 2.2", Some(Object::Bool(true))),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "want expr {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_return_should_work() {
        let tests = vec![
            ("return true", Some(Object::Bool(true))),
            ("return false", Some(Object::Bool(false))),
            ("return 1+2", Some(Object::Int(3))),
            (
                r#"return "1""#,
                Some(Object::String(CString(Rc::from("1")))),
            ),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "want return stmt {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_if_should_work() {
        let tests = vec![
            ("if (1>2) {1} else {2}", Some(Object::Int(2))),
            ("if (1>2) {1}", Some(Object::Null)),
            ("if (1<2) {1}", Some(Object::Int(1))),
            (
                r#"if (0<2) {
                       if (0<1) {
                         1
                       }
                       2;
                       }"#,
                Some(Object::Int(2)),
            ),
            (
                r#"if (0<2) {
                       if (0<1) {
                        return 1;
                       }
                       return 2;
                       }"#,
                Some(Object::Int(1)),
            ),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect if expr {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_for_should_work() {
        let tests = vec![
            ("for (true) {return 1;}", Some(Object::Int(1))),
            ("for (1>2) {return 1;}", Some(Object::Null)),
            (
                r#"
                let x = 10;
                for (x>0) {
                    x -= 1;
                    if (x==4) {
                       return 4;
                    }
                }"#,
                Some(Object::Int(4)),
            ),
            (
                r#"
                let sum = 0;
                let i = 5;
                for (i>0) {
                    sum += i;
                    i -= 1;
                }
                sum
                "#,
                Some(Object::Int(15)),
            ),
            (
                r#"
                let sum = 0;
                let i = 5;
                for (i>0) {
                    if (i == 4) {
                        break;
                    }
                    sum += i;
                    i -= 1;
                }
                sum
                "#,
                Some(Object::Int(5)),
            ),
            (
                r#"
                let sum = 0;
                let i = 5;
                for (i>0) {
                    i -= 1;
                    if (i == 4) {
                        continue;
                    }
                    sum += i;
                }
                sum
                "#,
                Some(Object::Int(6)),
            ),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect for expr {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_let_should_work() {
        let tests = vec![
            ("let a = 12;", None),
            ("let a = 12; a", Some(Object::Int(12))),
            ("let a = 1 + 2; a", Some(Object::Int(3))),
            ("let a = 1 * 2; a", Some(Object::Int(2))),
            (
                "let a = 2; let b = a; let c = b * a; c",
                Some(Object::Int(4)),
            ),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect let stmt {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_assign_should_work() {
        let tests = vec![
            ("let a = 12; a=a+100; a", Some(Object::Int(112))),
            ("let a = 12; a+=100; a", Some(Object::Int(112))),
            ("let a = 12; a-=2; a", Some(Object::Int(10))),
            ("let a = 12; a*=2; a", Some(Object::Int(24))),
            ("let a = 12; a/=2; a", Some(Object::Int(6))),
            ("let a = 12; a+=1+2; a", Some(Object::Int(15))),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect assign stmt {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_function_should_work() {
        let tests = vec![
            ("let f = fn(x){x}; f(1)", Some(Object::Int(1))),
            ("fn(x){x}(1);", Some(Object::Int(1))),
            ("let x = 1; fn(x){x}(100)", Some(Object::Int(100))),
            ("let x = 1; fn(x){x}(100); x", Some(Object::Int(1))),
            (
                "let f1 = fn(x,y){x+y}; let f2 = fn(x,y,func) {func(x,y)}; f2(1,2,f1)",
                Some(Object::Int(3)),
            ),
            ("fn add(x,y){x+y}; add(1,2)", Some(Object::Int(3))),
            // x is capture variable, every time we call c should update x value
            (
                r#"let closure = fn(){
                    let x= 1;
                    fn(){x+=1; x}
                };
                let c = closure();
                c();
                c();
                c()
                "#,
                Some(Object::Int(4)),
            ),
            (
                r#"let fib = fn(x){
                    if (x==1 || x==2) {
                        1
                    } else {
                        fib(x-1) + fib(x-2)
                    }
                };
                fib(9);
                "#,
                Some(Object::Int(34)),
            ),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect function call {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_array_should_work() {
        let tests = vec![
            (
                "[1,2,3]",
                Some(Object::Array(vec![
                    Object::Int(1),
                    Object::Int(2),
                    Object::Int(3),
                ])),
            ),
            ("[1+2]", Some(Object::Array(vec![Object::Int(3)]))),
            ("[]", Some(Object::Array(vec![]))),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect array {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_hash_should_work() {
        let tests = vec![
            (
                r#"{1:2, "a":"b", true: false}"#,
                Some(Object::Hash(map! {
                    Object::Int(1) => Object::Int(2),
                    Object::String(CString(Rc::from("a"))) => Object::String(CString(Rc::from("b"))),
                    Object::Bool(true) => Object::Bool(false),
                })),
            ),
            ("{}", Some(Object::Hash(HashMap::new()))),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect hash {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_index_should_work() {
        let tests = vec![
            ("let arr = [1,2,3]; arr[0]", Some(Object::Int(1))),
            ("let arr = [1,2,3]; arr[-1]", Some(Object::Null)),
            ("let arr = [1+2]; arr[0]", Some(Object::Int(3))),
            ("let arr = [1+2]; arr[1]", Some(Object::Null)),
            ("[1,2,3][1]", Some(Object::Int(2))),
            ("let arr = [1,2,3]; let i=2; arr[i]", Some(Object::Int(3))),
            ("let hash = {1:2,true:false}; hash[1]", Some(Object::Int(2))),
            (
                r#"let list = [{"name":"aa"},{"name":"bb"}]; list[1]["name"]"#,
                Some(Object::String(CString(Rc::from("bb")))),
            ),
            (
                r#"{"name":"aa"}["name"]"#,
                Some(Object::String(CString(Rc::from("aa")))),
            ),
            (
                r#"[{"name":"aa"},{"name":"bb"}][1]["name"]"#,
                Some(Object::String(CString(Rc::from("bb")))),
            ),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect index {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_comment_should_work() {
        let tests = vec![
            (
                r#"
            let a = 12;
            // b+=1;
            a
            "#,
                Some(Object::Int(12)),
            ),
            (
                r#"
            let a = 12;
            // a+=1;
            a // comment
            "#,
                Some(Object::Int(12)),
            ),
            (
                r#"
            let a = 12;
            // a+=1;
            // a"#,
                None,
            ),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect stmt {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_error_should_work() {
        let tests = vec![
            (
                "-true",
                Some(Object::Error("unknown operator: -true".into())),
            ),
            (
                r#"-"str""#,
                Some(Object::Error("unknown operator: -\"str\"".into())),
            ),
            (
                "1-true; 10",
                Some(Object::Error("unknown operator: 1 - true".into())),
            ),
            (
                r#""str1"-"str""#,
                Some(Object::Error(
                    "unsupported operator - for String(str)".into(),
                )),
            ),
            (
                r#"[1.2] - [1]"#,
                Some(Object::Error("unknown operator: [1.2] - [1]".into())),
            ),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_logical_should_work() {
        let tests = vec![
            ("true && true", Some(Object::Bool(true))),
            ("true && false", Some(Object::Bool(false))),
            ("false && false", Some(Object::Bool(false))),
            ("true || false", Some(Object::Bool(true))),
            ("1 || 2", Some(Object::Int(1))),
            ("1 && 2", Some(Object::Int(2))),
            ("1 && 2", Some(Object::Int(2))),
            ("1.1 && 2.2", Some(Object::Float(2.2))),
            ("null && null", Some(Object::Null)),
            ("null || 1", Some(Object::Int(1))),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn eval_binary_operation_should_work() {
        let tests = vec![
            ("1 & 1", Some(Object::Int(1))),
            ("1 | 0", Some(Object::Int(1))),
            ("1 ^ 0", Some(Object::Int(1))),
            ("1 << 2", Some(Object::Int(4))),
            ("2 >> 1", Some(Object::Int(1))),
        ];

        tests.iter().for_each(|test| {
            let obj = eval(test.0);
            assert_eq!(
                test.1, obj,
                "expect {} eval to be {:?}, got {:?}",
                test.0, test.1, obj
            );
        })
    }

    #[test]
    fn global_variable_should_not_be_overridden() {
        let test = r#"
        let a = 100;
        fn func() {
        let a = 10;
        }


        func();
        a
        "#;

        let obj = eval(test);
        assert_eq!(Some(Object::Int(100)), obj,);
    }
}
