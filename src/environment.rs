use crate::{
    lexer::TokenRepr,
    parser::{AstRepr, Expr},
};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Tuple(Vec<Value>),
    Unit,
}

impl Value {
    pub fn unwrap_number(self) -> f64 {
        match self {
            Self::Number(f) => f,
            _ => panic!("expected number"),
        }
    }

    pub fn unwrap_tuple(self) -> Vec<Value> {
        match self {
            Self::Tuple(a) => a,
            _ => panic!("expected a tuple"),
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

type BuiltinFn = fn(Value) -> Value;

pub struct Environment<'src> {
    pub constants: HashMap<&'src str, Value>,
    pub functions: HashMap<&'src str, BuiltinFn>,
}

impl<'src> Environment<'src> {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn push_constant(&mut self, name: &'src str, val: Value) {
        if let None = self.constants.get(name) {
            self.constants.insert(name, val);
        }
    }

    pub fn push_function(&mut self, name: &'src str, func: BuiltinFn) {
        if let None = self.constants.get(name) {
            self.functions.insert(name, func);
        }
    }

    pub fn interpret_constant(&mut self, name: &'src str, value: Expr<'src>) {
        let value = self.calculate_expr(value);
        self.push_constant(name, value);
    }

    fn calculate_expr(&self, value: Expr<'src>) -> Value {
        match value {
            Expr::String(_) | Expr::Access { .. } => todo!(),
            Expr::Tuple(exprs) => {
                Value::Tuple(exprs.into_iter().map(|e| self.calculate_expr(e)).collect())
            }
            Expr::Unit => Value::Unit,
            Expr::BinOp { left, op, right } => {
                let left = self.calculate_expr(*left).unwrap_number();
                let right = self.calculate_expr(*right).unwrap_number();
                let res = match op {
                    TokenRepr::Plus => left + right,
                    TokenRepr::Minus => left - right,
                    TokenRepr::Div => left / right,
                    TokenRepr::Mult => left * right,
                    _ => panic!("unknown operand"),
                };

                Value::Number(res)
            }
            Expr::Number(num) => num
                .parse::<f64>()
                .expect("Expr::Number is always a valid float")
                .into(),
            Expr::Call { object, params } => {
                let func = match *object {
                    Expr::Identifier(ident) => ident,
                    _ => panic!("unsupported"),
                };

                if let Some(f) = self.functions.get(func) {
                    f(self.calculate_expr(*params))
                } else {
                    panic!("no such function exists!")
                }
            }
            Expr::Identifier(ident) => {
                if let Some(var) = self.constants.get(ident) {
                    var.clone()
                } else {
                    panic!("unknown variable")
                }
            }
            Expr::Unary(var) => {
                let value = self.calculate_expr(*var).unwrap_number();
                Value::Number(-value)
            }
        }
    }

    pub fn interpret_ast<I>(&mut self, ast: I)
    where
        I: Iterator<Item = AstRepr<'src>>,
    {
        for a in ast {
            if let AstRepr::Const { name, value } = a {
                self.interpret_constant(name, value);
            }
        }
    }
}
