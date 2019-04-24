use std::collections::HashMap;
use std::rc::Rc;

use serde_yaml;

use crate::ksy::Attribute;

lalrpop_mod!(pub parser);

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Signed(i64),
    Unsigned(u64),
    Bool(bool),
    Float(f64),
    Bytes(Box<[u8]>),
    Array(Vec<Value>),
    Stream(String),
}

#[derive(Debug)]
pub enum Expr {
    Value(Value),
    Variable(String),
    Field { object: Box<Expr>, field: String },
    Subscript { array: Box<Expr>, subscript: Box<Expr> },
    Enum { base: String, option: String },
    Binop { op: Binop, left: Box<Expr>, right: Box<Expr> },
    Unop { op: Unop, value: Box<Expr> },
    Condition { condition: Box<Expr>, yes: Box<Expr>, no: Box<Expr> },
    Call { function: Box<Expr>, arguments: Vec<Expr> },
    Typecast { object: Box<Expr>, typ: String },
}

#[derive(Clone, Copy, Debug)]
pub enum Unop {
    LogicNot,
}

#[derive(Clone, Copy, Debug)]
pub enum Binop {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicAnd,
    LogicOr,
    Attribute,
}

impl Expr {
    pub fn execute(expr: &Expr, env: &[HashMap<String, Value>]) -> Result<Value, String> {
        match expr {
            Expr::Value(ref v) => Ok(v.clone()),
            Expr::Variable(ref name) => env
                .iter()
                .find(|x| x.contains_key(name))
                .map(|x| x[name].clone())
                .ok_or(format!("unknown field")),
            Expr::Binop { op, ref left, ref right } => {
                let left = Self::execute(&*left, env)?;
                let right = Self::execute(&*right, env)?;

                unimplemented!()
            }
            Expr::Unop { op, ref value } => {
                let value = Self::execute(&*value, env)?;

                unimplemented!()
            }
            Expr::Enum { ref base, ref option } => unimplemented!(),
            Expr::Call { .. } => unimplemented!(),
            Expr::Condition { .. } => unimplemented!(),
            Expr::Field { .. } => unimplemented!(),
            Expr::Subscript { .. } => unimplemented!(),
            Expr::Typecast { .. } => unimplemented!(),
        }
    }

    pub fn from_yaml(value: &serde_yaml::Value) -> Result<Expr, String> {
        match value {
            &serde_yaml::Value::String(ref s) => Self::parse(s),
            &serde_yaml::Value::Bool(b) => Ok(Expr::Value(Value::Bool(b))),
            &serde_yaml::Value::Number(ref b) if b.is_i64() => Ok(Expr::Value(Value::Signed(b.as_i64().unwrap()))),
            v => Err(format!("{:?} is not a value", v)),
        }
    }

    pub fn parse(s: &str) -> Result<Self, String> {
        match parser::ExpressionParser::new().parse(s).map_err(|x| format!("cannot parse '{}': {:?}", s, x)) {
            o @ Ok(_) => o,
            err => {
                eprintln!("{:?}", err);
                err
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser() {
        let ex = Expr::parse("32").unwrap();
        match ex {
            Expr::Value(Value::Signed(32)) => {}
            _ => unreachable!(),
        }
        let ex = Expr::parse("32.0").unwrap();
        match ex {
            Expr::Value(Value::Float(ref f)) if f - 32.0 <= 0.0001 => {}
            _ => unreachable!(),
        }
        let ex = Expr::parse("\"test\"").unwrap();
        match ex {
            Expr::Value(Value::String(ref s)) if s == "test" => {}
            _ => unreachable!(),
        }
        let ex = Expr::parse("a - b - 1").unwrap();
        match ex {
            Expr::Binop { op: Binop::Subtract, .. } => {}
            _ => unreachable!(),
        }
        let ex = Expr::parse("a[1]").unwrap();
        match ex {
            Expr::Subscript { .. } => {}
            _ => unreachable!(),
        }
        let ex = Expr::parse("0").unwrap();
        match ex {
            Expr::Value(Value::Signed(0)) => {}
            _ => unreachable!(),
        }
        let ex = Expr::parse("(num_digits < 6 ? 0 : 1)").unwrap();
        match ex {
            Expr::Condition { .. } => {}
            _ => unreachable!(),
        }
        let ex = Expr::parse("0x1c").unwrap();
        match ex {
            Expr::Value(Value::Signed(0x1c)) => {}
            _ => unreachable!(),
        }
    }
}
