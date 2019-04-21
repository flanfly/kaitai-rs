use std::collections::HashMap;
use std::rc::Rc;

use serde_yaml;

use crate::ksy::Attribute;

lalrpop_mod!(pub parser);

#[derive(Clone, Debug)]
enum Value {
    Object(Rc<Attribute>),
    String(String),
    Signed(i64),
    Unsigned(i64),
    Bool(bool),
    Float(f64),
    Bytes(Box<[u8]>),
    Array(Vec<Value>),
    Stream(String),
}

#[derive(Debug)]
enum Expr {
    Value(Value),
    Field(String),
    Enum { base: String, option: String },
    Binop { op: Binop, left: Box<Expr>, right: Box<Expr> },
    Unop { op: Unop, value: Box<Expr> },
    Condition { condition: Box<Expr>, yes: Box<Expr>, no: Box<Expr> },
    Call { function: Box<Expr>, arguments: Vec<Expr> },
}

#[derive(Clone, Copy, Debug)]
enum Unop {
    LogicNot,
}

#[derive(Clone, Copy, Debug)]
enum Binop {
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
            Expr::Field(ref name) => env
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
        }
    }

    pub fn from_yaml(value: &serde_yaml::Value) -> Value {
        unimplemented!()
    }

    pub fn parse(s: &str) -> Result<Self, String> {
        let _ = parser::SignedTerm::new().parse(s).unwrap();
        unimplemented!()
    }
}
