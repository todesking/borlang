#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Null,
    Int(i32),
    Intrinsic(String),
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Int(value)
    }
}
