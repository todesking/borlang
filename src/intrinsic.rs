use crate::{module::ModuleLoader, value::ObjectKey, EvalError, EvalResult, RuntimeContext, Value};

pub fn register_intrinsics<L: ModuleLoader>(rt: &mut RuntimeContext<L>) {
    macro_rules! intrinsic {
        ($name:ident, $args:ident, $body:expr) => {{
            fn $name($args: &[Value]) -> EvalResult {
                $body
            }
            let name = stringify!($name).to_owned();
            rt.register_intrinsic(name.clone(), $name);
            Value::intrinsic(name)
        }};
    }

    intrinsic!(array_iterator_next, args, {
        EvalError::check_argument_len(1, args.len())?;
        args[0].use_object_mut(|iter| {
            let Some(arr) = iter.get(&ObjectKey::new_str_from_str("data")) else {
                return Err(EvalError::type_error("Iterator.data", args[0].clone()));
            };
            let Some(index) = iter.get(&ObjectKey::new_str_from_str("cur")) else {
                return Err(EvalError::type_error("Iterator.index", args[0].clone()));
            };
            let index = index.try_into()?;
            let next_value = arr.use_array(|arr| {
                if arr.len() <= index {
                    return Ok(Value::array(vec![]));
                }
                Ok(Value::array(vec![arr[index].clone()]))
            })?;
            iter.insert(
                ObjectKey::new_str_from_str("cur"),
                Value::try_int(index + 1)?,
            );
            Ok(next_value)
        })
    });
    intrinsic!(array_len, args, {
        EvalError::check_argument_len(1, args.len())?;
        args[0].use_array(|arr| Ok(Value::int(arr.len() as i32)))
    });
    intrinsic!(array_push, args, {
        EvalError::check_argument_len(2, args.len())?;
        args[0].use_array_mut(|arr| {
            arr.push(args[1].clone());
            Ok(Value::null())
        })
    });

    macro_rules! intrinsic_binop {
        ($name:ident, $t:ty, |$lhs:ident, $rhs:ident| $body:expr) => {{
            intrinsic!($name, args, {
                EvalError::check_argument_len(2, args.len())?;
                let [$lhs, $rhs] = args else {
                    unreachable!();
                };
                let $lhs: $t = $lhs.try_into()?;
                let $rhs: $t = $rhs.try_into()?;
                Ok($body.into())
            })
        }};
    }

    intrinsic_binop!(int_add, i32, |lhs, rhs| lhs + rhs);
    intrinsic_binop!(int_sub, i32, |lhs, rhs| lhs - rhs);
    intrinsic_binop!(int_mul, i32, |lhs, rhs| lhs * rhs);
    intrinsic_binop!(int_mod, i32, |lhs, rhs| lhs % rhs);
    intrinsic_binop!(int_gt, i32, |lhs, rhs| lhs > rhs);
    intrinsic_binop!(int_ge, i32, |lhs, rhs| lhs >= rhs);
    intrinsic_binop!(int_lt, i32, |lhs, rhs| lhs < rhs);
    intrinsic_binop!(int_le, i32, |lhs, rhs| lhs <= rhs);
    intrinsic!(int_negate, args, {
        EvalError::check_argument_len(1, args.len())?;
        let v: i32 = (&args[0]).try_into()?;
        Ok((-v).into())
    });

    macro_rules! intrinsic_binop_any {
        ($name:ident, |$lhs:ident, $rhs:ident| $body:expr) => {
            intrinsic!($name, args, {
                EvalError::check_argument_len(2, args.len())?;
                let [$lhs, $rhs] = args else {
                    unreachable!();
                };
                Ok($body.into())
            })
        };
    }
    intrinsic_binop_any!(op_eq, |lhs, rhs| lhs == rhs);
    intrinsic_binop_any!(op_ne, |lhs, rhs| lhs != rhs);

    intrinsic!(throw, args, {
        EvalError::check_argument_len(1, args.len())?;
        Err(EvalError::Exception {
            data: args[0].clone(),
        })
    });

    intrinsic!(print_string, args, {
        EvalError::check_argument_len(1, args.len())?;
        let s = <&str>::try_from(&args[0])?;
        print!("{}", s);
        Ok(Value::null())
    });

    intrinsic!(to_string, args, {
        EvalError::check_argument_len(1, args.len())?;
        match &args[0] {
            s @ Value::Str(_) => Ok(s.clone()),
            _ => Ok(Value::Str(std::rc::Rc::new(format!("{}", args[0])))),
        }
    });

    intrinsic!(intrinsic, args, {
        EvalError::check_argument_len(1, args.len())?;
        Ok(Value::intrinsic(String::try_from(&args[0])?))
    });
}
