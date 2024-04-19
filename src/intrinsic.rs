use crate::{
    evaluator::ModuleLoader, value::ObjectKey, EvalError, EvalResult, RuntimeContext, Value,
};

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
                return Err(EvalError::trait_protocol("Array iterator value(data)"));
            };
            let Some(index) = iter.get(&ObjectKey::new_str_from_str("cur")) else {
                return Err(EvalError::trait_protocol("Array iterator value(cur)"));
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

    intrinsic_binop!(op_plus, i32, |lhs, rhs| lhs + rhs);
    intrinsic_binop!(op_minus, i32, |lhs, rhs| lhs - rhs);
    intrinsic_binop!(op_mul, i32, |lhs, rhs| lhs * rhs);
    intrinsic_binop!(op_mod, i32, |lhs, rhs| lhs % rhs);

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

    intrinsic!(op_negate, args, {
        EvalError::check_argument_len(1, args.len())?;
        let v: i32 = (&args[0]).try_into()?;
        Ok((-v).into())
    });
}
