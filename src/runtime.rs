use crate::ast::ArrayItem;
use crate::ast::Block;
use crate::ast::ExprStr;
use crate::ast::Ident;
use crate::ast::LetPattern;
use crate::ast::ObjItem;
use crate::ast::PropSpec;
use crate::ast::TopTerm;
use crate::intrinsic::register_intrinsics;
use crate::module::FsModuleLoader;
use crate::module::Module;
use crate::module::ModuleLoader;
use crate::module::ModulePath;
use crate::value::LocalEnv;
use crate::value::LocalEnvRef;
use crate::value::ObjectKey;
use crate::value::ObjectValue;
use crate::value::ObjectValueRef;
use crate::value::RefValue;
use crate::EvalError;
use crate::Expr;
use crate::Program;
use crate::Value;
use gc::Gc;
use gc::GcCell;

use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

pub type EvalResult<T = Value> = Result<T, EvalError>;
pub type IntrinsicFn = dyn Fn(&[Value]) -> EvalResult;

enum Cont {
    Finished(Value),
    Next {
        current_module: Gc<Module>,
        local_env: Option<LocalEnvRef>,
        expr: Expr,
    },
}
impl From<Value> for Cont {
    fn from(value: Value) -> Self {
        Self::Finished(value)
    }
}

pub struct Prototype {
    null: ObjectValueRef,
    int: ObjectValueRef,
    bool: ObjectValueRef,
    string: ObjectValueRef,
    symbol: ObjectValueRef,
    function: ObjectValueRef,
    array: ObjectValueRef,
}

macro_rules! define_symbols {
    ($($name:ident),+$(,)?) => {
        define_symbols!(@type Symbols $($name),+);
        define_symbols!(@impl Symbols $($name),+);
    };
    (@type $ty:ident $($name:ident),+) => {
        pub struct $ty {
            $($name: ObjectKey),+
        }
    };
    (@impl $ty:ident $($name:ident),+) => {
        impl $ty {
            fn register(module: &Module) -> EvalResult<Self> {
                Ok(Self {
                    $(
                        $name: module.bind_symbol(Rc::new(stringify!($name).into()), true)?.to_object_key()?
                    ),+
                })
            }
        }
    };
    (@impl_members $module:ident $name:ident, $($rest:ident),* $(,)?) => {
        $name: $module.define_symbol(stringify!($name))?.to_object_key()?,
        define_symbols!(@impl_members $module $($rest),*)
    };
}

define_symbols!(
    op_plus,
    op_minus,
    op_mul,
    op_mod,
    op_gt,
    op_ge,
    op_lt,
    op_le,
    op_negate,
    op_not,
    op_range,
    op_range_eq,
    iterable_iterator,
    iterator_next
);
impl Symbols {
    fn get_by_bin_op(&self, op: &str) -> Option<&ObjectKey> {
        match op {
            "+" => Some(&self.op_plus),
            "-" => Some(&self.op_minus),
            "*" => Some(&self.op_mul),
            "%" => Some(&self.op_mod),
            ">" => Some(&self.op_gt),
            ">=" => Some(&self.op_ge),
            "<" => Some(&self.op_lt),
            "<=" => Some(&self.op_le),
            ".." => Some(&self.op_range),
            "..=" => Some(&self.op_range_eq),
            _ => None,
        }
    }
}

pub struct RuntimeContext<Loader: ModuleLoader> {
    module_loader: Loader,
    modules: HashMap<ModulePath, Gc<Module>>,
    intrinsics: HashMap<String, fn(&[Value]) -> EvalResult>,
    allow_rebind_global: bool,
    proto: Prototype,
    symbols: Symbols,
}

impl RuntimeContext<FsModuleLoader> {
    pub fn with_paths<P: Into<PathBuf>>(paths: Vec<P>) -> EvalResult<Self> {
        Self::load(FsModuleLoader::new(
            paths.into_iter().map(|x| x.into()).collect(),
        ))
    }
}

impl<L: ModuleLoader> RuntimeContext<L> {
    fn bootstrap(module_loader: L) -> Self {
        let internal = Module::new(ModulePath::new("_internal"));
        internal
            .bind("intrinsic", Value::intrinsic("intrinsic"), true, false)
            .unwrap();
        internal
            .bind("true", Value::bool(true), true, false)
            .unwrap();
        internal
            .bind("false", Value::bool(false), true, false)
            .unwrap();

        let symbols = Symbols::register(&internal).unwrap();

        let dummy_proto = Gc::new(GcCell::new(ObjectValue::new()));
        let mut rt = Self {
            module_loader,
            modules: Default::default(),
            intrinsics: Default::default(),
            allow_rebind_global: false,
            symbols,
            proto: Prototype {
                array: dummy_proto.clone(),
                bool: dummy_proto.clone(),
                function: dummy_proto.clone(),
                symbol: dummy_proto.clone(),
                string: dummy_proto.clone(),
                null: dummy_proto.clone(),
                int: dummy_proto.clone(),
            },
        };
        rt.modules.insert(internal.path.clone(), Gc::new(internal));
        register_intrinsics(&mut rt);
        rt
    }

    pub fn load(module_loader: L) -> EvalResult<Self> {
        let mut rt = Self::bootstrap(module_loader);

        rt.load_module(&ModulePath::new("std/prelude"))?;

        let std = rt.load_module(&ModulePath::new("std"))?;
        fn get_proto(m: &Module, name: &str) -> EvalResult<ObjectValueRef> {
            m.lookup_or_err(name)?
                .get_object_prop_str("prototype")?
                .try_into()
        }
        let proto = Prototype {
            null: get_proto(&std, "Null")?,
            int: get_proto(&std, "Int")?,
            bool: get_proto(&std, "Bool")?,
            string: get_proto(&std, "String")?,
            symbol: get_proto(&std, "Symbol")?,
            function: get_proto(&std, "Function")?,
            array: get_proto(&std, "Array")?,
        };

        let rt = RuntimeContext { proto, ..rt };

        Ok(rt)
    }

    pub fn register_intrinsic<S: Into<String>>(
        &mut self,
        name: S,
        f: fn(&[Value]) -> EvalResult,
    ) -> Value {
        let name = name.into();
        let prev = self.intrinsics.insert(name.clone(), f);
        if prev.is_some() {
            panic!("Intrinsic duplicated: {name}");
        }
        Value::intrinsic(name)
    }

    pub fn allow_rebind_global(&mut self, v: bool) {
        self.allow_rebind_global = v;
    }

    pub fn load_module(&mut self, path: &ModulePath) -> EvalResult<Gc<Module>> {
        if let Some(m) = self.modules.get(path).cloned() {
            return Ok(m);
        }
        let program = self
            .module_loader
            .load(path)
            .map_err(EvalError::LoadError)?;
        let module = self.new_module(path.clone())?;
        self.eval_program_in_module(&program, &module)?;
        Ok(module)
    }

    pub fn new_module(&mut self, path: ModulePath) -> EvalResult<Gc<Module>> {
        if self.modules.contains_key(&path) {
            return Err(EvalError::NameDefined(path.to_string()));
        }
        let m = Gc::new(Module::new(path.clone()));
        self.modules.insert(path, m.clone());
        self.import_prelude(&m)?;
        Ok(m)
    }
    fn import_prelude(&mut self, module: &Gc<Module>) -> EvalResult<()> {
        if matches!(module.path.as_ref(), "std/prelude" | "_internal") {
            return Ok(());
        }
        let prelude = self.load_module(&ModulePath::new("std/prelude"))?;
        prelude
            .pub_object()
            .use_object(|o| {
                for (name, value) in o.iter() {
                    if let ObjectKey::Str(name) = name {
                        module.bind(&**name, value.clone(), false, false)?;
                    }
                }
                Ok(())
            })
            .unwrap();
        Ok(())
    }

    pub fn eval_program_in_module(
        &mut self,
        program: &Program,
        module: &Gc<Module>,
    ) -> EvalResult<()> {
        for t in &program.top_terms {
            self.eval_top_term_in_module(t, module)?;
        }
        Ok(())
    }

    pub fn eval_top_term_in_module(
        &mut self,
        top_term: &TopTerm,
        module: &Gc<Module>,
    ) -> EvalResult<()> {
        match top_term {
            TopTerm::Let { is_pub, name, expr } => {
                let value = self.eval_expr_in_module(expr, module)?;
                Self::let_bind(name, value, |n, v| {
                    module.bind(&*n.0, v, is_pub.is_some(), self.allow_rebind_global)
                })
            }
            TopTerm::Sym { is_pub, name } => {
                let value = Value::sym(format!("{}:{}", module.path, name.0));
                module.bind(&*name.0, value, is_pub.is_some(), false)
            }
        }
    }

    pub fn eval_expr_in_module(&mut self, expr: &Expr, module: &Gc<Module>) -> EvalResult {
        self.eval_expr(expr, &None, module)
    }

    fn eval_expr(
        &mut self,
        expr: &Expr,
        local_env: &Option<LocalEnvRef>,
        current_module: &Gc<Module>,
    ) -> EvalResult {
        let cont = self.eval_expr_cont(expr, local_env, current_module)?;
        self.eval_cont(cont)
    }

    fn eval_cont(&mut self, mut cont: Cont) -> EvalResult {
        loop {
            match cont {
                Cont::Finished(v) => {
                    return Ok(v);
                }
                Cont::Next {
                    current_module,
                    local_env,
                    expr,
                } => {
                    cont = self.eval_expr_cont(&expr, &local_env, &current_module)?;
                }
            }
        }
    }

    fn eval_expr_cont(
        &mut self,
        expr: &Expr,
        local_env: &Option<LocalEnvRef>,
        current_module: &Gc<Module>,
    ) -> EvalResult<Cont> {
        match expr {
            Expr::Int(v) => Ok(Value::Int(*v).into()),
            Expr::Str { content } => Ok(Value::Str(content.clone()).into()),
            Expr::Object(items) => {
                let mut buf = ObjectValue::new();
                for item in items {
                    match item {
                        ObjItem::Const {
                            key,
                            expr: Some(expr),
                        } => {
                            let value = self.eval_expr(expr, local_env, current_module)?;
                            buf.insert(key.to_object_key(), value);
                        }
                        ObjItem::Const { key, expr: None } => {
                            let value = self.get_var(local_env, current_module, key)?;
                            buf.insert(key.to_object_key(), value);
                        }
                        ObjItem::Dyn { key, expr } => {
                            let key = self.eval_expr(key, local_env, current_module)?;
                            let value = self.eval_expr(expr, local_env, current_module)?;
                            buf.insert(key.to_object_key()?, value);
                        }
                        ObjItem::Spread(expr) => {
                            let value = self.eval_expr(expr, local_env, current_module)?;
                            value.use_object(|obj| {
                                for (name, value) in obj.iter() {
                                    buf.insert(name.clone(), value.clone());
                                }
                                Ok(())
                            })?;
                        }
                    }
                }
                Ok(Value::object(buf).into())
            }
            Expr::Array(items) => {
                let mut buf = Vec::new();
                for ArrayItem { spread, expr } in items {
                    let value = self.eval_expr(expr, local_env, current_module)?;
                    if spread.is_some() {
                        value.use_array(|arr| {
                            buf.extend(arr.iter().cloned());
                            Ok(())
                        })?;
                    } else {
                        buf.push(value);
                    }
                }
                Ok(Value::array(buf).into())
            }
            Expr::Var(name) => Ok(self.get_var(local_env, current_module, name)?.into()),
            Expr::Paren { expr } => Ok(self.eval_expr(expr, local_env, current_module)?.into()),
            Expr::Do(block) => self.eval_block_cont(block, local_env, current_module),
            Expr::Binop { lhs, op, rhs } => {
                let lhs = self.eval_expr(lhs, local_env, current_module)?;
                let rhs = self.eval_expr(rhs, local_env, current_module)?;
                if &*op.0 == "==" {
                    return Ok(Value::bool(lhs == rhs).into());
                }
                if &*op.0 == "!=" {
                    return Ok(Value::bool(lhs != rhs).into());
                }
                let Some(sym) = self.symbols.get_by_bin_op(&op.0) else {
                    panic!("Unknown binary operator: `{}`", &op)
                };
                let f = self.as_object(&lhs, |o| o.get_or_err(sym))?;
                self.eval_app_cont(&f, &[lhs, rhs])
            }
            Expr::Negate { expr } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                let f = self.as_object(&value, |v| v.get_or_err(&self.symbols.op_negate))?;
                self.eval_app_cont(&f, &[value])
            }
            Expr::Not { expr } => {
                let v = self.eval_expr(expr, local_env, current_module)?;
                let f = self.as_object(&v, |v| v.get_or_err(&self.symbols.op_not))?;
                self.eval_app_cont(&f, &[v])
            }
            Expr::App { expr: f, args } => match &**f {
                Expr::Prop { expr, prop } => {
                    let this = self.eval_expr(expr, local_env, current_module)?;
                    let key = self.prop_spec_to_object_key(prop, local_env, current_module);
                    let f = self.get_prop(&this, &key?)?;
                    let mut args = self.eval_args(args, local_env, current_module)?;
                    args.insert(0, this);
                    self.eval_app_cont(&f, &args)
                }
                _ => {
                    let f = self.eval_expr(f, local_env, current_module)?;
                    let args = self.eval_args(args, local_env, current_module)?;
                    self.eval_app_cont(&f, &args)
                }
            },
            Expr::Let { name, expr } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                Self::let_bind(name, value, |n, v| {
                    self.bind_var(local_env, current_module, n, v)
                })?;
                Ok(Value::null().into())
            }
            Expr::Reassign { lhs, rhs } => {
                match &**lhs {
                    Expr::Var(name) => {
                        let value = self.eval_expr(rhs, local_env, current_module)?;
                        self.reassign_var(local_env, current_module, name, value)?;
                    }
                    Expr::Index { expr, index } => {
                        let arr = self.eval_expr(expr, local_env, current_module)?;
                        let index =
                            (&self.eval_expr(index, local_env, current_module)?).try_into()?;
                        let value = self.eval_expr(rhs, local_env, current_module)?;
                        arr.use_array_mut(|arr| {
                            if arr.len() <= index {
                                return Err(EvalError::IndexOutOfBound {
                                    len: arr.len(),
                                    index,
                                });
                            }
                            arr[index] = value;
                            Ok(Value::null())
                        })?;
                    }
                    Expr::Prop { expr, prop } => {
                        let obj = self.eval_expr(expr, local_env, current_module)?;
                        let key = match prop {
                            PropSpec::Dyn(prop) => self
                                .eval_expr(prop, local_env, current_module)?
                                .try_into()?,
                            PropSpec::Const(name) => ObjectKey::new_str_from_str(name),
                        };
                        let value = self.eval_expr(rhs, local_env, current_module)?;
                        obj.use_object_mut(|obj| {
                            obj.insert(key, value);
                            Ok(Value::null())
                        })?;
                    }
                    _ => return Err(EvalError::AssignNotSupported),
                }
                Ok(Value::null().into())
            }
            Expr::If { cond, th, el } => {
                let cond = self.eval_expr(cond, local_env, current_module)?;
                let cond = (&cond).try_into()?;
                let value = if cond {
                    self.eval_block_cont(th, local_env, current_module)?
                } else {
                    el.as_ref()
                        .map(|el| self.eval_block_cont(el, local_env, current_module))
                        .unwrap_or_else(|| Ok(Value::null().into()))?
                };
                Ok(value)
            }
            Expr::For { name, target, body } => {
                let target = self.eval_expr(target, local_env, current_module)?;
                let iter_new = self.get_prop(&target, &self.symbols.iterable_iterator)?;
                let iter = self.eval_app(&iter_new, &[target])?;
                let args = [iter];
                loop {
                    let next = self.get_prop(&args[0], &self.symbols.iterator_next)?;
                    let next_value = self.eval_app(&next, &args)?;
                    let next_value = next_value.use_array(|a| match a {
                        [] => Ok(None),
                        [v] => Ok(Some(v.clone())),
                        _ => Err(EvalError::type_error("[] | [T]", next_value.clone())),
                    })?;
                    let Some(next_value) = next_value else {
                        break;
                    };
                    let env = LocalEnv::extend(local_env.clone());
                    LocalEnv::bind(&env, name.clone(), next_value.clone())?;
                    self.eval_block_cont(body, &Some(env), current_module)?;
                }
                Ok(Value::null().into())
            }
            Expr::Fun { params, expr } => Ok(Value::fun(
                params.clone(),
                expr.clone(),
                local_env.clone(),
                current_module.clone(),
            )
            .into()),
            Expr::Prop { expr, prop } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                let key = self.prop_spec_to_object_key(prop, local_env, current_module)?;
                self.get_prop(&value, &key).map(Cont::Finished)
            }
            Expr::PropOpt { expr, prop } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                if value.is_null() {
                    return Ok(Value::null().into());
                }
                let key = self.prop_spec_to_object_key(prop, local_env, current_module)?;
                self.get_prop(&value, &key).map(Cont::Finished)
            }
            Expr::Index { expr, index } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                let index = self.eval_expr(index, local_env, current_module)?;
                self.get_index(&value, &index).map(Cont::Finished)
            }
            Expr::Import(ExprStr { content }) => {
                let module = self.load_module(&ModulePath::new((**content).to_owned()))?;
                Ok(module.pub_object().clone().into())
            }
            Expr::Catch {
                expr,
                name,
                catch_expr,
            } => match self.eval_expr(expr, local_env, current_module) {
                Err(EvalError::Exception { data }) => {
                    let local_env = LocalEnv::extend(local_env.clone());
                    LocalEnv::bind(&local_env, name.clone(), data.clone())?;
                    self.eval_expr_cont(catch_expr, &Some(local_env), current_module)
                }
                Err(err) => Err(err),
                Ok(v) => Ok(v.into()),
            },
        }
    }

    fn eval_block_cont(
        &mut self,
        block: &Block,
        local_env: &Option<LocalEnvRef>,
        current_module: &Gc<Module>,
    ) -> EvalResult<Cont> {
        let Block { terms, expr } = block;
        let local_env = Some(LocalEnv::extend(local_env.clone()));
        // TODO: support tailrec for term-only block
        for e in terms {
            self.eval_expr(e, &local_env, current_module)?;
        }
        if let Some(e) = expr {
            self.eval_expr_cont(e, &local_env, current_module)
        } else {
            Ok(Cont::Finished(Value::null()))
        }
    }

    fn let_bind<F: FnMut(Ident, Value) -> EvalResult<()>>(
        name: &LetPattern,
        value: Value,
        mut bind: F,
    ) -> EvalResult<()> {
        match name {
            LetPattern::Name(name) => {
                bind(name.clone(), value)?;
            }
            LetPattern::Obj { name, rest } => {
                value.use_object(|o| {
                    for n in name {
                        if !o.contains_key(&ObjectKey::new_str_from_str(&*n.0)) {
                            return Err(EvalError::property_not_found(
                                ObjectKey::new_str_from_str(&*n.0),
                            ));
                        }
                    }
                    for n in name {
                        bind(
                            n.clone(),
                            o.get(&ObjectKey::new_str_from_str(&*n.0)).unwrap().clone(),
                        )?;
                    }
                    if let Some(rest) = rest {
                        let mut rest_obj = ObjectValue::new();
                        for (k, v) in o.iter() {
                            match k {
                                ObjectKey::Str(k) => {
                                    if name.iter().any(|n| &n.0 == k) {
                                        continue;
                                    }
                                }
                                ObjectKey::Sym(_) => {}
                            }
                            rest_obj.insert(k.clone(), v.clone())
                        }
                        bind(rest.clone(), rest_obj.into())?;
                    }
                    Ok(())
                })?;
            }
            LetPattern::Arr { name, rest } => {
                value.use_array(|arr| {
                    if rest.is_some() && arr.len() < name.len() {
                        return Err(EvalError::array_length(name.len(), arr.len()));
                    }
                    if rest.is_none() && arr.len() != name.len() {
                        return Err(EvalError::array_length(name.len(), arr.len()));
                    }
                    for (n, v) in name.iter().zip(arr) {
                        bind(n.clone(), v.clone())?;
                    }
                    if let Some(rest) = rest {
                        let rest_value = arr[name.len()..].to_vec();
                        bind(rest.clone(), Value::array(rest_value))?;
                    }
                    Ok(())
                })?;
            }
        }
        Ok(())
    }

    fn eval_app(&mut self, f: &Value, args: &[Value]) -> EvalResult {
        let cont = self.eval_app_cont(f, args)?;
        self.eval_cont(cont)
    }
    fn eval_app_cont(&mut self, f: &Value, args: &[Value]) -> EvalResult<Cont> {
        match f {
            Value::Intrinsic(name) => {
                let f = self
                    .intrinsics
                    .get(&*name.0)
                    .unwrap_or_else(|| panic!("Intrinsic not registered: {}", name));
                f(args).map(Cont::Finished)
            }
            Value::Ref(RefValue::Fun {
                params,
                body,
                local_env,
                current_module,
            }) => {
                if params.len() != args.len() {
                    return Err(EvalError::ArgumentLength {
                        expected: params.len(),
                        actual: args.len(),
                    });
                }
                let local_env = LocalEnv::extend(local_env.clone());
                for (param, arg) in params.iter().zip(args.iter()) {
                    LocalEnv::bind(&local_env, param.clone(), arg.clone())?;
                }
                Ok(Cont::Next {
                    current_module: current_module.clone(),
                    local_env: Some(local_env),
                    expr: (**body).clone(),
                })
            }
            _ => Err(EvalError::TypeError("Intrinsic".to_owned(), f.clone())),
        }
    }

    pub fn get_var(
        &self,
        local_env: &Option<LocalEnvRef>,
        current_module: &Module,
        name: &Ident,
    ) -> EvalResult {
        LocalEnv::get_var(local_env, name)
            .or_else(|| current_module.lookup(&name.0))
            .ok_or_else(|| EvalError::NameNotFound(name.to_string()))
    }
    fn get_index(&self, value: &Value, index: &Value) -> EvalResult {
        let index: usize = index.try_into()?;
        value.use_array(|values| {
            EvalError::check_array_index(values.len(), index)?;
            Ok(values[index].clone())
        })
    }

    fn eval_args(
        &mut self,
        args: &[Expr],
        local_env: &Option<LocalEnvRef>,
        current_module: &Gc<Module>,
    ) -> Result<Vec<Value>, EvalError> {
        args.iter()
            .map(|a| self.eval_expr(a, local_env, current_module))
            .collect::<Result<Vec<_>, _>>()
    }

    fn as_object<T, F: FnOnce(&ObjectValue) -> EvalResult<T>>(
        &self,
        value: &Value,
        f: F,
    ) -> EvalResult<T> {
        let proto = &self.proto;
        let proto = match value {
            Value::Int(_) => &proto.int,
            Value::Bool(_) => &proto.bool,
            Value::Null => &proto.null,
            Value::Str(_) => &proto.string,
            Value::Sym(_) => &proto.symbol,
            Value::Intrinsic(_) => &proto.function,
            Value::Ref(RefValue::Array(_)) => &proto.array,
            Value::Ref(RefValue::Fun { .. }) => &proto.function,
            Value::Ref(RefValue::Object(obj)) => {
                return f(&obj.borrow());
            }
        };
        f(&proto.borrow())
    }
    fn prop_spec_to_object_key(
        &mut self,
        prop: &PropSpec,
        local_env: &Option<LocalEnvRef>,
        current_module: &Gc<Module>,
    ) -> EvalResult<ObjectKey> {
        let key = match prop {
            PropSpec::Const(name) => ObjectKey::Str(Rc::new(name.to_owned())),
            PropSpec::Dyn(expr) => {
                let name = self.eval_expr(expr, local_env, current_module)?;
                match name {
                    Value::Str(name) => ObjectKey::Str(name.clone()),
                    Value::Sym(name) => ObjectKey::Sym(name.clone()),
                    _ => return Err(EvalError::type_error("String|Symbol", name.clone())),
                }
            }
        };
        Ok(key)
    }
    fn get_prop(&self, value: &Value, key: &ObjectKey) -> EvalResult {
        self.as_object(value, |obj| {
            obj.get(key)
                .cloned()
                .ok_or_else(|| EvalError::property_not_found(key.clone()))
        })
    }
    pub fn bind_var(
        &mut self,
        local_env: &Option<LocalEnvRef>,
        current_module: &Module,
        name: Ident,
        value: Value,
    ) -> Result<(), EvalError> {
        if let Some(local_env) = local_env {
            LocalEnv::bind(local_env, name.clone(), value)
        } else {
            // TODO: expr should not bind global
            current_module.bind((*name.0).clone(), value, false, self.allow_rebind_global)
        }
    }

    fn reassign_var(
        &mut self,
        local_env: &Option<LocalEnvRef>,
        current_module: &Module,
        name: &Ident,
        value: Value,
    ) -> Result<(), EvalError> {
        match LocalEnv::reassign_if_exists(local_env, name, value) {
            Ok(_) => Ok(()),
            Err(value) => current_module.reassign((*name.0).clone(), value),
        }
    }
}
