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

use crate::value::AtomValue;
use crate::value::LocalEnv;
use crate::value::LocalEnvRef;
use crate::value::ObjectKey;
use crate::value::ObjectValue;
use crate::value::RefValue;
use crate::EvalError;
use crate::Expr;

use crate::Program;
use crate::Value;
use gc::Gc;

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

pub type EvalResult<T = Value> = Result<T, EvalError>;
pub type IntrinsicFn = dyn Fn(&[Value]) -> EvalResult;

pub struct RuntimeContext<Loader: ModuleLoader> {
    module_loader: Loader,
    modules: HashMap<ModulePath, Gc<Module>>,
    intrinsics: HashMap<String, fn(&[Value]) -> EvalResult>,
    allow_rebind_global: bool,
    array_proto: RefCell<Value>,
}

impl RuntimeContext<FsModuleLoader> {
    pub fn with_paths<P: Into<PathBuf>>(paths: Vec<P>) -> Self {
        Self::new(FsModuleLoader::new(
            paths.into_iter().map(|x| x.into()).collect(),
        ))
    }
}

impl<L: ModuleLoader> RuntimeContext<L> {
    pub fn new(module_loader: L) -> Self {
        let mut rt = Self {
            module_loader,
            modules: Default::default(),
            intrinsics: Default::default(),
            allow_rebind_global: false,
            array_proto: RefCell::new(Value::null()),
        };

        register_intrinsics(&mut rt);

        let std_internal = rt.new_module(ModulePath::new("std.internal")).unwrap();
        std_internal
            .bind("intrinsic", Value::intrinsic("intrinsic"), true, false)
            .unwrap();
        let std = rt.load_module(&ModulePath::new("std")).unwrap();
        *rt.array_proto.borrow_mut() = std
            .lookup("Array")
            .unwrap()
            .use_object(|o| {
                o.get(&ObjectKey::new_str_from_str("prototype"))
                    .cloned()
                    .ok_or_else(|| unreachable!())
            })
            .unwrap();
        rt
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
        if module.path.as_ref() == "std/prelude" {
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
                let value = Value::sym(format!("{}.{}", module.path, name.0));
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
        match expr {
            Expr::Int(v) => Ok(AtomValue::Int(*v).into()),
            Expr::Str { content } => Ok(AtomValue::Str(content.clone()).into()),
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
                Ok(Value::object(buf))
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
                Ok(Value::array(buf))
            }
            Expr::Var(name) => self.get_var(local_env, current_module, name),
            Expr::Paren { expr } => self.eval_expr(expr, local_env, current_module),
            Expr::Do(block) => self.eval_block(block, local_env, current_module),
            Expr::Binop { lhs, op, rhs } => {
                let lhs = self.eval_expr(lhs, local_env, current_module)?;
                let rhs = self.eval_expr(rhs, local_env, current_module)?;
                let intrinsic_name = match &**op.0 {
                    "+" => "op_plus",
                    "-" => "op_minus",
                    "*" => "op_mul",
                    "%" => "op_mod",
                    "==" => "op_eq",
                    "!=" => "op_ne",
                    ">" => "op_gt",
                    ">=" => "op_ge",
                    "<" => "op_lt",
                    "<=" => "op_le",
                    unk => panic!("Unknown binary operator: {unk}"),
                };
                let f = Value::intrinsic(intrinsic_name);
                self.eval_app(&f, &[lhs, rhs])
            }
            Expr::Negate { expr } => {
                let f = Value::intrinsic("op_negate");
                let v = self.eval_expr(expr, local_env, current_module)?;
                self.eval_app(&f, &[v])
            }
            Expr::App { expr: f, args } => match &**f {
                Expr::Prop { expr, prop } => {
                    let this = self.eval_expr(expr, local_env, current_module)?;
                    let key = self.prop_spec_to_object_key(prop, local_env, current_module);
                    let f = self.get_prop(&this, &key?)?;
                    let mut args = self.eval_args(args, local_env, current_module)?;
                    args.insert(0, this);
                    self.eval_app(&f, &args)
                }
                _ => {
                    let f = self.eval_expr(f, local_env, current_module)?;
                    let args = self.eval_args(args, local_env, current_module)?;
                    self.eval_app(&f, &args)
                }
            },
            Expr::Let { name, expr } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                Self::let_bind(name, value, |n, v| {
                    self.bind_var(local_env, current_module, n, v)
                })?;
                Ok(Value::null())
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
                Ok(Value::null())
            }
            Expr::If { cond, th, el } => {
                let cond = self.eval_expr(cond, local_env, current_module)?;
                let cond = (&cond).try_into()?;
                let value = if cond {
                    self.eval_block(th, local_env, current_module)?
                } else {
                    el.as_ref()
                        .map(|el| self.eval_block(el, local_env, current_module))
                        .unwrap_or_else(|| Ok(Value::null()))?
                };
                Ok(value)
            }
            Expr::For { name, target, body } => {
                let target = self.eval_expr(target, local_env, current_module)?;
                let iter_new = self.get_prop(&target, &ObjectKey::new_str_from_str("iterator"))?;
                let iter = self.eval_app(&iter_new, &[target])?;
                let args = [iter];
                loop {
                    let next = self.get_prop(&args[0], &ObjectKey::new_str_from_str("next"))?;
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
                    self.eval_block(body, &Some(env), current_module)?;
                }
                Ok(Value::null())
            }
            Expr::Fun { params, expr } => Ok(Value::fun(
                params.clone(),
                expr.clone(),
                local_env.clone(),
                current_module.clone(),
            )),
            Expr::Prop { expr, prop } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                let key = self.prop_spec_to_object_key(prop, local_env, current_module)?;
                self.get_prop(&value, &key)
            }
            Expr::PropOpt { expr, prop } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                if value.is_null() {
                    return Ok(Value::null());
                }
                let key = self.prop_spec_to_object_key(prop, local_env, current_module)?;
                self.get_prop(&value, &key)
            }
            Expr::Index { expr, index } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                let index = self.eval_expr(index, local_env, current_module)?;
                self.get_index(&value, &index)
            }
            Expr::Import(ExprStr { content }) => {
                let module = self.load_module(&ModulePath::new((**content).to_owned()))?;
                Ok(module.pub_object().clone())
            }
        }
    }

    fn eval_block(
        &mut self,
        block: &Block,
        local_env: &Option<LocalEnvRef>,
        current_module: &Gc<Module>,
    ) -> EvalResult {
        let Block { terms, expr } = block;
        let local_env = Some(LocalEnv::extend(local_env.clone()));
        for e in terms {
            self.eval_expr(e, &local_env, current_module)?;
        }
        if let Some(e) = expr {
            self.eval_expr(e, &local_env, current_module)
        } else {
            Ok(Value::null())
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
        match f {
            Value::Atom(AtomValue::Intrinsic(name)) => {
                let f = self
                    .intrinsics
                    .get(&*name.0)
                    .unwrap_or_else(|| panic!("Intrinsic not registered: {}", name));
                f(args)
            }
            Value::Ref(v) => match &**v {
                RefValue::Fun {
                    params,
                    body,
                    local_env,
                    current_module,
                } => {
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
                    self.eval_expr(body, &Some(local_env), current_module)
                }
                _ => Err(EvalError::TypeError("Fun".into(), f.clone())),
            },
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

    fn as_object<F: FnOnce(&ObjectValue) -> EvalResult>(&self, value: &Value, f: F) -> EvalResult {
        match value {
            Value::Atom(_) => f(&ObjectValue::new()),
            Value::Ref(ref_value) => match &**ref_value {
                RefValue::Object(obj) => {
                    let obj = obj.borrow();
                    f(&obj)
                }
                RefValue::Array(_) => self.array_proto.borrow().use_object(f),
                _ => f(&ObjectValue::new()),
            },
        }
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
                    Value::Atom(AtomValue::Str(name)) => ObjectKey::Str(name.clone()),
                    Value::Atom(AtomValue::Sym(name)) => ObjectKey::Sym(name.clone()),
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
