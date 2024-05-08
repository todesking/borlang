use crate::ast::Ident;
use crate::ast::LetPattern;
use crate::ast::TopTerm;
use crate::compiler::compile;
use crate::compiler::VM;
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

pub struct Prototype {
    null: ObjectValueRef,
    int: ObjectValueRef,
    bool: ObjectValueRef,
    string: ObjectValueRef,
    symbol: ObjectValueRef,
    function: ObjectValueRef,
    array: ObjectValueRef,
}

pub struct RuntimeContext<Loader: ModuleLoader> {
    module_loader: Loader,
    modules: HashMap<ModulePath, Gc<Module>>,
    intrinsics: HashMap<String, fn(&[Value]) -> EvalResult>,
    allow_rebind_global: bool,
    proto: Prototype,
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
        for sym_name in [
            "op_plus",
            "op_minus",
            "op_mul",
            "op_mod",
            "op_gt",
            "op_ge",
            "op_lt",
            "op_le",
            "op_negate",
            "op_not",
            "op_range",
            "op_range_eq",
            "iterable_iterator",
            "iterator_next",
        ] {
            internal
                .bind_symbol(Rc::new(sym_name.to_owned()), true)
                .unwrap();
        }

        let dummy_proto = Gc::new(GcCell::new(ObjectValue::new()));
        let mut rt = Self {
            module_loader,
            modules: Default::default(),
            intrinsics: Default::default(),
            allow_rebind_global: false,
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

    pub fn is_allow_rebind_global(&self) -> bool {
        self.allow_rebind_global
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
        let insns = compile(expr).map_err(EvalError::CompileError)?;
        let mut vm = VM::new(module.clone(), Rc::new(insns));
        vm.run(self)
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

    pub fn call_intrinsic(&self, name: &str, args: &[Value]) -> EvalResult {
        let f = self
            .intrinsics
            .get(name)
            .unwrap_or_else(|| panic!("Intrinsic not registered: {}", name));
        f(args)
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

    pub fn as_object<T, F: FnOnce(&ObjectValue) -> EvalResult<T>>(
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
            Value::Ref(RefValue::Fun2 { .. }) => &proto.function,
            Value::Ref(RefValue::Object(obj)) => {
                return f(&obj.borrow());
            }
        };
        f(&proto.borrow())
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
}
