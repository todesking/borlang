use crate::value::ObjectKey;
use crate::value::ObjectValue;

use crate::EvalError;
use crate::EvalResult;

use crate::Program;
use crate::Value;
use gc::Gc;
use gc::GcCell;
use gc::Trace;
use std::collections::HashMap;
use std::fmt::Display;

use std::rc::Rc;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ModulePath(String);
impl ModulePath {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Self(s.into())
    }
}
impl Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleLocator(String);
impl ModuleLocator {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Self(s.into())
    }
    pub fn resolve(&self, _base: &Option<ModulePath>) -> ModulePath {
        ModulePath::new(self.0.clone())
    }
}

#[derive(Debug, Clone, Eq, gc::Finalize)]
pub struct Module {
    values: GcCell<HashMap<String, Value>>,
    pub_object: Value,
    pub path: ModulePath,
}
impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}
impl Module {
    pub fn new(path: ModulePath) -> Module {
        Module {
            values: Default::default(),
            pub_object: Value::object(ObjectValue::new()),
            path,
        }
    }
    pub fn is_bound(&self, name: &str) -> bool {
        self.values.borrow().contains_key(name)
    }
    pub fn bind<S: Into<String>>(
        &self,
        name: S,
        value: Value,
        is_pub: bool,
        allow_rebind: bool,
    ) -> EvalResult<()> {
        let name = name.into();
        if !allow_rebind && self.is_bound(&name) {
            return Err(EvalError::NameDefined(name));
        }
        if is_pub {
            self.pub_object.use_object_mut(|o| {
                o.insert(ObjectKey::Str(Rc::new(name.clone())), value.clone());
                Ok(())
            })?;
        }
        self.values.borrow_mut().insert(name, value);
        Ok(())
    }
    pub fn pub_object(&self) -> &Value {
        &self.pub_object
    }
    pub fn lookup(&self, name: &str) -> Option<Value> {
        self.values.borrow().get(name).cloned()
    }
    pub fn reassign(&self, name: String, value: Value) -> EvalResult<()> {
        if !self.is_bound(&name) {
            return Err(EvalError::NameNotFound(name));
        }
        self.values.borrow_mut().insert(name.clone(), value.clone());
        self.pub_object.use_object_mut(|o| {
            let key = ObjectKey::Str(Rc::new(name));
            if o.contains_key(&key) {
                o.insert(key, value);
            }
            Ok(())
        })?;
        Ok(())
    }
}
unsafe impl Trace for Module {
    gc::custom_trace!(this, {
        // TODO: is it safe?
        for (_k, v) in this.values.borrow().iter() {
            mark(v);
        }
        mark(&this.pub_object);
    });
}

pub struct ModuleEnv<Loader: ModuleLoader> {
    loader: Loader,
    modules: HashMap<ModulePath, Gc<Module>>,
}

impl<Loader: ModuleLoader> ModuleEnv<Loader> {
    pub fn new(loader: Loader) -> Self {
        Self {
            modules: HashMap::new(),
            loader,
        }
    }

    pub fn new_module(&mut self, path: ModulePath) -> Gc<Module> {
        let m = Gc::new(Module::new(path.clone()));
        self.modules.insert(path, m.clone());
        m
    }

    pub fn load(
        &mut self,
        path: ModulePath,
        initialize: impl FnOnce(&Gc<Module>, &Program) -> EvalResult<()>,
    ) -> EvalResult<Gc<Module>> {
        if let Some(m) = self.modules.get(&path) {
            Ok(m.clone())
        } else {
            let program = self.loader.load(&path)?;
            let m = Gc::new(Module::new(path.clone()));
            initialize(&m, &program)?;
            self.modules.insert(path, m.clone());
            Ok(m)
        }
    }
}

pub trait ModuleLoader {
    fn load(&mut self, path: &ModulePath) -> EvalResult<Program>;
}

pub struct NullModuleLoader;
impl ModuleLoader for NullModuleLoader {
    fn load(&mut self, path: &ModulePath) -> EvalResult<Program> {
        Err(EvalError::ModuleNotFound(path.clone()))
    }
}
