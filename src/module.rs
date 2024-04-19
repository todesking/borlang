use crate::parse_program;
use crate::parser::ParseError;
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

use std::io::Read;
use std::path::PathBuf;
use std::rc::Rc;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ModulePath(String);
impl ModulePath {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Self(s.into())
    }

    fn parts_iter(&self) -> impl Iterator<Item = &str> {
        self.0.split('/')
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
            let program = self.loader.load(&path).map_err(EvalError::LoadError)?;
            let m = Gc::new(Module::new(path.clone()));
            initialize(&m, &program)?;
            self.modules.insert(path, m.clone());
            Ok(m)
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    #[error("Module `{0}` not found")]
    NotFound(ModulePath),
    #[error(transparent)]
    ParseError(ParseError),
    #[error(transparent)]
    IoError(std::io::Error),
}
impl PartialEq for LoadError {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::IoError(_) => false,
            Self::ParseError(e1) => match other {
                Self::ParseError(e2) => e1 == e2,
                _ => false,
            },
            Self::NotFound(m1) => match other {
                Self::NotFound(m2) => m1 == m2,
                _ => false,
            },
        }
    }
}

pub trait ModuleLoader {
    fn load(&mut self, path: &ModulePath) -> Result<Program, LoadError>;
}

pub struct NullModuleLoader;
impl ModuleLoader for NullModuleLoader {
    fn load(&mut self, path: &ModulePath) -> Result<Program, LoadError> {
        Err(LoadError::NotFound(path.clone()))
    }
}

pub struct FsModuleLoader {
    paths: Vec<PathBuf>,
}
impl FsModuleLoader {
    pub fn new(paths: Vec<PathBuf>) -> Self {
        Self { paths }
    }
}
impl ModuleLoader for FsModuleLoader {
    fn load(&mut self, path: &ModulePath) -> Result<Program, LoadError> {
        let mut rel_path = PathBuf::new();
        for p in path.parts_iter() {
            rel_path.push(p);
        }
        rel_path.set_extension("borlang");

        for root in &self.paths {
            match std::fs::File::open(root.join(&rel_path)) {
                Ok(mut file) => {
                    let mut src = String::new();
                    let _size = file.read_to_string(&mut src).map_err(LoadError::IoError)?;
                    return parse_program(&src).map_err(LoadError::ParseError);
                }
                Err(err) => match err.kind() {
                    std::io::ErrorKind::NotFound => continue,
                    _ => {
                        return Err(LoadError::IoError(err));
                    }
                },
            }
        }
        Err(LoadError::NotFound(path.clone()))
    }
}

#[cfg(test)]
mod test {
    use super::*;


    #[test]
    fn test_fs_empty_root() {
        let mut loader = FsModuleLoader::new([].to_vec());
        assert_eq!(
            loader.load(&ModulePath::new("aaa")),
            Err(LoadError::NotFound(ModulePath::new("aaa"))),
        );
    }

    #[test]
    fn test_fs() {
        let mut loader = FsModuleLoader::new(vec![
            PathBuf::from_iter(["test_data", "fs_module_loader"].iter()),
        ]);

        assert_eq!(
            loader.load(&ModulePath::new("empty")),
            Ok(crate::ast::Program{top_terms: vec![]})
        );
        assert_eq!(
            loader.load(&ModulePath::new("not_found")),
            Err(LoadError::NotFound(ModulePath::new("not_found"))),
        );
        assert!(
            matches!(
                loader.load(&ModulePath::new("syntax_error")),
                Err(LoadError::ParseError(_)),
            )
        );
    }
}
