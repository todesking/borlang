use std::rc::Rc;

use gc::Gc;

use crate::{
    ast::{Block, Ident, LetPattern, ObjItem, PropSpec},
    module::{Module, ModuleLoader, ModulePath},
    value::{LocalEnv, LocalEnvRef, ObjectValue, RefValue},
    EvalError, EvalResult, Expr, RuntimeContext, Value,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Insn {
    /// `[] -> [new_obj]`
    ObjectNew,
    /// `[obj, key, value] -> [obj]`
    ObjectPut,
    /// `[obj, obj2] -> obj`
    ObjectMerge,
    /// `[obj] -> [obj_copy]`
    ObjectCopy,
    /// `[obj, prop] -> [obj, value]`
    ObjectPluck,
    /// `[] -> [new_array]`
    ArrayNew,
    /// `[arr, value] -> [arr]`
    ArrayPush,
    /// `[arr, arr2] -> [arr]`
    ArrayExtend,
    // TODO: split into discrete literal types
    /// `[] -> [value]`
    Literal(Value),
    /// `[] -> [value]`
    Var(Rc<String>),
    /// `[f, a1, .., a_n] -> [ret]`
    App(usize),
    /// `[value] -> []`
    Bind(Rc<String>),
    /// `[value] -> []`
    Assign(Rc<String>),
    /// `[arr, index, value] -> []`
    AssignIndex,
    /// `[obj, index, value] -> []`
    AssignProp,
    /// `[arr, index] -> [value]`
    ArrayIndex,
    /// `[obj, prop] -> [value]`
    Prop,
    /// false: `[arr] -> [a1, .., an]`
    /// true: `[arr] -> [a1, .., an, rest]`
    ArrDecompose(usize, bool),
    /// `[] -> []`
    Nop,
    /// `[cond] -> []`
    IfNot(usize),
    /// `[] -> []`
    Jmp(usize),
    /// `[] -> []`
    Ret,
    /// `[value] -> [value, value]`
    Dup,
    /// `[v1, v2] -> [eq]`
    Eq,
    /// `[value] -> []`
    Pop,
    /// `[] -> [fun]`
    Fun(Rc<Vec<Rc<String>>>, Rc<Vec<Insn>>),
    /// `[a, b] -> [b, a]`
    Swap,
    /// `[] -> [module]`
    Import(Rc<String>),
    /// `[] -> []`
    TryEnter(usize),
    /// `[] -> ?`
    TryExit,
    /// `[] -> []`
    PushEnv,
    /// `[] -> []`
    PopEnv,
    /// `[] -> []`
    Halt,
}
impl Insn {
    pub fn null() -> Insn {
        Insn::Literal(Value::null())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompileError {
    AssignNotSupported,
}

#[derive(Debug)]
pub struct ExceptionHandler {
    env_depth: usize,
    state_depth: usize,
    stack_depth: usize,
    index: usize,
}

#[derive(Debug)]
pub struct VMState {
    module: Gc<Module>,
    insns: Rc<Vec<Insn>>,
    pc: usize,
}
impl VMState {
    fn new(module: Gc<Module>, insns: Rc<Vec<Insn>>) -> Self {
        Self {
            module,
            insns,
            pc: 0,
        }
    }

    fn jmp(&mut self, dest: usize) {
        // TODO: sanity check
        self.pc = dest;
    }
    fn inc_pc(&mut self) {
        self.pc += 1;
    }
}

#[derive(Debug)]
pub struct VM {
    stack: Vec<Value>,
    state: VMState,
    state_stack: Vec<VMState>,
    env: Option<LocalEnvRef>,
    env_stack: Vec<LocalEnvRef>,
    exception_handlers: Vec<ExceptionHandler>,
}
impl VM {
    pub fn new(module: Gc<Module>, insns: Rc<Vec<Insn>>) -> Self {
        VM {
            state: VMState::new(module, insns),
            state_stack: Vec::new(),
            env: None,
            env_stack: Vec::new(),
            stack: Vec::new(),
            exception_handlers: Vec::new(),
        }
    }
    pub fn run<L: ModuleLoader>(&mut self, rt: &mut RuntimeContext<L>) -> Result<Value, EvalError> {
        loop {
            // TODO: exception handling
            match self.step(rt) {
                Ok(false) => {}
                Ok(true) => {
                    return Ok(self.stack.last().unwrap().clone());
                }
                Err(EvalError::Exception { data }) => {
                    self.handle_exception(data.clone())?;
                }
                Err(e) => return Err(e),
            }
        }
    }

    fn handle_exception(&mut self, e: Value) -> EvalResult<()> {
        let Some(ExceptionHandler {
            env_depth,
            state_depth,
            stack_depth,
            index,
        }) = self.exception_handlers.pop()
        else {
            return Err(EvalError::Exception { data: e });
        };
        assert!(self.env_stack.len() >= env_depth);
        self.env_stack.truncate(env_depth + 1);
        self.env = self.env_stack.pop();

        assert!(self.state_stack.len() >= state_depth);
        self.state_stack.truncate(state_depth + 1);
        self.state = self.state_stack.pop().unwrap();

        assert!(self.stack.len() >= stack_depth);
        self.stack.truncate(stack_depth);
        self.stack.push(e);

        self.state.jmp(index);
        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn pop_n(&mut self, n: usize) -> Vec<Value> {
        self.stack.drain(self.stack.len() - n..).collect::<Vec<_>>()
    }

    fn inc_pc(&mut self) {
        self.state.inc_pc();
    }
    fn jmp(&mut self, dest: usize) {
        self.state.jmp(dest);
    }

    fn push_state(&mut self, module: Gc<Module>, insns: Rc<Vec<Insn>>) {
        let state = VMState::new(module, insns);
        let prev_state = std::mem::replace(&mut self.state, state);
        self.state_stack.push(prev_state);
    }

    fn pop_state(&mut self) {
        self.state = self.state_stack.pop().unwrap();
    }

    fn bind(&mut self, name: &str, value: Value, allow_rebind_global: bool) -> EvalResult<()> {
        if let Some(env) = &self.env {
            LocalEnv::bind(env, Ident::new(name), value)
        } else {
            self.state
                .module
                .bind(name, value, false, allow_rebind_global)
        }
    }

    fn assign(&mut self, name: &str, value: Value) -> EvalResult<()> {
        match LocalEnv::reassign_if_exists(&self.env, &Ident::new(name), value) {
            Ok(_) => Ok(()),
            // TODO: Use module.assign
            Err(value) => {
                if self.state.module.lookup(name).is_none() {
                    return Err(EvalError::name_not_found(name));
                }
                self.state.module.bind(name, value, false, true)
            }
        }
    }

    fn var(&self, name: &str) -> EvalResult {
        // TODO: rm Ident
        LocalEnv::get_var(&self.env, &Ident::new(name))
            .or_else(|| self.state.module.lookup(name))
            .ok_or_else(|| EvalError::name_not_found(name))
    }

    fn push_env(&mut self, env: LocalEnvRef) {
        let mut env = Some(env);
        std::mem::swap(&mut self.env, &mut env);
        if let Some(prev) = env {
            self.env_stack.push(prev);
        }
    }

    fn pop_env(&mut self) {
        assert!(self.env.is_some());
        self.env = self.env_stack.pop();
    }

    fn step<L: ModuleLoader>(&mut self, rt: &mut RuntimeContext<L>) -> Result<bool, EvalError> {
        match &self.state.insns[self.state.pc] {
            Insn::Literal(v) => {
                self.push(v.clone());
                self.inc_pc();
            }
            Insn::Var(name) => {
                self.push(self.var(name)?);
                self.inc_pc();
            }
            Insn::App(n) => {
                let args = self.pop_n(*n);
                let f = self.pop();
                self.inc_pc();
                match f {
                    Value::Intrinsic(id) => {
                        self.push(rt.call_intrinsic(&id.0, &args)?);
                    }
                    Value::Ref(RefValue::Fun2 {
                        params,
                        body,
                        local_env,
                        current_module,
                    }) => {
                        EvalError::check_argument_len(params.len(), args.len())?;
                        let env = LocalEnv::extend(local_env.clone());
                        for (p, a) in params.iter().zip(args) {
                            LocalEnv::bind(&env, Ident::new(&**p), a)?;
                        }
                        self.push_env(env);
                        self.push_state(current_module.clone(), body.clone());
                    }
                    _ => {
                        return Err(EvalError::type_error("Function", f.clone()));
                    }
                }
            }
            Insn::Bind(name) => {
                let name = name.clone();
                let value = self.pop();
                self.bind(&name, value, rt.is_allow_rebind_global())?;
                self.inc_pc();
            }
            Insn::Assign(name) => {
                let name = name.clone();
                let value = self.pop();
                self.assign(&name, value)?;
                self.inc_pc();
            }
            Insn::AssignIndex => {
                let value = self.pop();
                let index = self.pop();
                let index = usize::try_from(&index)?;
                let arr = self.pop();
                arr.use_array_mut(|arr| {
                    if arr.len() <= index {
                        return Err(EvalError::IndexOutOfBound {
                            len: arr.len(),
                            index,
                        });
                    }
                    arr[index] = value;
                    Ok(())
                })?;
                self.inc_pc();
            }
            Insn::AssignProp => {
                let value = self.pop();
                let prop = self.pop();
                let obj = self.pop();
                obj.use_object_mut(|obj| {
                    obj.insert(prop.to_object_key()?, value);
                    Ok(())
                })?;
                self.inc_pc();
            }
            Insn::ArrayIndex => {
                let index = self.pop();
                let value = self.pop();
                let index = usize::try_from(&index)?;
                let result = value.use_array(|arr| {
                    arr.get(index).cloned().ok_or(EvalError::IndexOutOfBound {
                        len: arr.len(),
                        index,
                    })
                })?;
                self.push(result);
                self.inc_pc();
            }
            Insn::Prop => {
                let prop = self.pop().to_object_key()?;
                let value = self.pop();
                let result = rt.as_object(&value, |obj| {
                    obj.get(&prop)
                        .cloned()
                        .ok_or_else(|| EvalError::property_not_found(prop))
                })?;
                self.push(result);
                self.inc_pc();
            }
            Insn::ArrDecompose(n, rest) => {
                let n = *n;
                let rest = *rest;
                let value = self.pop();
                value.use_array(|arr| {
                    if (rest && arr.len() < n) || (!rest && arr.len() != n) {
                        return Err(EvalError::array_length(n, arr.len()));
                    }
                    for x in &arr[..n] {
                        self.push(x.clone());
                    }
                    if rest {
                        self.push(Value::array(arr[n..].to_vec()));
                    }
                    Ok(())
                })?;
                self.inc_pc();
            }
            Insn::Nop => {}
            Insn::IfNot(dest) => {
                let dest = *dest;
                let cond = bool::try_from(&self.pop())?;
                if !cond {
                    self.jmp(dest);
                } else {
                    self.inc_pc();
                }
            }
            Insn::Jmp(dest) => {
                self.jmp(*dest);
            }
            Insn::Ret => {
                self.pop_state();
                self.pop_env();
            }
            Insn::Dup => {
                self.push(self.stack.last().unwrap().clone());
                self.inc_pc();
            }
            Insn::Eq => {
                let lhs = self.pop();
                let rhs = self.pop();
                self.push((lhs == rhs).into());
                self.inc_pc();
            }
            Insn::Pop => {
                self.pop();
                self.inc_pc();
            }
            Insn::Fun(params, body) => {
                let f = Value::Ref(RefValue::Fun2 {
                    params: params.clone(),
                    body: body.clone(),
                    local_env: self.env.clone(),
                    current_module: self.state.module.clone(),
                });
                self.push(f);
                self.inc_pc();
            }
            Insn::Swap => {
                let len = self.stack.len();
                self.stack.swap(len - 1, len - 2);
                self.inc_pc();
            }
            Insn::Import(path) => {
                let module = rt.load_module(&ModulePath::new(path.to_string()))?;
                self.push(module.pub_object().clone());
                self.inc_pc();
            }
            Insn::TryEnter(index) => {
                self.exception_handlers.push(ExceptionHandler {
                    index: *index,
                    env_depth: self.env_stack.len(),
                    state_depth: self.state_stack.len(),
                    stack_depth: self.stack.len(),
                });
                self.inc_pc();
            }
            Insn::TryExit => {
                self.exception_handlers.pop();
                self.inc_pc();
            }
            Insn::PushEnv => {
                self.push_env(LocalEnv::extend(self.env.clone()));
                self.inc_pc();
            }
            Insn::PopEnv => {
                self.pop_env();
                self.inc_pc();
            }
            Insn::ObjectNew => {
                self.push(ObjectValue::new().into());
                self.inc_pc();
            }
            Insn::ObjectPut => {
                let value = self.pop();
                let key = self.pop().to_object_key()?;
                let obj = self.stack.last().unwrap();
                obj.use_object_mut(|obj| {
                    obj.insert(key, value);
                    Ok(())
                })?;
                self.inc_pc();
            }
            Insn::ObjectMerge => {
                let value = self.pop();
                let obj = self.stack.last().unwrap();
                obj.use_object_mut(|obj| {
                    value.use_object(|obj2| {
                        for (k, v) in obj2.iter() {
                            obj.insert(k.clone(), v.clone());
                        }
                        Ok(())
                    })?;
                    Ok(())
                })?;
                self.inc_pc();
            }
            Insn::ObjectCopy => {
                let obj = self.pop();
                let mut new_obj = ObjectValue::new();
                obj.use_object(|obj| {
                    for (k, v) in obj.iter() {
                        new_obj.insert(k.clone(), v.clone());
                    }
                    Ok(())
                })?;
                self.push(new_obj.into());
                self.inc_pc();
            }
            Insn::ObjectPluck => {
                let key = self.pop().to_object_key()?;
                let obj = self.stack.last().unwrap();
                let Some(removed) = obj.use_object_mut(|obj| Ok(obj.remove(&key)))? else {
                    return Err(EvalError::property_not_found(key));
                };
                self.push(removed);
                self.inc_pc();
            }
            Insn::ArrayNew => {
                self.push(Value::array(Vec::new()));
                self.inc_pc();
            }
            Insn::ArrayPush => {
                let value = self.pop();
                let arr = self.stack.last().unwrap();
                arr.use_array_mut(|arr| {
                    arr.push(value.clone());
                    Ok(())
                })?;
                self.inc_pc();
            }
            Insn::ArrayExtend => {
                let value = self.pop();
                let arr = self.stack.last().unwrap();
                arr.use_array_mut(|arr| {
                    value.use_array(|arr2| {
                        arr.extend(arr2.iter().cloned());
                        Ok(())
                    })?;
                    Ok(())
                })?;
                self.inc_pc();
            }
            Insn::Halt => return Ok(true),
        }
        Ok(false)
    }
}

pub fn compile(expr: &Expr) -> Result<Vec<Insn>, CompileError> {
    let mut buf = Vec::new();
    compile_part(expr, &mut buf)?;
    buf.push(Insn::Halt);
    Ok(buf)
}

fn compile_function_body(expr: &Expr) -> Result<Vec<Insn>, CompileError> {
    let mut buf = Vec::new();
    compile_part(expr, &mut buf)?;
    buf.push(Insn::Ret);
    Ok(buf)
}

fn compile_part(expr: &Expr, buf: &mut Vec<Insn>) -> Result<(), CompileError> {
    match expr {
        Expr::Object(items) => {
            buf.push(Insn::ObjectNew);
            for item in items {
                match item {
                    ObjItem::Const { key, expr } => {
                        buf.push(Insn::Literal(Value::Str(key.0.clone())));
                        if let Some(expr) = expr {
                            compile_part(expr, buf)?;
                        } else {
                            buf.push(Insn::Var(key.0.clone()));
                        }
                        buf.push(Insn::ObjectPut);
                    }
                    ObjItem::Dyn { key, expr } => {
                        compile_part(key, buf)?;
                        compile_part(expr, buf)?;
                        buf.push(Insn::ObjectPut);
                    }
                    ObjItem::Spread(expr) => {
                        compile_part(expr, buf)?;
                        buf.push(Insn::ObjectMerge);
                    }
                }
            }
        }
        Expr::Array(items) => {
            buf.push(Insn::ArrayNew);
            for item in items {
                compile_part(&item.expr, buf)?;
                if item.spread.is_some() {
                    buf.push(Insn::ArrayExtend);
                } else {
                    buf.push(Insn::ArrayPush);
                }
            }
        }
        Expr::Int(n) => {
            buf.push(Insn::Literal(Value::Int(*n)));
        }
        Expr::Str { content } => {
            buf.push(Insn::Literal(Value::Str(content.clone())));
        }
        Expr::Var(name) => {
            buf.push(Insn::Var(name.0.clone()));
        }
        Expr::App { expr, args } => {
            // TODO: TCO
            let mut arg_len = args.len();
            match &**expr {
                Expr::Prop { expr, prop } => {
                    compile_part(expr, buf)?;
                    buf.push(Insn::Dup);
                    compile_prop(prop, buf)?;
                    buf.push(Insn::Prop);
                    buf.push(Insn::Swap);
                    arg_len += 1;
                }
                e => {
                    compile_part(e, buf)?;
                }
            }
            for a in args {
                compile_part(a, buf)?;
            }
            buf.push(Insn::App(arg_len));
        }
        Expr::Do(block) => {
            compile_part_block(block, buf, &[])?;
        }
        Expr::Let { name, expr } => {
            compile_part(expr, buf)?;
            compile_let_pattern(name, Insn::Bind, buf);
            buf.push(Insn::null());
        }
        Expr::Reassign { lhs, rhs } => {
            match &**lhs {
                Expr::Var(name) => {
                    compile_part(rhs, buf)?;
                    buf.push(Insn::Assign(name.0.clone()));
                }
                Expr::Index { expr, index } => {
                    compile_part(expr, buf)?;
                    compile_part(index, buf)?;
                    compile_part(rhs, buf)?;
                    buf.push(Insn::AssignIndex);
                }
                Expr::Prop { expr, prop } => {
                    compile_part(expr, buf)?;
                    compile_prop(prop, buf)?;
                    compile_part(rhs, buf)?;
                    buf.push(Insn::AssignProp);
                }
                _ => return Err(CompileError::AssignNotSupported),
            }
            buf.push(Insn::null());
        }
        Expr::If { cond, th, el } => {
            compile_part(cond, buf)?;

            buf.push(Insn::Nop);
            let index_if = buf.len() - 1;

            compile_part_block(th, buf, &[])?;

            if let Some(el) = el {
                buf.push(Insn::Nop);
                let index_th_end = buf.len() - 1;
                let index_el_start = buf.len();
                compile_part_block(el, buf, &[])?;
                let index_el_after = buf.len();
                buf[index_if] = Insn::IfNot(index_el_start);
                buf[index_th_end] = Insn::Jmp(index_el_after);
            } else {
                let index_th_after = buf.len();
                buf[index_if] = Insn::IfNot(index_th_after);
            }
        }
        Expr::For { name, target, body } => {
            compile_part(target, buf)?;

            // iter = target.[iterator]()
            buf.push(Insn::Dup);
            buf.push(Insn::Literal(Value::Sym(Rc::new(
                "_internal:iterable_iterator".to_owned(),
            ))));
            buf.push(Insn::Prop);
            buf.push(Insn::Swap);
            buf.push(Insn::App(1));

            let index_loop_start = buf.len();

            // item = iter.next()
            buf.push(Insn::Dup);
            buf.push(Insn::Dup);
            buf.push(Insn::Literal(Value::Sym(Rc::new(
                "_internal:iterator_next".to_owned(),
            ))));
            buf.push(Insn::Prop);
            buf.push(Insn::Swap);
            buf.push(Insn::App(1));

            // if item.len() == 0 goto end
            buf.push(Insn::Dup);
            buf.push(Insn::Dup);
            buf.push(Insn::Literal(Value::Str(Rc::new("len".to_owned()))));
            buf.push(Insn::Prop);
            buf.push(Insn::Swap);
            buf.push(Insn::App(1));
            buf.push(Insn::Literal(Value::Int(0)));
            buf.push(Insn::Eq);
            compile_part_unary_op("_internal:op_not", buf);
            buf.push(Insn::Nop);
            let index_loop_cond = buf.len() - 1;

            // [name] = item[0]
            buf.push(Insn::Literal(Value::Int(0)));
            buf.push(Insn::ArrayIndex);

            // loop body
            compile_part_block(body, buf, &[Insn::Bind(name.0.clone())])?;
            buf.push(Insn::Pop);
            buf.push(Insn::Jmp(index_loop_start));

            let index_loop_after = buf.len();
            buf[index_loop_cond] = Insn::IfNot(index_loop_after);
        }
        Expr::Fun { params, expr } => {
            let body = compile_function_body(expr)?;
            buf.push(Insn::Fun(
                Rc::new(params.iter().map(|i| i.0.clone()).collect()),
                Rc::new(body),
            ));
        }
        Expr::Paren { expr } => {
            compile_part(expr, buf)?;
        }
        Expr::Prop { expr, prop } => {
            compile_part(expr, buf)?;
            compile_prop(prop, buf)?;
            buf.push(Insn::Prop);
        }
        Expr::PropOpt { expr, prop } => {
            compile_part(expr, buf)?;
            buf.push(Insn::Dup);
            buf.push(Insn::null());
            buf.push(Insn::Eq);
            compile_part_unary_op("_internal:op_not", buf);
            buf.push(Insn::Nop);
            let index_jmp_null = buf.len() - 1;

            compile_prop(prop, buf)?;
            buf.push(Insn::Prop);
            buf.push(Insn::Nop);
            let index_jmp_finish = buf.len() - 1;

            buf.push(Insn::null());
            let index_null = buf.len() - 1;

            let index_after = buf.len();

            buf[index_jmp_null] = Insn::IfNot(index_null);
            buf[index_jmp_finish] = Insn::Jmp(index_after);
        }
        Expr::Index { expr, index } => {
            compile_part(expr, buf)?;
            compile_part(index, buf)?;
            buf.push(Insn::ArrayIndex);
        }
        Expr::Binop { lhs, op, rhs } => {
            if &*op.0 == "==" {
                compile_part(lhs, buf)?;
                compile_part(rhs, buf)?;
                buf.push(Insn::Eq);
                return Ok(());
            }
            if &*op.0 == "!=" {
                compile_part(lhs, buf)?;
                compile_part(rhs, buf)?;
                buf.push(Insn::Eq);
                compile_part_unary_op("_internal:op_not", buf);
                return Ok(());
            }
            let sym_name = match &**op.0 {
                "+" => "op_plus",
                "-" => "op_minus",
                "*" => "op_mul",
                "%" => "op_mod",
                ">" => "op_gt",
                ">=" => "op_ge",
                "<" => "op_lt",
                "<=" => "op_le",
                ".." => "op_range",
                "..=" => "op_range_eq",
                unk => panic!("Unknown operator: {unk}"),
            };
            // TODO: use std/ops
            let sym_name = format!("_internal:{sym_name}");
            compile_part(lhs, buf)?;
            compile_part_binary_op(rhs, sym_name, buf)?;
        }
        Expr::Negate { expr } => {
            compile_part(expr, buf)?;
            // TODO: use std/ops
            compile_part_unary_op("_internal:op_negate", buf);
        }
        Expr::Not { expr } => {
            compile_part(expr, buf)?;
            // TODO: use std/ops
            compile_part_unary_op("_internal:op_not", buf);
        }
        Expr::Import(path) => {
            buf.push(Insn::Import(path.content.clone()));
        }
        Expr::Catch {
            expr,
            name,
            catch_expr,
        } => {
            buf.push(Insn::Nop);
            let try_enter_index = buf.len() - 1;
            compile_part(expr, buf)?;
            buf.push(Insn::TryExit);
            buf.push(Insn::Nop);
            let try_end_index = buf.len() - 1;

            let catch_start_index = buf.len();
            buf.push(Insn::Bind(name.0.clone()));
            compile_part(catch_expr, buf)?;
            let catch_after_index = buf.len();

            buf[try_enter_index] = Insn::TryEnter(catch_start_index);
            buf[try_end_index] = Insn::Jmp(catch_after_index);
        }
    }
    Ok(())
}

/// `[value] -> [result]`
fn compile_part_unary_op<S: Into<String>>(name: S, buf: &mut Vec<Insn>) {
    buf.push(Insn::Dup);
    let sym = Value::sym(name.into());
    buf.push(Insn::Literal(sym));
    buf.push(Insn::Prop);
    buf.push(Insn::Swap);
    buf.push(Insn::App(1));
}

/// `[lhs] -> [result]`
fn compile_part_binary_op<S: Into<String>>(
    rhs: &Expr,
    name: S,
    buf: &mut Vec<Insn>,
) -> Result<(), CompileError> {
    buf.push(Insn::Dup);
    let sym = Value::sym(name.into());
    buf.push(Insn::Literal(sym));
    buf.push(Insn::Prop);
    buf.push(Insn::Swap);
    compile_part(rhs, buf)?;
    buf.push(Insn::App(2));
    Ok(())
}

/// `[value] -> []`
fn compile_let_pattern(
    pat: &LetPattern,
    insn_bind: impl Fn(Rc<String>) -> Insn,
    buf: &mut Vec<Insn>,
) {
    match pat {
        LetPattern::Name(name) => {
            buf.push(insn_bind(name.0.clone()));
        }
        LetPattern::Obj { name, rest } => {
            buf.push(Insn::ObjectCopy);
            for n in name.iter().rev() {
                buf.push(Insn::Literal(Value::Str(n.0.clone())));
                buf.push(Insn::ObjectPluck);
                buf.push(insn_bind(n.0.clone()));
            }
            if let Some(rest) = rest {
                buf.push(insn_bind(rest.0.clone()));
            } else {
                buf.push(Insn::Pop);
            }
        }
        LetPattern::Arr { name, rest } => {
            if let Some(rest) = rest {
                buf.push(Insn::ArrDecompose(name.len(), true));
                buf.push(insn_bind(rest.0.clone()));
            } else {
                buf.push(Insn::ArrDecompose(name.len(), false));
            }
            for n in name.iter().rev() {
                buf.push(insn_bind(n.0.clone()));
            }
        }
    }
}

/// `[] -> [value]`
fn compile_part_block(
    block: &Block,
    buf: &mut Vec<Insn>,
    prefix: &[Insn],
) -> Result<(), CompileError> {
    buf.push(Insn::PushEnv);
    buf.extend(prefix.iter().cloned());
    for t in block.terms.iter() {
        compile_part(t, buf)?;
        buf.push(Insn::Pop);
    }
    if let Some(expr) = &block.expr {
        compile_part(expr, buf)?;
    } else {
        buf.push(Insn::null());
    }
    buf.push(Insn::PopEnv);
    Ok(())
}

fn compile_prop(prop: &PropSpec, buf: &mut Vec<Insn>) -> Result<(), CompileError> {
    match prop {
        PropSpec::Dyn(prop) => {
            compile_part(prop, buf)?;
        }
        PropSpec::Const(name) => buf.push(Insn::Literal(Value::Str(Rc::new(name.clone())))),
    };
    Ok(())
}
