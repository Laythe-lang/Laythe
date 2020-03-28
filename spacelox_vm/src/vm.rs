use crate::call_frame::CallFrame;
use crate::compiler::{Compiler, CompilerResult, Parser};
use crate::constants::{define_special_string, SpecialStrings, FRAME_MAX};
use crate::memory::{Gc, NO_GC};
use spacelox_core::chunk::{decode_u16, ByteCode, UpvalueIndex};
use spacelox_core::managed::{Manage, Managed, Trace};
use spacelox_core::native::{NativeFun, NativeResult};
use spacelox_core::{
  utils::do_if_some,
  value::{BoundMethod, Class, Closure, Fun, Instance, Upvalue, Value},
};
use std::collections::HashMap;
use std::io::{stdin, stdout, Write};
use std::mem;
use std::rc::Rc;

#[cfg(feature = "debug")]
use crate::debug::disassemble_instruction;

pub type InterpretResult = Result<usize, Interpret>;

#[derive(Debug, Clone, PartialEq)]
pub enum Interpret {
  Ok,
  CompileError,
  RuntimeError,
}

/// The virtual machine for the spacelox programming language
pub struct Vm {
  /// A stack holding all local variable currently in use
  stack: Vec<Value>,

  /// A stack holding call frames currently in use
  frames: Vec<CallFrame>,

  /// The vm's garbage collector
  gc: Gc,

  /// special strings to the vm that aren't keywords
  special_strings: SpecialStrings,

  /// A persisted set of globals most for a repl context
  globals: HashMap<Managed<String>, Value>,
}

impl Vm {
  pub fn new(stack: Vec<Value>, frames: Vec<CallFrame>, natives: &[Rc<dyn NativeFun>]) -> Vm {
    let mut gc = Gc::new();
    let special_strings = define_special_string();
    let globals = define_globals(&mut gc, &natives);

    Vm {
      stack,
      frames,
      gc,
      special_strings,
      globals,
    }
  }

  pub fn repl(&mut self) {
    loop {
      let mut buffer = String::new();

      print!("> ");
      stdout().flush().expect("Could not write to stdout");

      match stdin().read_line(&mut buffer) {
        Ok(_) => {
          self.interpret(&buffer);
        }
        Err(error) => panic!(error),
      }
    }
  }

  pub fn run(&mut self, source: &str) -> Interpret {
    self.interpret(source)
  }

  fn compile(&mut self, source: &str) -> CompilerResult {
    let mut parser = Parser::new(source);

    let compiler = Compiler::new(&mut parser, None, &mut self.special_strings, &mut self.gc);
    compiler.compile()
  }

  fn interpret(&mut self, source: &str) -> Interpret {
    let result = self.compile(source);

    if !result.success {
      return Interpret::CompileError;
    }

    let script_closure = self.gc.manage(Closure::new(result.fun), &NO_GC);
    let script = Value::Closure(script_closure);
    let mut executor = VmExecutor::new(self, script);
    executor.run()
  }
}

fn define_globals<'a>(
  gc: &mut Gc,
  natives: &[Rc<dyn NativeFun>],
) -> HashMap<Managed<String>, Value> {
  let mut globals = HashMap::new();

  natives.iter().for_each(|native| {
    let name = gc.manage_str(native.meta().name.to_string(), &NO_GC);
    let native_value = Value::Native(gc.manage(Rc::clone(native), &NO_GC));

    globals.insert(name, native_value);
  });

  globals
}

struct VmExecutor<'a> {
  /// A stack of call frames for the current execution
  pub frames: &'a mut Vec<CallFrame>,

  /// The current frame depth of the program
  pub frame_count: usize,

  /// A stack holding all local variable currently in use
  pub stack: &'a mut Vec<Value>,

  /// The current fun
  current_fun: Managed<Fun>,

  /// A reference to a object currently in the vm
  gc: &'a mut Gc,

  /// the main script level function
  script: Value,

  /// class init string
  special_strings: &'a SpecialStrings,

  /// index to the top of the value stack
  pub stack_top: usize,

  /// global variable present in the vm
  pub globals: &'a mut HashMap<Managed<String>, Value>,

  /// A collection of currently available upvalues
  pub open_upvalues: Vec<Managed<Upvalue>>,
}

impl<'a> VmExecutor<'a> {
  pub fn new(vm: &'a mut Vm, script: Value) -> VmExecutor<'a> {
    let current_fun = vm.gc.manage(Fun::default(), &NO_GC);

    let mut executor = VmExecutor {
      frames: &mut vm.frames,
      frame_count: 0,
      stack: &mut vm.stack,
      script,
      current_fun,
      gc: &mut vm.gc,
      special_strings: &vm.special_strings,
      stack_top: 1,
      globals: &mut vm.globals,
      open_upvalues: Vec::with_capacity(100),
    };

    let result = executor.call(executor.script.to_closure(), 0, 0);
    match result {
      Ok(_) => executor,
      _ => panic!("Script call failed"),
    }
  }

  pub fn run(&mut self) -> Interpret {
    let mut ip: usize = 0;

    #[cfg(feature = "debug")]
    let mut last_ip: usize = 0;

    loop {
      let op_code: ByteCode = self.frame_instruction(ip);

      #[cfg(feature = "debug")]
      {
        self.print_debug(ip, last_ip);
        last_ip = ip;
      }

      let result = match op_code {
        ByteCode::Negate => self.op_negate(ip),
        ByteCode::Add => self.op_add(ip),
        ByteCode::Subtract => self.op_sub(ip),
        ByteCode::Multiply => self.op_mul(ip),
        ByteCode::Divide => self.op_div(ip),
        ByteCode::Not => self.op_not(ip),
        ByteCode::Equal => self.op_equal(ip),
        ByteCode::Greater => self.op_greater(ip),
        ByteCode::Less => self.op_less(ip),
        ByteCode::JumpIfFalse => self.op_jump_if_not_false(ip),
        ByteCode::Jump => self.op_jump(ip),
        ByteCode::Loop => self.op_loop(ip),
        ByteCode::DefineGlobal => self.op_define_global(ip),
        ByteCode::GetGlobal => self.op_get_global(ip),
        ByteCode::SetGlobal => self.op_set_global(ip),
        ByteCode::GetLocal => self.op_get_local(ip),
        ByteCode::SetLocal => self.op_set_local(ip),
        ByteCode::GetUpvalue => self.op_get_upvalue(ip),
        ByteCode::SetUpvalue => self.op_set_upvalue(ip),
        ByteCode::GetProperty => self.op_get_property(ip),
        ByteCode::SetProperty => self.op_set_property(ip),
        ByteCode::Pop => self.op_pop(ip),
        ByteCode::Nil => self.op_literal(ip, Value::Nil),
        ByteCode::True => self.op_literal(ip, Value::Bool(true)),
        ByteCode::False => self.op_literal(ip, Value::Bool(false)),
        ByteCode::Constant => self.op_constant(ip),
        ByteCode::Print => self.op_print(ip),
        ByteCode::Call => self.op_call(ip),
        ByteCode::Invoke => self.op_invoke(ip),
        ByteCode::SuperInvoke => self.op_super_invoke(ip),
        ByteCode::Closure => self.op_closure(ip),
        ByteCode::Method => self.op_method(ip),
        ByteCode::Class => self.op_class(ip),
        ByteCode::Inherit => self.op_inherit(ip),
        ByteCode::GetSuper => self.op_get_super(ip),
        ByteCode::CloseUpvalue => self.op_close_upvalue(ip),
        ByteCode::Return => self.op_return(ip),
      };

      match result {
        Ok(new_ip) => ip = new_ip,
        Err(interrupt) => {
          return interrupt;
        }
      }
    }
  }

  /// Get an immutable reference to the current callframe
  #[inline]
  fn current_frame(&self) -> &CallFrame {
    unsafe { self.frames.get_unchecked(self.frame_count - 1) }
  }

  /// Get a mutable reference to the current callframe
  #[inline]
  fn current_mut_frame(&mut self) -> &mut CallFrame {
    unsafe { self.frames.get_unchecked_mut(self.frame_count - 1) }
  }

  /// Get an immutable reference to value on the stack
  #[inline]
  fn get_val(&self, index: usize) -> Value {
    unsafe { *self.stack.get_unchecked(index) }
  }

  /// Set a value on the stack
  #[inline]
  fn set_val(&mut self, index: usize, val: Value) {
    unsafe {
      *self.stack.get_unchecked_mut(index) = val;
    }
  }

  /// read a u8 out of the bytecode
  #[inline]
  fn read_byte(&self, ip: usize) -> u8 {
    unsafe { *self.current_fun.chunk.instructions.get_unchecked(ip) }
  }

  /// read a u16 out of the bytecode
  #[inline]
  fn read_short(&self, ip: usize) -> u16 {
    decode_u16(self.read_byte(ip), self.read_byte(ip + 1))
  }

  /// Get the current instruction from the present call frame
  #[inline]
  fn frame_instruction(&self, ip: usize) -> ByteCode {
    ByteCode::from(unsafe { *self.current_fun.chunk.instructions.get_unchecked(ip) })
  }

  /// push a value onto the stack
  #[inline]
  fn push(&mut self, value: Value) {
    self.set_val(self.stack_top, value);
    self.stack_top += 1;
  }

  #[inline]
  fn pop(&mut self) -> Value {
    self.stack_top -= 1;
    unsafe { *self.stack.get_unchecked(self.stack_top) }
  }

  /// reference a value n slots from the stack head
  #[inline]
  fn peek(&self, distance: usize) -> Value {
    self.get_val(self.stack_top - (distance + 1))
  }

  fn op_literal(&mut self, ip: usize, value: Value) -> InterpretResult {
    self.push(value);
    Ok(ip + 1)
  }

  fn op_pop(&mut self, ip: usize) -> InterpretResult {
    self.pop();
    Ok(ip + 1)
  }


  fn read_constant<'b>(&self, index: u8) -> Value {
    self.current_fun.chunk.constants[index as usize]
  }

  fn read_string(&self, index: u8) -> Managed<String> {
    self.read_constant(index).to_string()
  }

  fn reset_stack(&mut self) {
    self.stack_top = 1;
    self.frame_count = 0;
    self.open_upvalues.clear();
  }

  fn op_call(&mut self, ip: usize) -> InterpretResult {
    let arg_count = self.read_byte(ip + 1);
    let callee = self.peek(arg_count as usize);

    self.resolve_call(callee, arg_count, ip + 2)
  }

  fn resolve_call(&mut self, callee: Value, arg_count: u8, ip: usize) -> InterpretResult {
    match callee {
      Value::Closure(closure) => self.call(closure, arg_count, ip),
      Value::Method(method) => self.call_method(method, arg_count, ip),
      Value::Native(native) => self.call_native(native, arg_count, ip),
      Value::Class(class) => self.call_class(class, arg_count, ip),
      Value::Fun(fun) => panic!(
        "function {} was not wrapped in a closure",
        fun.name.clone().unwrap_or("script".to_string())
      ),
      _ => self.runtime_error("Can only call functions and classes."),
    }
  }

  fn call_class(&mut self, class: Managed<Class>, arg_count: u8, ip: usize) -> InterpretResult {
    let value = Value::Instance(self.gc.manage(Instance::new(class), self));
    self.set_val(self.stack_top - (arg_count as usize) - 1, value);

    match class.init {
      Some(init) => self.call(init, arg_count, ip),
      None => {
        if arg_count != 0 {
          self.runtime_error(&format!("Expected 0 arguments but got {}", arg_count))
        } else {
          Ok(ip)
        }
      }
    }
  }

  fn call_native(
    &mut self,
    native: Managed<Rc<dyn NativeFun>>,
    arg_count: u8,
    ip: usize,
  ) -> InterpretResult {
    let meta = native.meta();
    if arg_count != meta.arity {
      return self.runtime_error(&format!(
        "Function {} expected {} argument but got {}",
        meta.name, meta.arity, arg_count,
      ));
    }

    let args = unsafe {
      self
        .stack
        .get_unchecked((self.stack_top - arg_count as usize)..self.stack_top)
    };
    let result = native.call(args);
    match result {
      NativeResult::Success(value) => {
        self.stack_top -= arg_count as usize + 1;
        self.push(value);
        Ok(ip)
      }
      NativeResult::RuntimeError(message) => {
        return self.runtime_error(&message);
      }
    }
  }

  fn call(&mut self, closure: Managed<Closure>, arg_count: u8, ip: usize) -> InterpretResult {
    if (arg_count as u16) != closure.fun.arity {
      return self.runtime_error(&format!(
        "Function {} expected {} arguments but got {}",
        closure.fun.name.clone().unwrap_or("script".to_string()),
        closure.fun.arity,
        arg_count
      ));
    }

    if self.frame_count == FRAME_MAX {
      return self.runtime_error("Stack overflow.");
    }

    if self.frame_count > 0 {
      self.current_mut_frame().ip = ip;
    }
    let current_closure = self.gc.clone_managed(closure, self);

    let frame = &mut self.frames[self.frame_count];
    frame.closure = current_closure;
    frame.ip = 0;
    frame.slots = self.stack_top - (arg_count as usize + 1);

    self.current_fun = current_closure.fun;
    self.frame_count += 1;
    Ok(0)
  }

  fn call_method(
    &mut self,
    bound: Managed<BoundMethod>,
    arg_count: u8,
    ip: usize,
  ) -> InterpretResult {
    self.set_val(self.stack_top - (arg_count as usize) - 1, bound.receiver);
    self.call(bound.method, arg_count, ip)
  }

  fn bind_method(
    &mut self,
    class: Managed<Class>,
    name: Managed<String>,
    ip: usize,
  ) -> InterpretResult {
    match class.methods.get(&name) {
      Some(method) => {
        let bound = self
          .gc
          .manage(BoundMethod::new(self.peek(0), *method), self);
        self.pop();
        self.push(Value::Method(bound));
        Ok(ip)
      }
      None => self.runtime_error(&format!("Undefined property {}", name.as_str())),
    }
  }

  fn op_invoke(&mut self, ip: usize) -> InterpretResult {
    let constant = self.read_byte(ip + 1);
    let arg_count = self.read_byte(ip + 2);

    let method_name = self.read_string(constant);
    let receiver = self.peek(arg_count as usize);

    if let Value::Instance(instance) = receiver {
      match instance.fields.get(&method_name) {
        Some(field) => {
          self.set_val(self.stack_top - (arg_count as usize) - 1, *field);
          return self.resolve_call(*field, arg_count, ip + 3);
        }
        None => self.invoke_from_class(instance.class, method_name, arg_count, ip + 3),
      }
    } else {
      self.runtime_error("Only instances have methods.")
    }
  }

  fn op_super_invoke(&mut self, ip: usize) -> InterpretResult {
    let constant = self.read_byte(ip + 1);
    let arg_count = self.read_byte(ip + 2);

    let method_name = self.read_string(constant);
    let super_class = self.pop().to_class();

    self.invoke_from_class(super_class, method_name, arg_count, ip + 3)
  }

  fn invoke_from_class(
    &mut self,
    class: Managed<Class>,
    method_name: Managed<String>,
    arg_count: u8,
    ip: usize,
  ) -> InterpretResult {
    match class.methods.get(&method_name) {
      Some(method) => self.call(*method, arg_count, ip),
      None => self.runtime_error(&format!("Undefined property {}.", method_name.as_str())),
    }
  }

  fn op_class(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let name = self.read_string(slot);
    let class = Value::Class(self.gc.manage(Class::new(name), self));
    self.push(class);
    Ok(ip + 2)
  }

  fn op_get_super(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let name = self.read_string(slot);
    let super_class = self.pop().to_class();

    self.bind_method(super_class, name, ip + 2)
  }

  fn op_inherit(&mut self, ip: usize) -> InterpretResult {
    let mut class = self.peek(0).to_class();

    match self.peek(1) {
      Value::Class(super_class) => {
        super_class.methods.iter().for_each(|(key, value)| {
          class.methods.entry(*key).or_insert_with(|| *value);
        });

        class.init = class.init.or(super_class.init);

        self.pop();
        Ok(ip + 1)
      }
      _ => self.runtime_error("Superclass must be a class."),
    }
  }

  fn op_loop(&mut self, ip: usize) -> InterpretResult {
    Ok(ip + 3 - self.read_short(ip + 1) as usize)
  }

  fn op_jump_if_not_false(&mut self, ip: usize) -> InterpretResult {
    let jump = self.read_short(ip + 1);
    if is_falsey(self.peek(0)) {
      return Ok(ip + 3 + jump as usize);
    }

    Ok(ip + 3)
  }

  fn op_jump(&mut self, ip: usize) -> InterpretResult {
    let jump = self.read_short(ip + 1);
    Ok(ip + 3 + jump as usize)
  }

  fn op_define_global(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let name = self.read_string(slot);
    let global = self.pop();
    self.globals.insert(name, global);
    Ok(ip + 2)
  }

  fn op_set_global(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let string = self.read_string(slot);

    if self.globals.insert(string, self.peek(0)).is_none() {
      self.globals.remove_entry(&string);
      return self.runtime_error(&format!("Undefined variable {}", string.as_str()));
    }

    Ok(ip + 2)
  }

  fn op_set_local(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let copy = self.peek(0);
    let slots = self.current_frame().slots;
    self.stack[slots + slot as usize] = copy;

    Ok(ip + 2)
  }

  fn op_set_property(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let mut value = self.peek(1);
    let name = self.read_string(slot);

    if let Value::Instance(ref mut instance) = value {
      instance.fields.insert(name, self.peek(0));

      let popped = self.pop();
      self.pop();
      self.push(popped);

      return Ok(ip + 2);
    }

    self.runtime_error("Only instances have fields.")
  }

  fn op_set_upvalue(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let value = self.peek(0);
    let upvalue = &mut self.current_mut_frame().closure.upvalues[slot as usize];

    let open_index = match &mut *upvalue.to_upvalue() {
      Upvalue::Open(index) => Some(*index),
      Upvalue::Closed(store) => {
        mem::replace(&mut **store, value);
        None
      }
    };

    if let Some(index) = open_index {
      self.stack[index] = value;
    }

    Ok(ip + 2)
  }

  fn op_get_global(&mut self, ip: usize) -> InterpretResult {
    let store_index = self.read_byte(ip + 1);
    let string = self.read_string(store_index);

    match self.globals.get(&string) {
      Some(gbl) => {
        let copy = *gbl;
        self.push(copy);
        Ok(ip + 2)
      }
      None => self.runtime_error(&format!("Undefined variable {}", string.as_str())),
    }
  }

  fn op_get_local(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let slots = self.current_frame().slots;
    let copy = self.get_val(slots + slot as usize);
    self.push(copy);
    Ok(ip + 2)
  }

  fn op_get_upvalue(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let upvalue_value = &self.current_frame().closure.upvalues[slot as usize];

    let value = match &*upvalue_value.to_upvalue() {
      Upvalue::Open(index) => self.stack[*index],
      Upvalue::Closed(store) => **store,
    };

    self.push(value);
    Ok(ip + 2)
  }

  fn op_get_property(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let value = self.peek(0);

    if let Value::Instance(instance) = value {
      let name = self.read_string(slot);

      return match instance.fields.get(&name) {
        Some(value) => {
          self.pop();
          self.push(*value);
          Ok(ip + 2)
        }
        None => self.bind_method(instance.class, name, ip + 2),
      };
    }

    self.runtime_error("Only instances have properties.")
  }

  fn op_return(&mut self, _: usize) -> InterpretResult {
    let result = self.pop();
    self.close_upvalues(self.current_frame().slots);
    self.frame_count -= 1;

    if self.frame_count == 0 {
      self.pop();
      return Err(Interpret::Ok);
    }

    self.stack_top = self.frames[self.frame_count].slots;
    let return_ip = Ok(self.current_frame().ip);
    self.current_fun = self.current_frame().closure.fun;
    self.push(result);

    return_ip
  }

  fn op_negate(&mut self, ip: usize) -> InterpretResult {
    match self.pop() {
      Value::Number(num) => {
        self.push(Value::Number(-num));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operand must be a number."),
    }
  }

  fn op_not(&mut self, ip: usize) -> InterpretResult {
    let value = self.pop();
    self.push(Value::Bool(is_falsey(value)));
    Ok(ip + 1)
  }

  fn op_add(&mut self, ip: usize) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::String(right), Value::String(left)) => {
        let result = format!("{}{}", left.as_str(), right.as_str());
        let string = self.gc.manage_str(result, self);
        self.push(Value::String(string));
        Ok(ip + 1)
      }
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left + right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be two numbers or two strings."),
    }
  }

  fn op_sub(&mut self, ip: usize) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left - right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_mul(&mut self, ip: usize) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left * right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_div(&mut self, ip: usize) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Number(left / right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_less(&mut self, ip: usize) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Bool(left < right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_greater(&mut self, ip: usize) -> InterpretResult {
    match (self.pop(), self.pop()) {
      (Value::Number(right), Value::Number(left)) => {
        self.push(Value::Bool(left > right));
        Ok(ip + 1)
      }
      _ => self.runtime_error("Operands must be numbers."),
    }
  }

  fn op_equal(&mut self, ip: usize) -> InterpretResult {
    let right = self.pop();
    let left = self.pop();

    self.push(Value::Bool(left == right));
    Ok(ip + 1)
  }

  fn op_method(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let name = self.read_string(slot);

    match (self.peek(1), self.peek(0)) {
      (Value::Class(ref mut class), Value::Closure(method)) => {
        if *name == self.special_strings.init {
          class.init = Some(method);
        }
        class.methods.insert(name, method);
      }
      _ => panic!("Internal spacelox error. stack invalid for op_method"),
    }

    self.pop();
    Ok(ip + 2)
  }

  fn op_closure(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let fun = self.read_constant(slot).to_fun();
    let mut closure = Closure::new(fun);
    let mut current_ip = ip + 2;

    for _ in 0..fun.upvalue_count {
      let upvalue_index: UpvalueIndex = unsafe { mem::transmute(self.read_short(current_ip)) };

      match upvalue_index {
        UpvalueIndex::Local(index) => {
          let total_index = self.current_frame().slots + index as usize;
          closure.upvalues.push(self.capture_upvalue(total_index));
        }
        UpvalueIndex::Upvalue(upvalue) => {
          let upvalue = &self.current_frame().closure.upvalues[upvalue as usize];
          closure.upvalues.push(*upvalue);
        }
      }

      current_ip = current_ip + 2;
    }

    let closure = Value::Closure(self.gc.manage(closure, self));
    self.push(closure);
    Ok(current_ip)
  }

  fn capture_upvalue(&mut self, local_index: usize) -> Value {
    let closest_upvalue = self
      .open_upvalues
      .iter()
      .rev()
      .find(|upvalue| match ***upvalue {
        Upvalue::Open(index) => index <= local_index,
        Upvalue::Closed(_) => panic!("Encountered closed upvalue!"),
      });

    if let Some(upvalue) = closest_upvalue {
      if let Upvalue::Open(index) = **upvalue {
        if index == local_index {
          return Value::Upvalue(*upvalue);
        }
      }
    }

    let created_upvalue = self.gc.manage(Upvalue::Open(local_index), self);
    self.open_upvalues.push(created_upvalue);

    Value::Upvalue(created_upvalue)
  }

  fn op_close_upvalue(&mut self, ip: usize) -> InterpretResult {
    self.close_upvalues(self.stack_top - 1);
    self.pop();
    Ok(ip + 1)
  }

  fn close_upvalues(&mut self, last_index: usize) {
    for upvalue in self.open_upvalues.iter_mut().rev() {
      let index = match **upvalue {
        Upvalue::Open(index) => index,
        Upvalue::Closed(_) => panic!("Unexpected closed upvalue"),
      };

      if index < last_index {
        break;
      }

      upvalue.hoist(self.stack);
    }

    self.open_upvalues.retain(|upvalue| upvalue.is_open())
  }

  fn op_print(&mut self, ip: usize) -> InterpretResult {
    println!("{}", self.pop());
    Ok(ip + 1)
  }

  fn op_constant(&mut self, ip: usize) -> InterpretResult {
    let slot = self.read_byte(ip + 1);
    let constant = self.read_constant(slot);
    self.push(constant);
    Ok(ip + 2)
  }

  #[cfg(feature = "debug")]
  fn print_debug(&self, ip: usize, last_ip: usize) {
    print!("Stack:        ");
    // print!("          ");
    for i in 1..self.stack_top {
      print!("[ {} ]", self.get_val(i));
    }
    println!();

    #[cfg(feature = "debug_upvalue")]
    {
      print!("Open UpVal:   ");
      self
        .open_upvalues
        .iter()
        .for_each(|upvalue| match &**upvalue {
          Upvalue::Open(index) => {
            print!("[ stack {} ]", self.get_val(*index));
          }
          Upvalue::Closed(closed) => {
            print!("[ heap {} ]", closed);
          }
        });
      println!();
    }

    disassemble_instruction(self.current_fun.chunk, ip, last_ip);
  }

  /// Report a known spacelox runtime error to the user
  fn runtime_error(&mut self, message: &str) -> InterpretResult {
    self.error(message);
    Err(Interpret::RuntimeError)
  }

  /// Print an error message and the current call stack to the user
  fn error(&mut self, message: &str) {
    eprintln!("{}", message);
    eprintln!("");

    for frame in self.frames[0..self.frame_count].iter().rev() {
      let closure = &frame.closure;
      let location = match &closure.fun.name {
        Some(name) => format!("{}()", name),
        None => "script".to_string(),
      };

      eprintln!(
        "[line {}] in {}",
        closure.fun.chunk.get_line(frame.ip),
        location
      );
    }

    self.reset_stack();
  }
}

impl<'a> Trace for VmExecutor<'a> {
  fn trace(&self, mark: &mut dyn FnMut(Managed<dyn Manage>)) -> bool {
    do_if_some(self.script.get_dyn_managed(), |obj| mark(obj));

    self.stack[0..self.stack_top].iter().for_each(|value| {
      do_if_some(value.get_dyn_managed(), |obj| mark(obj));
    });

    self.frames[0..self.frame_count]
      .iter()
      .for_each(|frame| mark(frame.closure.clone_dyn()));

    self
      .open_upvalues
      .iter()
      .for_each(|upvalue| mark(upvalue.clone_dyn()));

    self.globals.iter().for_each(|(key, val)| {
      mark(key.clone_dyn());
      do_if_some(val.get_dyn_managed(), |obj| mark(obj));
    });

    true
  }
}

/// Is the provided `value` falsey according to spacelox rules
#[inline]
fn is_falsey(value: Value) -> bool {
  match value {
    Value::Nil => true,
    Value::Bool(b) => !b,
    _ => false,
  }
}
