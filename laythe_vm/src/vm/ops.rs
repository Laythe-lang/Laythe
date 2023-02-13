use super::{source_loader::ImportResult, Signal, Vm};
use crate::{byte_code::CaptureIndex, constants::MAX_FRAME_SIZE};
use laythe_core::{
  captures::Captures,
  hooks::{GcContext, GcHooks, Hooks},
  if_let_obj,
  managed::{Array, Gc, GcObj, GcStr},
  match_obj,
  module::Import,
  object::{
    Channel, Class, Closure, Fun, List, LyBox, Map, Method, Native, NativeMeta, ObjectKind,
    ReceiveResult, SendResult,
  },
  signature::{ArityError, Environment, ParameterKind, SignatureError},
  to_obj_kind,
  utils::is_falsey,
  val,
  value::{Value, VALUE_NIL, VALUE_TRUE},
  Call, LyError,
};
use laythe_core::{managed::GcObject, object::Fiber};
use std::{cmp::Ordering, mem};

impl Vm {
  /// push a literal value onto the stack
  pub(super) unsafe fn op_literal(&mut self, value: Value) -> Signal {
    self.fiber.push(value);
    Signal::Ok
  }

  /// drop a value off the stack
  pub(super) unsafe fn op_drop(&mut self) -> Signal {
    self.fiber.drop();
    Signal::Ok
  }

  /// drop a value off the stack
  pub(super) unsafe fn op_drop_n(&mut self) -> Signal {
    let count = self.read_byte() as usize;
    self.fiber.drop_n(count);
    Signal::Ok
  }

  /// duplicate the top value on the stack
  pub(super) unsafe fn op_dup(&mut self) -> Signal {
    let top = self.fiber.peek(0);
    self.fiber.push(top);
    Signal::Ok
  }

  /// create a list from a list literal
  pub(super) unsafe fn op_list(&mut self) -> Signal {
    let arg_count = self.read_short() as usize;

    let args = self.fiber.stack_slice(arg_count);
    let list = val!(self.manage_obj(List::from(args)));
    self.fiber.drop_n(arg_count);
    self.fiber.push(list);

    Signal::Ok
  }

  /// create a list from a list literal
  pub(super) unsafe fn op_tuple(&mut self) -> Signal {
    let arg_count = self.read_short() as usize;

    let args = self.fiber.stack_slice(arg_count);
    let list = val!(self.manage_tuple(args));
    self.fiber.drop_n(arg_count);
    self.fiber.push(list);

    Signal::Ok
  }

  /// create a map from a map literal
  pub(super) unsafe fn op_map(&mut self) -> Signal {
    let arg_count = self.read_short() as usize;
    let mut map = self.manage_obj(Map::with_capacity(arg_count));

    for i in 0..arg_count {
      let key = self.fiber.peek(i * 2 + 1);
      let value = self.fiber.peek(i * 2);

      map.insert(key, value);
    }

    self.fiber.drop_n(arg_count * 2);
    self.fiber.push(val!(map));

    Signal::Ok
  }

  /// launch a function on a separate fiber
  pub(super) unsafe fn op_launch(&mut self) -> Signal {
    let arg_count = self.read_byte();
    let callee = self.fiber.peek(arg_count as usize);
    let frame_count = self.fiber.frames().len();
    let current_fun = self.current_fun;

    // call function as normal to get setup
    let signal = self.resolve_call(callee, arg_count);
    match signal {
      Signal::Ok => (),
      Signal::OkReturn => (),
      _ => return signal,
    };

    let hooks = GcHooks::new(self);
    let fiber = self.fiber;

    // call fiber split which will peel off the last frame if it's above the previous
    // water mark
    if let Some(new_fiber) = Fiber::split(fiber, &hooks, frame_count, arg_count as usize) {
      // put the fiber in the queue
      self.fiber_queue.push_back(new_fiber);
      self.current_fun = current_fun;
      self.load_ip();
    }

    Signal::Ok
  }

  /// create synchronous channel
  pub(super) unsafe fn op_channel(&mut self) -> Signal {
    let hooks = GcHooks::new(self);
    let channel = self.manage_obj(Channel::sync(&hooks));
    self.fiber.push(val!(channel));

    Signal::Ok
  }

  /// create a buffered channel
  pub(super) unsafe fn op_buffered_channel(&mut self) -> Signal {
    let capacity = self.fiber.pop();

    if !capacity.is_num() {
      return self.runtime_error(
        self.builtin.errors.type_,
        &format!(
          "function \"chan\"'s parameter \"capacity\" required a number but received a {}.",
          ParameterKind::from(capacity)
        ),
      );
    }

    let capacity = capacity.to_num();
    if capacity.fract() != 0.0 || capacity < 1.0 {
      return self.runtime_error(
        self.builtin.errors.type_,
        "buffer must be an positive integer.",
      );
    }

    let hooks = GcHooks::new(self);
    let channel = self.manage_obj(Channel::with_capacity(&hooks, capacity as usize));
    self.fiber.push(val!(channel));

    Signal::Ok
  }

  /// receive from a channel
  pub(super) unsafe fn op_receive(&mut self) -> Signal {
    let channel = self.fiber.pop();

    if_let_obj!(ObjectKind::Channel(mut channel) = (channel) {
      self.fiber.add_used_channel(channel);

      match channel.receive(self.fiber) {
        ReceiveResult::Ok(value) => {
          self.fiber.push(value);
          Signal::Ok
        }
        ReceiveResult::NoReceiveAccess => {
          self.runtime_error(self.builtin.errors.value, "todo no read access")
        }
        ReceiveResult::EmptyBlock(fiber) => {
          if let Some(mut fiber) = fiber.or_else(|| self.fiber.get_runnable())  {
            fiber.unblock();
            self.fiber_queue.push_back(fiber);
          }

          self.fiber.push(val!(channel));
          self.update_ip(-1);
          self.fiber.block();
          Signal::ContextSwitch
        },
        ReceiveResult::Empty(fiber) => {
          if let Some(mut fiber) = fiber.or_else(|| self.fiber.get_runnable()) {
            fiber.unblock();
            self.fiber_queue.push_back(fiber);
          }

          self.fiber.push(val!(channel));
          self.update_ip(-1);
          self.fiber.sleep();
          Signal::ContextSwitch
        },
        ReceiveResult::Closed => {
          self.fiber.push(VALUE_NIL);
          Signal::Ok
        }
      }
    } else {
      self.runtime_error(
        self.builtin.errors.type_,
        "Can only drain from a channel.",
      )
    })
  }

  /// send to a channel
  pub(super) unsafe fn op_send(&mut self) -> Signal {
    let channel = self.fiber.pop();
    let value = self.fiber.peek(0);

    if_let_obj!(ObjectKind::Channel(mut channel) = (channel) {
      self.fiber.add_used_channel(channel);

      match channel.send(self.fiber, value) {
        SendResult::Ok => Signal::Ok,
        SendResult::NoSendAccess => self.runtime_error(
          self.builtin.errors.runtime,
          "Attempted to send into a receive only channel.",
        ),
        SendResult::FullBlock(fiber) => {
          // if channel has a waiter put into
          // the fiber queue
          if let Some(mut fiber) = fiber.or_else(|| self.fiber.get_runnable()) {
            fiber.unblock();
            self.fiber_queue.push_back(fiber);
          }

          // the channel enqueued the result but is now full so we need to switch
          self.fiber.block();
          Signal::ContextSwitch
        }
        SendResult::Full(fiber) => {
          // if channel has a waiter put into
          // the fiber queue
          if let Some(mut fiber) = fiber.or_else(|| self.fiber.get_runnable()) {
            fiber.unblock();
            self.fiber_queue.push_back(fiber);
          }

          // replace the channel on the stack and rewind the
          // ip to the beginning of this instruction
          self.fiber.push(val!(channel));
          self.update_ip(-1);
          self.fiber.sleep();
          Signal::ContextSwitch
        }
        SendResult::Closed => self.runtime_error(
          self.builtin.errors.runtime,
          "Attempted to send into a closed channel.",
        ),
      }
    } else {
      self.runtime_error(self.builtin.errors.runtime, "Attempted to send into a non channel.")
    })
  }

  /// create a map from a map literal
  pub(super) unsafe fn op_interpolate(&mut self) -> Signal {
    let arg_count = self.read_short() as usize;
    let args = self.fiber.stack_slice(arg_count);

    let mut length: usize = 0;
    for arg in args {
      length += arg.to_obj().to_str().len();
    }

    let mut buffers = String::with_capacity(length);
    for arg in args {
      buffers.push_str(&arg.to_obj().to_str())
    }

    self.fiber.drop_n(arg_count);
    let interpolated = val!(self.manage_str(buffers));
    self.fiber.push(interpolated);

    Signal::Ok
  }

  /// move an iterator to the next element
  pub(super) unsafe fn op_iter_next(&mut self) -> Signal {
    let receiver = self.fiber.peek(0);

    if_let_obj!(ObjectKind::Enumerator(mut enumerator) = (receiver) {
      self.update_ip(2);
      match enumerator.next(&mut Hooks::new(self)) {
        Call::Ok(value) => {
          self.fiber.peek_set(0, value);
          Signal::Ok
        },
        Call::Err(LyError::Err(error)) => self.set_error(error),
        Call::Err(LyError::Exit(code)) => self.set_exit(code),
      }
    } else {
      let constant = self.read_short();
      let method_name = self.read_string(constant);
      self.invoke(receiver, method_name, 0)
    })
  }

  /// get the current value from an iterator
  pub(super) unsafe fn op_iter_current(&mut self) -> Signal {
    let receiver = self.fiber.peek(0);

    if_let_obj!(ObjectKind::Enumerator(enumerator) = (receiver) {
      self.update_ip(2);
      let result = enumerator.current();
      self.fiber.peek_set(0, result);
      Signal::Ok
    } else {
      let constant = self.read_short();
      let method_name = self.read_string(constant);
      self.invoke(receiver, method_name, 0)
    })
  }

  /// call a function or method
  pub(super) unsafe fn op_call(&mut self) -> Signal {
    let arg_count = self.read_byte();
    let callee = self.fiber.peek(arg_count as usize);

    self.resolve_call(callee, arg_count)
  }

  /// invoke a method on an instance's class
  pub(super) unsafe fn op_invoke(&mut self) -> Signal {
    let constant = self.read_short();
    let arg_count = self.read_byte();
    let inline_slot = self.read_slot() as usize;

    let method_name = self.read_string(constant);
    let receiver = self.fiber.peek(arg_count as usize);

    let class = self.value_class(receiver);
    match self.inline_cache().get_invoke_cache(inline_slot, class) {
      Some(method) => self.resolve_call(method, arg_count),
      None => {
        if_let_obj!(ObjectKind::Instance(instance) = (receiver) {
          if let Some(field) = instance.get_field(method_name) {
            self.fiber.peek_set(arg_count as usize, *field);
            self.inline_cache_mut().clear_invoke_cache(inline_slot);
            return self.resolve_call(*field, arg_count);
          }
        });

        match class.get_method(&method_name) {
          Some(method) => {
            self
              .inline_cache_mut()
              .set_invoke_cache(inline_slot, class, method);
            self.resolve_call(method, arg_count)
          },
          None => self.runtime_error(
            self.builtin.errors.property,
            &format!(
              "Undefined property {} on class {}.",
              method_name,
              class.name()
            ),
          ),
        }
      },
    }
  }

  /// invoke a method
  unsafe fn invoke(&mut self, receiver: Value, method_name: GcStr, arg_count: u8) -> Signal {
    if_let_obj!(ObjectKind::Instance(instance) = (receiver) {
      if let Some(field) = instance.get_field(method_name) {
        self.fiber.peek_set(arg_count as usize, *field);
        return self.resolve_call(*field, arg_count);
      }
    });

    let class = self.value_class(receiver);
    self.invoke_from_class(class, method_name, arg_count)
  }

  /// Invoke a method on a instance's super class
  pub(super) unsafe fn op_super_invoke(&mut self) -> Signal {
    let constant = self.read_short();
    let arg_count = self.read_byte();
    let inline_slot = self.read_slot() as usize;

    let method_name = self.read_string(constant);
    let super_class = self.fiber.pop().to_obj().to_class();

    match self
      .inline_cache()
      .get_invoke_cache(inline_slot, super_class)
    {
      Some(method) => self.resolve_call(method, arg_count),
      None => match super_class.get_method(&method_name) {
        Some(method) => {
          self
            .inline_cache_mut()
            .set_invoke_cache(inline_slot, super_class, method);
          self.resolve_call(method, arg_count)
        },
        None => self.runtime_error(
          self.builtin.errors.property,
          &format!(
            "Undefined property {} on class {}.",
            method_name,
            super_class.name()
          ),
        ),
      },
    }
  }

  /// Generate a new class
  pub(super) unsafe fn op_class(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = val!(self.manage_obj(Class::bare(name)));
    self.fiber.push(class);
    Signal::Ok
  }

  pub(super) unsafe fn op_inherit(&mut self) -> Signal {
    let super_class = self.fiber.peek(1);

    if !super_class.is_obj() {
      return self.runtime_error(self.builtin.errors.runtime, "Superclass must be a class.");
    }

    let super_class = super_class.to_obj();
    let super_class = match_obj!((&super_class) {
      ObjectKind::Class(class) => {
        class
      },
      ObjectKind::LyBox(ly_box) => {
        if ly_box.value.is_obj_kind(ObjectKind::Class) {
          ly_box.value.to_obj().to_class()
        } else {
          return self.runtime_error(self.builtin.errors.runtime, "Superclass must be a class.");
        }
      },
      _ => {
        return self.runtime_error(self.builtin.errors.runtime, "Superclass must be a class.");
      },
    });

    let hooks = GcHooks::new(self);
    let mut sub_class = self.fiber.peek(0).to_obj().to_class();

    sub_class.inherit(&hooks, super_class);
    sub_class.meta_from_super(&hooks);

    Signal::Ok
  }

  /// Get this classes super class
  pub(super) unsafe fn op_get_super(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);
    let super_class = self.fiber.pop().to_obj().to_class();

    self.bind_method(super_class, name)
  }

  /// Loop by performing an unconditional jump to a new instruction
  pub(super) unsafe fn op_loop(&mut self) -> Signal {
    let jump = self.read_short() as isize;
    self.update_ip(-jump);
    Signal::Ok
  }

  /// Jump if the condition evaluates to a falsey value
  pub(super) unsafe fn op_jump_if_false(&mut self) -> Signal {
    let jump = self.read_short();
    if is_falsey(self.fiber.peek(0)) {
      self.update_ip(jump as isize);
    }
    self.fiber.drop();

    Signal::Ok
  }

  /// Unconditionally jump to some other instruction
  pub(super) unsafe fn op_jump(&mut self) -> Signal {
    let jump = self.read_short();
    self.update_ip(jump as isize);
    Signal::Ok
  }

  pub(super) unsafe fn op_push_handler(&mut self) -> Signal {
    let slot_depth = self.read_short() as usize;
    let jump = self.read_short() as usize;
    let start = &self.fiber.fun().chunk().instructions()[0] as *const u8;
    let offset = self.ip.offset_from(start) as usize + jump;
    self.fiber.push_exception_handler(offset, slot_depth);
    Signal::Ok
  }

  pub(super) unsafe fn op_pop_handler(&mut self) -> Signal {
    self.fiber.pop_exception_handler();
    Signal::Ok
  }

  /// Define a global variable
  pub(super) unsafe fn op_define_global(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);
    let global = self.fiber.pop();
    let mut current_module = self.current_fun.module();
    match current_module.insert_symbol(name, global) {
      Ok(_) => Signal::Ok,
      Err(_) => Signal::Ok,
    }
  }

  /// Box a local
  pub(super) unsafe fn op_box(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let slot = self.stack_start().offset(slot);
    let local = *slot;
    *slot = val!(self.manage_obj(LyBox::new(local)));
    Signal::Ok
  }

  /// Create a new empty box
  pub(super) unsafe fn op_empty_box(&mut self) -> Signal {
    let value = val!(self.manage_obj(LyBox::default()));
    self.fiber.push(value);

    Signal::Ok
  }

  /// Move the top of the stack into a box
  pub(super) unsafe fn op_fill_box(&mut self) -> Signal {
    let value = self.fiber.pop();
    self.fiber.peek(0).to_obj().to_box().value = value;

    Signal::Ok
  }

  pub(super) unsafe fn op_set_global(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);
    let value = self.fiber.peek(0);

    let mut current_module = self.current_fun.module();
    if current_module.set_symbol(name, value).is_err() {
      return self.runtime_error(
        self.builtin.errors.property,
        &format!("Undefined variable {name}"),
      );
    }

    Signal::Ok
  }

  pub(super) unsafe fn op_set_local(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let copy = self.fiber.peek(0);

    *self.stack_start().offset(slot) = copy;

    Signal::Ok
  }

  pub(super) unsafe fn op_set_box(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let copy = self.fiber.peek(0);

    (*self.stack_start().offset(slot)).to_obj().to_box().value = copy;

    Signal::Ok
  }

  pub(super) unsafe fn op_set_capture(&mut self) -> Signal {
    let slot = self.read_byte();
    let value = self.fiber.peek(0);
    self
      .fiber
      .captures()
      .set_capture_value(slot as usize, value);

    Signal::Ok
  }

  pub(super) unsafe fn op_set_property(&mut self) -> Signal {
    let slot = self.read_short();
    let instance = self.fiber.peek(1);
    let name = self.read_string(slot);
    let inline_slot = self.read_slot() as usize;

    if_let_obj!(ObjectKind::Instance(mut instance) = (instance) {
      let class = instance.class();

      match self.inline_cache().get_property_cache(inline_slot, class) {
        Some(property_slot) => {
          let value = self.fiber.pop();

          self.fiber.drop();
          self.fiber.push(value);

          instance[property_slot] = value;
          return Signal::Ok;
        },
        None => {
          let property_slot = class.get_field_index(&name);
          let value = self.fiber.pop();

          self.fiber.drop();
          self.fiber.push(value);

          return match property_slot {
            Some(property_slot) => {
              let cache = self.inline_cache_mut();
              cache.set_property_cache(inline_slot, class, property_slot as usize);
              instance[property_slot as usize] = value;
              Signal::Ok
            },
            None => self.runtime_error(
              self.builtin.errors.property,
              &format!("Undefined property {} on class {}.", name, class.name()),
            ),
          };
        },
      }
    });

    self.runtime_error(
      self.builtin.errors.runtime,
      "Only instances have settable fields.",
    )
  }

  pub(super) unsafe fn op_get_global(&mut self) -> Signal {
    let store_index = self.read_short();
    let string = self.read_string(store_index);

    match self.current_fun.module().get_symbol(string) {
      Some(gbl) => {
        self.fiber.push(gbl);
        Signal::Ok
      },
      None => self.runtime_error(
        self.builtin.errors.runtime,
        &format!("Undefined variable {string}"),
      ),
    }
  }

  pub(super) unsafe fn op_get_local(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let local = *self.stack_start().offset(slot);
    self.fiber.push(local);
    Signal::Ok
  }

  pub(super) unsafe fn op_get_box(&mut self) -> Signal {
    let slot = self.read_byte() as isize;
    let local = *self.stack_start().offset(slot);
    let local = local.to_obj().to_box().value;
    self.fiber.push(local);
    Signal::Ok
  }

  pub(super) unsafe fn op_get_capture(&mut self) -> Signal {
    let slot = self.read_byte();

    let upvalue = self.fiber.captures().get_capture_value(slot as usize);
    self.fiber.push(upvalue);

    Signal::Ok
  }

  pub(super) unsafe fn op_get_property(&mut self) -> Signal {
    let slot = self.read_short();
    let value = self.fiber.peek(0);
    let name = self.read_string(slot);
    let inline_slot = self.read_slot() as usize;

    if_let_obj!(ObjectKind::Instance(instance) = (value) {
      let class = instance.class();
      match self.inline_cache().get_property_cache(inline_slot, class) {
        Some(property_slot) => {
          self.fiber.peek_set(0, instance[property_slot]);
          return Signal::Ok;
        },
        None => {
          if let Some(property_slot) = class.get_field_index(&name) {
            self
              .inline_cache_mut()
              .set_property_cache(inline_slot, class, property_slot as usize);

            self.fiber.peek_set(0, instance[property_slot as usize]);
            return Signal::Ok;
          }
        },
      }
    });

    let class = self.value_class(value);
    let cache = self.inline_cache_mut();
    cache.clear_property_cache(inline_slot);
    self.bind_method(class, name)
  }

  pub(super) unsafe fn op_import(&mut self) -> Signal {
    let index_path = self.read_short();
    let path = self.read_constant(index_path).to_obj().to_list();

    let path_segments = self.extract_import_path(path);

    // check if fully resolved module has already been loaded
    let resolved = self.full_import_path(&path_segments);
    self.push_root(resolved);

    if let Some(module) = self.module_cache.get(&resolved) {
      let imported = module.module_instance(&GcHooks::new(self));
      self.fiber.push(val!(imported));
      self.pop_roots(1);
      return Signal::Ok;
    }

    let import = self.build_import(&path_segments);
    self.push_root(import);

    let result = match self.import_module(import) {
      ImportResult::Loaded(module) => {
        self.module_cache.insert(resolved, module);
        let module_instance = val!(module.module_instance(&GcHooks::new(self)));

        self.fiber.push(module_instance);
        Signal::Ok
      },
      ImportResult::Compiled(fun) => {
        self.update_ip(-3);
        self.fiber.sleep();

        let import_fiber = match Fiber::new(Some(self.fiber), fun, self.capture_stub) {
          Ok(fiber) => fiber,
          Err(_) => self.internal_error("Importing fiber"),
        };
        let import_fiber = self.manage_obj(import_fiber);

        self.fiber_queue.push_back(import_fiber);
        Signal::ContextSwitch
      },
      ImportResult::NotFound => {
        let env = self.io.env();
        let resolved_path = env.current_dir().unwrap();

        self.runtime_error(
          self.builtin.errors.import,
          &format!(
            "Module {} not found in directory {:?}",
            &import, &resolved_path
          ),
        )
      },
      ImportResult::CompileError => Signal::Exit,
    };

    self.pop_roots(2);
    result
  }

  pub(super) unsafe fn op_import_symbol(&mut self) -> Signal {
    let index_path = self.read_short();
    let index_name = self.read_short();
    let path = self.read_constant(index_path).to_obj().to_list();
    let symbol_name = self.read_string(index_name);

    let path_segments = self.extract_import_path(path);

    // check if fully resolved module has already been loaded
    let resolved = self.full_import_path(&path_segments);
    self.push_root(resolved);

    if let Some(module) = self.module_cache.get(&resolved).cloned() {
      let signal = match module.get_exported_symbol(symbol_name) {
        Some(symbol) => {
          self.fiber.push(symbol);
          Signal::Ok
        },
        None => self.runtime_error(
          self.builtin.errors.import,
          &format!(
            "Symbol {} not exported from module {}",
            symbol_name,
            &module.name()
          ),
        ),
      };

      self.pop_roots(1);
      return signal;
    }

    let import = self.build_import(&path_segments);
    self.push_root(import);

    let result = match self.import_module(import) {
      ImportResult::Loaded(module) => {
        self.module_cache.insert(resolved, module);

        match module.get_exported_symbol(symbol_name) {
          Some(symbol) => {
            self.fiber.push(symbol);
            Signal::Ok
          },
          None => self.runtime_error(
            self.builtin.errors.import,
            &format!(
              "Symbol {} not exported from module {}",
              symbol_name,
              &module.name()
            ),
          ),
        }
      },
      ImportResult::Compiled(fun) => {
        self.update_ip(-5);
        self.fiber.sleep();

        let import_fiber = match Fiber::new(Some(self.fiber), fun, self.capture_stub) {
          Ok(fiber) => fiber,
          Err(_) => self.internal_error("Importing fiber"),
        };
        let import_fiber = self.manage_obj(import_fiber);

        self.fiber_queue.push_back(import_fiber);
        Signal::ContextSwitch
      },
      ImportResult::NotFound => {
        let env = self.io.env();
        let resolved_path = env.current_dir().unwrap();

        self.runtime_error(
          self.builtin.errors.import,
          &format!(
            "Module {} not found in directory {:?}",
            &import, &resolved_path
          ),
        )
      },
      ImportResult::CompileError => Signal::Exit,
    };

    self.pop_roots(2);
    result
  }

  fn extract_import_path(&mut self, path: GcObj<List<Value>>) -> List<GcStr> {
    let mut path_segments: List<GcStr> = List::with_capacity(path.len());
    path_segments.extend(path.iter().map(|segment| segment.to_obj().to_str()));
    path_segments
  }

  fn full_import_path(&mut self, path_segments: &[GcStr]) -> GcStr {
    let mut buffer = String::new();
    for segment in &path_segments[..path_segments.len() - 1] {
      buffer.push_str(segment);
      buffer.push('/');
    }

    buffer.push_str(&path_segments[path_segments.len() - 1]);

    self.manage_str(buffer)
  }

  fn build_import(&mut self, path_segments: &[GcStr]) -> Gc<Import> {
    match path_segments.split_first() {
      Some((package, path)) => {
        // generate a new import object
        let path = self.manage(path);
        self.manage(Import::new(*package, path))
      },
      None => {
        // generate a new import object
        let path: Array<GcStr> = self.manage(&[] as &[GcStr]);
        self.manage(Import::new(path_segments[0], path))
      },
    }
  }

  pub(super) unsafe fn op_export(&mut self) -> Signal {
    let index = self.read_short();
    let name = self.read_string(index);
    let mut current_module = self.current_fun.module();

    match current_module.export_symbol(name) {
      Ok(_) => Signal::Ok,
      Err(error) => self.runtime_error(self.builtin.errors.export, &error.to_string()),
    }
  }

  /// return from a laythe function placing the result on top of the stack
  pub(super) unsafe fn op_return(&mut self) -> Signal {
    // get the function result and pop frame
    let result = self.fiber.pop();

    // pop a frame from the call stack return signal if provided
    if let Some(signal) = self.pop_frame() {
      return signal;
    }

    // push result onto stack
    self.fiber.push(result);
    Signal::OkReturn
  }

  pub(super) unsafe fn op_negate(&mut self) -> Signal {
    let pop = self.fiber.pop();

    if pop.is_num() {
      self.fiber.push(val!(-pop.to_num()));
      Signal::Ok
    } else {
      self.runtime_error(self.builtin.errors.runtime, "Operand must be a number.")
    }
  }

  pub(super) unsafe fn op_not(&mut self) -> Signal {
    let value = self.fiber.pop();
    self.fiber.push(val!(is_falsey(value)));
    Signal::Ok
  }

  pub(super) unsafe fn op_add(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() + right.to_num()));
      Signal::Ok
    } else if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      let left = left.to_obj().to_str();
      let right = right.to_obj().to_str();

      let mut buffer = String::with_capacity(left.len() + right.len());
      buffer.push_str(&left);
      buffer.push_str(&right);

      let string = self.manage_str(buffer);
      self.fiber.push(val!(string));
      Signal::Ok
    } else {
      self.runtime_error(
        self.builtin.errors.runtime,
        "Operands must be two numbers or two strings.",
      )
    }
  }

  pub(super) unsafe fn op_sub(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() - right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  pub(super) unsafe fn op_mul(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() * right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  pub(super) unsafe fn op_div(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() / right.to_num()));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  pub(super) unsafe fn op_and(&mut self) -> Signal {
    let jump = self.read_short();
    let left = self.fiber.peek(0);

    if is_falsey(left) {
      self.update_ip(jump as isize);
    } else {
      self.fiber.drop();
    }

    Signal::Ok
  }

  pub(super) unsafe fn op_or(&mut self) -> Signal {
    let jump = self.read_short();
    let left = self.fiber.peek(0);

    if is_falsey(left) {
      self.fiber.drop();
    } else {
      self.update_ip(jump as isize);
    }

    Signal::Ok
  }

  pub(super) unsafe fn op_less(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() < right.to_num()));
      return Signal::Ok;
    }

    if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      self.fiber.push(val!(
        (*left.to_obj().to_str()).cmp(&right.to_obj().to_str()) == Ordering::Less
      ));
      return Signal::Ok;
    }

    self.runtime_error(self.builtin.errors.runtime, "Operands must be numbers.")
  }

  pub(super) unsafe fn op_less_equal(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() <= right.to_num()));
      return Signal::Ok;
    }

    if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      if left == right {
        self.fiber.push(VALUE_TRUE);
      } else {
        self.fiber.push(val!(
          (*left.to_obj().to_str()).cmp(&right.to_obj().to_str()) == Ordering::Less
        ));
      }

      return Signal::Ok;
    }

    self.runtime_error(
      self.builtin.errors.runtime,
      "Operands must be numbers or strings.",
    )
  }

  pub(super) unsafe fn op_greater(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() > right.to_num()));
      return Signal::Ok;
    }

    if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      self.fiber.push(val!(
        (*left.to_obj().to_str()).cmp(&right.to_obj().to_str()) == Ordering::Greater
      ));
      return Signal::Ok;
    }

    self.runtime_error(
      self.builtin.errors.runtime,
      "Operands must be numbers or strings.",
    )
  }

  pub(super) unsafe fn op_greater_equal(&mut self) -> Signal {
    let (right, left) = (self.fiber.pop(), self.fiber.pop());

    if right.is_num() && left.is_num() {
      self.fiber.push(val!(left.to_num() >= right.to_num()));
      return Signal::Ok;
    }

    if right.is_obj_kind(ObjectKind::String) && left.is_obj_kind(ObjectKind::String) {
      if left == right {
        self.fiber.push(VALUE_TRUE);
      } else {
        self.fiber.push(val!(
          (*left.to_obj().to_str()).cmp(&right.to_obj().to_str()) == Ordering::Greater
        ));
      }

      return Signal::Ok;
    }

    self.runtime_error(
      self.builtin.errors.runtime,
      "Operands must be numbers or strings.",
    )
  }

  pub(super) unsafe fn op_equal(&mut self) -> Signal {
    let right = self.fiber.pop();
    let left = self.fiber.pop();

    self.fiber.push(val!(left == right));
    Signal::Ok
  }

  pub(super) unsafe fn op_not_equal(&mut self) -> Signal {
    let right = self.fiber.pop();
    let left = self.fiber.pop();

    self.fiber.push(val!(left != right));
    Signal::Ok
  }

  pub(super) unsafe fn op_method(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = self.fiber.peek(1);
    let method = self.fiber.peek(0);

    if class.is_obj_kind(ObjectKind::Class) && method.is_obj_kind(ObjectKind::Closure) {
      class.to_obj().to_class().add_method(name, method);
    } else {
      self.internal_error("Invalid Stack for op_method.");
    }

    self.fiber.drop();
    Signal::Ok
  }

  pub(super) unsafe fn op_field(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = self.fiber.peek(0);

    if_let_obj!(ObjectKind::Class(mut class) = (class) {
      class.add_field(name);
    } else {
      self.internal_error("Invalid Stack for op_method.");
    });

    Signal::Ok
  }

  pub(super) unsafe fn op_static_method(&mut self) -> Signal {
    let slot = self.read_short();
    let name = self.read_string(slot);

    let class = self.fiber.peek(1);
    let method = self.fiber.peek(0);

    if class.is_obj_kind(ObjectKind::Class) && method.is_obj_kind(ObjectKind::Closure) {
      let class = class.to_obj().to_class();

      match class.meta_class() {
        Some(mut meta) => {
          meta.add_method(name, method);
        },
        None => self.internal_error(&format!("{} meta class not set.", class.name())),
      }
    } else {
      self.internal_error("Invalid Stack for op_static_method.");
    }

    self.fiber.drop();
    Signal::Ok
  }

  pub(super) unsafe fn op_closure(&mut self) -> Signal {
    let slot = self.read_short();
    let fun = self.read_constant(slot).to_obj().to_fun();

    let captures = (0..fun.capture_count())
      .map(|_| {
        let capture_index: CaptureIndex = mem::transmute(self.read_short());
        match capture_index {
          CaptureIndex::Local(index) => (*self.fiber.stack_start().offset(index as isize))
            .to_obj()
            .to_box(),
          CaptureIndex::Enclosing(index) => self.fiber.captures().get_capture(index as usize),
        }
      })
      .collect::<Vec<GcObj<LyBox>>>();

    let captures = Captures::new(&GcHooks::new(self), &captures);
    let closure = self.manage_obj(Closure::new(fun, captures));

    self.fiber.push(val!(closure));

    Signal::Ok
  }

  pub(super) unsafe fn op_constant(&mut self) -> Signal {
    let slot = self.read_byte();
    let constant = self.read_constant(slot as u16);
    self.fiber.push(constant);

    Signal::Ok
  }

  pub(super) unsafe fn op_constant_long(&mut self) -> Signal {
    let slot = self.read_short();
    let constant = self.read_constant(slot);
    self.fiber.push(constant);

    Signal::Ok
  }

  /// resolve the current value and call in the appropriate manner
  pub(super) unsafe fn resolve_call(&mut self, callee: Value, arg_count: u8) -> Signal {
    if !callee.is_obj() {
      let class_name = self.value_class(callee).name();
      return self.runtime_error(
        self.builtin.errors.runtime,
        &format!("{class_name} is not callable."),
      );
    }

    match_obj!((&callee.to_obj()) {
      ObjectKind::Closure(closure) => {
        self.call_closure(closure, arg_count)
      },
      ObjectKind::Method(method) => {
        self.call_method(method, arg_count)
      },
      ObjectKind::Native(native) => {
        self.call_native(native, arg_count)
      },
      ObjectKind::Class(class) => {
        self.call_class(class, arg_count)
      },
      ObjectKind::Fun(fun) => {
        self.call(fun, arg_count)
      },
      _ => {
        let class_name = self.value_class(callee).name();
        self.runtime_error(
          self.builtin.errors.runtime,
          &format!("{class_name} is not callable."),
        )
      },
    })
  }

  /// call a class creating a new instance of that class
  unsafe fn call_class(&mut self, class: GcObj<Class>, arg_count: u8) -> Signal {
    let instance = val!(self.manage_instance(class));
    self.fiber.peek_set(arg_count as usize, instance);

    match class.init() {
      Some(init) => self.resolve_call(init, arg_count),
      None => {
        if arg_count != 0 {
          self.runtime_error(
            self.builtin.errors.runtime,
            &format!("Expected 0 arguments but got {arg_count}"),
          )
        } else {
          Signal::Ok
        }
      },
    }
  }

  /// call a native function immediately returning the result
  unsafe fn call_native(&mut self, native: GcObj<Native>, arg_count: u8) -> Signal {
    let meta = native.meta();
    let mut fiber = self.fiber;
    let args = fiber.stack_slice(arg_count as usize);

    // check that the current function is called with the right number of args and types
    if let Some(signal) = self.check_native_arity(meta, args) {
      return signal;
    }

    let this = if meta.is_method {
      Some(fiber.peek(arg_count as usize))
    } else {
      None
    };

    #[cfg(debug_assertions)]
    let roots_before = self.gc().temp_roots();

    match meta.environment {
      Environment::StackLess => match native.call(&mut Hooks::new(self), this, args) {
        Call::Ok(value) => {
          fiber.drop_n(arg_count as usize + 1);
          fiber.push(value);

          #[cfg(debug_assertions)]
          {
            let roots_current = self.gc().temp_roots();
            assert_roots(native, roots_before, roots_current);
          }
          Signal::OkReturn
        },
        Call::Err(LyError::Err(error)) => self.set_error(error),
        Call::Err(LyError::Exit(code)) => self.set_exit(code),
      },
      Environment::Normal => {
        let mut stub = self.native_fun_stubs.pop().unwrap_or_else(|| {
          self.manage_obj(Fun::stub(&GcHooks::new(self), meta.name, self.global))
        });
        stub.set_name(meta.name);
        self.push_root(stub);

        self.pop_roots(1);

        self.push_frame(stub, self.capture_stub, arg_count);

        let result = native.call(&mut Hooks::new(self), this, args);
        self.native_fun_stubs.push(stub);

        match result {
          Call::Ok(value) => {
            self.pop_frame();
            fiber.push(value);

            #[cfg(debug_assertions)]
            {
              let roots_current = self.gc().temp_roots();
              assert_roots(native, roots_before, roots_current);
            }
            Signal::OkReturn
          },
          Call::Err(LyError::Err(error)) => self.set_error(error),
          Call::Err(LyError::Exit(code)) => self.set_exit(code),
        }
      },
    }
  }

  /// call a laythe function setting it as the new call frame
  unsafe fn call_closure(&mut self, closure: GcObj<Closure>, arg_count: u8) -> Signal {
    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(closure.fun(), arg_count) {
      return error;
    }

    // set the current current instruction pointer. check for overflow
    if self.fiber.frames().len() == MAX_FRAME_SIZE {
      return self.runtime_error(self.builtin.errors.runtime, "Stack overflow.");
    }

    self.push_frame(closure.fun(), closure.captures(), arg_count);
    Signal::Ok
  }

  /// call a laythe function setting it as the new call frame
  unsafe fn call(&mut self, fun: GcObj<Fun>, arg_count: u8) -> Signal {
    // check that the current function is called with the right number of args
    if let Some(error) = self.check_arity(fun, arg_count) {
      return error;
    }

    // set the current current instruction pointer. check for overflow
    if self.fiber.frames().len() == MAX_FRAME_SIZE {
      return self.runtime_error(self.builtin.errors.runtime, "Stack overflow.");
    }

    self.push_frame(fun, self.capture_stub, arg_count);
    Signal::Ok
  }

  /// check that the number of args is valid for the function arity
  unsafe fn check_arity(&mut self, fun: GcObj<Fun>, arg_count: u8) -> Option<Signal> {
    match fun.arity().check(arg_count) {
      Ok(_) => None,
      Err(error) => match error {
        ArityError::Fixed(arity) => Some(self.runtime_error(
          self.builtin.errors.runtime,
          &format!(
            "{} expected {} argument(s) but got {}.",
            fun.name(),
            arity,
            arg_count,
          ),
        )),
        ArityError::Variadic(arity) => Some(self.runtime_error(
          self.builtin.errors.runtime,
          &format!(
            "{} expected at least {} argument(s) but got {}.",
            fun.name(),
            arity,
            arg_count,
          ),
        )),
        ArityError::DefaultLow(arity) => Some(self.runtime_error(
          self.builtin.errors.runtime,
          &format!(
            "{} expected at least {} argument(s) but got {}.",
            fun.name(),
            arity,
            arg_count,
          ),
        )),
        ArityError::DefaultHigh(arity) => Some(self.runtime_error(
          self.builtin.errors.runtime,
          &format!(
            "{} expected at most {} argument(s) but got {}.",
            fun.name(),
            arity,
            arg_count,
          ),
        )),
      },
    }
  }

  /// Check the arity of a native functio
  unsafe fn check_native_arity(
    &mut self,
    native_meta: &NativeMeta,
    args: &[Value],
  ) -> Option<Signal> {
    match native_meta.signature.check(args) {
      Ok(()) => None,
      Err(err) => {
        let callable_type = if native_meta.is_method {
          "method"
        } else {
          "function"
        };

        match err {
          SignatureError::LengthFixed(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} \"{}\" expected {} argument(s) but received {}.",
              callable_type,
              &*native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthVariadic(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} \"{}\" expected at least {} argument(s) but received {}.",
              callable_type,
              &*native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthDefaultLow(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} \"{}\" expected at least {} argument(s) but received {}.",
              callable_type,
              &*native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::LengthDefaultHigh(expected) => Some(self.runtime_error(
            self.builtin.errors.runtime,
            &format!(
              "{} \"{}\" expected at most {} argument(s) but received {}.",
              callable_type,
              &*native_meta.name,
              expected,
              args.len(),
            ),
          )),
          SignatureError::TypeWrong(parameter) => Some(self.runtime_error(
            self.builtin.errors.type_,
            &format!(
              "{} \"{}\"'s parameter \"{}\" required a {} but received a {}.",
              callable_type,
              &*native_meta.name,
              &*native_meta.signature.parameters[parameter as usize].name,
              native_meta.signature.parameters[parameter as usize].kind,
              ParameterKind::from(args[parameter as usize])
            ),
          )),
        }
      },
    }
  }

  /// Call a bound method
  unsafe fn call_method(&mut self, bound: GcObj<Method>, arg_count: u8) -> Signal {
    self.fiber.peek_set(arg_count as usize, bound.receiver());
    self.resolve_call(bound.method(), arg_count)
  }

  /// bind a method to an instance
  unsafe fn bind_method(&mut self, class: GcObj<Class>, name: GcStr) -> Signal {
    match class.get_method(&name) {
      Some(method) => {
        let bound = self.manage_obj(Method::new(self.fiber.peek(0), method));
        self.fiber.peek_set(0, val!(bound));
        Signal::Ok
      },
      None => self.runtime_error(
        self.builtin.errors.runtime,
        &format!("Undefined property {} on class {}.", name, class.name()),
      ),
    }
  }

  /// invoke a method from the provided class
  unsafe fn invoke_from_class(
    &mut self,
    class: GcObj<Class>,
    method_name: GcStr,
    arg_count: u8,
  ) -> Signal {
    match class.get_method(&method_name) {
      Some(method) => self.resolve_call(method, arg_count),
      None => self.runtime_error(
        self.builtin.errors.property,
        &format!(
          "Undefined property {} on class {}.",
          method_name,
          class.name()
        ),
      ),
    }
  }
}

#[cfg(debug_assertions)]
fn assert_roots(native: GcObj<Native>, roots_before: usize, roots_now: usize) {
  assert!(
    roots_before == roots_now,
    "Native function {} increased roots by {}.",
    native.meta().name,
    roots_now - roots_before,
  );
}
