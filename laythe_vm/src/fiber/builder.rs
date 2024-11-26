use super::Fiber;
use laythe_core::{
  constants::SCRIPT, object::ChannelWaiter, support::test_fun_builder, value::Value, Captures,
  Chunk, GcHooks, HookContext, NoContext, Ref,
};

pub struct TestFiberBuilder {
  name: String,
  parent: Option<Ref<Fiber>>,
  module_name: String,
  instructions: Vec<u8>,
  max_slots: i32,
}

impl Default for TestFiberBuilder {
  fn default() -> Self {
    Self {
      name: SCRIPT.to_string(),
      parent: None,
      module_name: "Fiber Module".to_string(),
      instructions: vec![0],
      max_slots: 0,
    }
  }
}

impl TestFiberBuilder {
  pub fn instructions(mut self, instructions: Vec<u8>) -> Self {
    self.instructions = instructions;
    self
  }

  pub fn parent(mut self, parent: Ref<Fiber>) -> Self {
    self.parent = Some(parent);
    self
  }

  pub fn max_slots(mut self, max_slots: i32) -> Self {
    self.max_slots = max_slots;
    self
  }

  pub fn build(self, context: &NoContext) -> Ref<Fiber> {
    let hooks = GcHooks::new(context);
    let mut fun = test_fun_builder(&hooks, &self.name, &self.module_name);
    fun.update_max_slots(self.max_slots);

    let instructions = hooks.manage(&*self.instructions);
    let constants = hooks.manage::<_, &[Value]>(&[]);
    let lines = hooks.manage::<_, &[u16]>(&vec![0; instructions.len()]);
    let chunk = Chunk::new(instructions, constants, lines);

    let fun = hooks.manage_obj(fun.build(chunk));
    hooks.push_root(fun);

    let captures = Captures::build(&hooks, &[]);
    hooks.push_root(captures);

    let waiter = hooks.manage(ChannelWaiter::new(true));
    hooks.push_root(waiter);

    let fiber = Fiber::new(
      &mut context.gc_context().gc(),
      context,
      self.parent,
      fun,
      captures,
      fun.max_slots() + 1,
    );
    hooks.pop_roots(3);
    hooks.manage(fiber)
  }
}
