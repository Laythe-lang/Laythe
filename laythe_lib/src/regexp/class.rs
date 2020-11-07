use crate::{
  native,
  support::{default_class_inheritance, export_and_insert, load_class_from_module, to_dyn_native},
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  object::List,
  package::Package,
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  value::VALUE_NIL,
  CallResult, LyResult,
};
use laythe_env::managed::{Managed, Trace};
use regex::Regex;
use std::io::Write;

const REGEXP_CLASS_NAME: &str = "RegExp";
const REGEXP_FIELD_PATTERN: &str = "pattern";
const REGEXP_FIELD_FLAGS: &str = "flags";

const REGEXP_INIT: NativeMetaBuilder = NativeMetaBuilder::method("init", Arity::Default(1, 2))
  .with_params(&[
    ParameterBuilder::new("pattern", ParameterKind::String),
    ParameterBuilder::new("flags", ParameterKind::String),
  ]);

const REGEXP_TEST: NativeMetaBuilder = NativeMetaBuilder::method("test", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const REGEXP_MATCH: NativeMetaBuilder = NativeMetaBuilder::method("match", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

const REGEXP_CAPTURES: NativeMetaBuilder = NativeMetaBuilder::method("captures", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)]);

pub fn declare_regexp_class(hooks: &GcHooks, module: &mut Module, std: &Package) -> LyResult<()> {
  let class = default_class_inheritance(hooks, std, REGEXP_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name, val!(class))
}

pub fn define_regexp_class(hooks: &GcHooks, module: &Module, _: &Package) -> LyResult<()> {
  let mut class = load_class_from_module(hooks, module, REGEXP_CLASS_NAME)?;

  class.add_field(hooks, hooks.manage_str(REGEXP_FIELD_PATTERN));
  class.add_field(hooks, hooks.manage_str(REGEXP_FIELD_FLAGS));

  class.add_method(
    hooks,
    hooks.manage_str(REGEXP_INIT.name),
    val!(to_dyn_native(hooks, RegExpInit::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(REGEXP_TEST.name),
    val!(to_dyn_native(hooks, RegExpTest::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(REGEXP_MATCH.name),
    val!(to_dyn_native(hooks, RegExpMatch::from(hooks))),
  );

  class.add_method(
    hooks,
    hooks.manage_str(REGEXP_CAPTURES.name),
    val!(to_dyn_native(hooks, RegExpCaptures::from(hooks))),
  );

  Ok(())
}

native!(RegExpInit, REGEXP_INIT);

impl Native for RegExpInit {
  fn call(&self, _hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let mut this = this.unwrap().to_instance();
    this[0] = args[0];
    if args.len() > 1 {
      this[1] = args[1];
    }

    Ok(val!(this))
  }
}

fn get_regex(this: Option<Value>, hooks: &mut Hooks) -> LyResult<Regex> {
  let this = this.unwrap().to_instance();

  match Regex::new(this[0].to_str().as_str()) {
    Ok(regexp) => Ok(regexp),
    Err(err) => hooks.error(err.to_string()),
  }
}

native!(RegExpTest, REGEXP_TEST);

impl Native for RegExpTest {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let regexp = get_regex(this, hooks)?;
    Ok(val!(regexp.is_match(args[0].to_str().as_str())))
  }
}

native!(RegExpMatch, REGEXP_MATCH);

impl Native for RegExpMatch {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let regexp = get_regex(this, hooks)?;

    match regexp.find(args[0].to_str().as_str()) {
      Some(found) => Ok(val!(hooks.manage_str(found.as_str()))),
      None => Ok(VALUE_NIL),
    }
  }
}

native!(RegExpCaptures, REGEXP_CAPTURES);

impl Native for RegExpCaptures {
  fn call(&self, hooks: &mut Hooks, this: Option<Value>, args: &[Value]) -> CallResult {
    let regexp = get_regex(this, hooks)?;

    match regexp.captures(args[0].to_str().as_str()) {
      Some(captures) => {
        let mut results: Managed<List<Value>> = hooks.manage(List::new());
        hooks.push_root(results);

        for capture in captures.iter().map(|sub_capture| match sub_capture {
          Some(sub_capture) => val!(hooks.manage_str(sub_capture.as_str())),
          None => VALUE_NIL,
        }) {
          results.push(capture);
        }

        hooks.pop_roots(1);
        Ok(val!(results))
      }
      None => Ok(VALUE_NIL),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::object::{Class, Instance};

  fn regexp_instance(hooks: &mut Hooks, pattern: &str) -> Value {
    let mut regexp_class = Class::bare(hooks.manage_str(REGEXP_CLASS_NAME));
    regexp_class.add_field(&hooks.as_gc(), hooks.manage_str(REGEXP_FIELD_PATTERN));
    regexp_class.add_field(&hooks.as_gc(), hooks.manage_str(REGEXP_FIELD_FLAGS));

    let regexp = hooks.manage(Instance::new(hooks.manage(regexp_class)));
    let init = RegExpInit::from(&hooks.as_gc());
    init
      .call(
        hooks,
        Some(val!(regexp)),
        &[val!(hooks.manage_str(pattern))],
      )
      .unwrap();

    val!(regexp)
  }

  mod test {
    use laythe_core::{
      hooks::GcHooks,
      value::{VALUE_FALSE, VALUE_TRUE},
    };

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let regexp_test = RegExpTest::from(&hooks);

      assert_eq!(regexp_test.meta().name, "test");
      assert_eq!(regexp_test.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        regexp_test.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let this = regexp_instance(&mut hooks, "[0-9]{3}");
      let regexp_test = RegExpTest::from(&hooks.as_gc());

      let pass = val!(hooks.manage_str("123"));
      let failure = val!(hooks.manage_str("abc"));

      let result = regexp_test.call(&mut hooks, Some(this), &[pass]);
      match result {
        Ok(r) => {
          assert_eq!(r, VALUE_TRUE);
        }
        Err(_) => assert!(false),
      }

      let result = regexp_test.call(&mut hooks, Some(this), &[failure]);
      match result {
        Ok(r) => {
          assert_eq!(r, VALUE_FALSE);
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod match_ {
    use laythe_core::hooks::GcHooks;

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let regexp_capture = RegExpMatch::from(&hooks);

      assert_eq!(regexp_capture.meta().name, "match");
      assert_eq!(regexp_capture.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        regexp_capture.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let this = regexp_instance(&mut hooks, "[0-9]{3}");
      let regexp_capture = RegExpMatch::from(&hooks.as_gc());

      let matched = val!(hooks.manage_str("   123 dude"));
      let unmatched = val!(hooks.manage_str("25 Main St."));

      let result = regexp_capture.call(&mut hooks, Some(this), &[matched]);
      match result {
        Ok(r) => {
          assert!(r.is_str());
          assert_eq!(r.to_str(), hooks.manage_str("123"));
        }
        Err(_) => assert!(false),
      }

      let result = regexp_capture.call(&mut hooks, Some(this), &[unmatched]);
      match result {
        Ok(r) => {
          assert!(r.is_nil());
        }
        Err(_) => assert!(false),
      }
    }
  }

  mod captures {
    use laythe_core::hooks::GcHooks;

    use super::*;
    use crate::support::MockedContext;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let regexp_captures = RegExpCaptures::from(&hooks);

      assert_eq!(regexp_captures.meta().name, "captures");
      assert_eq!(regexp_captures.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        regexp_captures.meta().signature.parameters[0].kind,
        ParameterKind::String
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let this = regexp_instance(&mut hooks, "([0-9]{3}) [a-zA-Z]+");
      let regexp_captures = RegExpCaptures::from(&hooks.as_gc());

      let example = val!(hooks.manage_str("   123 dude"));

      let result = regexp_captures.call(&mut hooks, Some(this), &[example]);
      match result {
        Ok(r) => {
          assert!(r.is_list());
          let list = r.to_list();

          assert_eq!(list.len(), 2);
          assert_eq!(list[0].to_str(), hooks.manage_str("123 dude"));
          assert_eq!(list[1].to_str(), hooks.manage_str("123"));
        }
        Err(_) => assert!(false),
      }
    }
  }
}
