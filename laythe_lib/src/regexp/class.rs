use crate::{
  global::SYNTAX_ERROR_NAME,
  native, native_with_error,
  support::load_class_from_package,
  support::{default_class_inheritance, export_and_insert, load_class_from_module},
  StdResult, STD,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  list,
  managed::{Gc, GcObj, Trace},
  module::{Module, Package},
  object::{LyNative, Native, NativeMetaBuilder, ObjectKind},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::{Value, VALUE_NIL},
  Call, LyError,
};
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
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)])
  .with_stack();

const REGEXP_MATCH: NativeMetaBuilder = NativeMetaBuilder::method("match", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)])
  .with_stack();

const REGEXP_CAPTURES: NativeMetaBuilder = NativeMetaBuilder::method("captures", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("string", ParameterKind::String)])
  .with_stack();

pub fn declare_regexp_class(
  hooks: &GcHooks,
  module: Gc<Module>,
  package: Gc<Package>,
) -> StdResult<()> {
  let class = default_class_inheritance(hooks, package, REGEXP_CLASS_NAME)?;
  export_and_insert(hooks, module, class.name(), val!(class))
}

pub fn define_regexp_class(
  hooks: &GcHooks,
  module: Gc<Module>,
  package: Gc<Package>,
) -> StdResult<()> {
  let mut class = load_class_from_module(hooks, module, REGEXP_CLASS_NAME)?;
  let syntax_error = val!(load_class_from_package(
    hooks,
    package,
    STD,
    SYNTAX_ERROR_NAME
  )?);

  class.add_field(hooks.manage_str(REGEXP_FIELD_PATTERN));
  class.add_field(hooks.manage_str(REGEXP_FIELD_FLAGS));

  class.add_method(
    hooks.manage_str(REGEXP_INIT.name),
    val!(RegExpInit::native(hooks)),
  );

  class.add_method(
    hooks.manage_str(REGEXP_TEST.name),
    val!(RegExpTest::native(hooks, syntax_error)),
  );

  class.add_method(
    hooks.manage_str(REGEXP_MATCH.name),
    val!(RegExpMatch::native(hooks, syntax_error)),
  );

  class.add_method(
    hooks.manage_str(REGEXP_CAPTURES.name),
    val!(RegExpCaptures::native(hooks, syntax_error)),
  );

  Ok(())
}

native!(RegExpInit, REGEXP_INIT);

impl LyNative for RegExpInit {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    let mut this = args[0].to_obj().to_instance();
    this[0] = args[1];
    if args.len() > 2 {
      this[1] = args[2];
    }

    Call::Ok(val!(this))
  }
}

macro_rules! get_regex {
  ( $self:ident, $this:expr, $hooks:ident ) => {{
    let instance = $this.to_obj().to_instance();

    match Regex::new(&*instance[0].to_obj().to_str()) {
      Ok(regexp) => regexp,
      Err(err) => return $self.call_error($hooks, err.to_string()),
    }
  }};
}

native_with_error!(RegExpTest, REGEXP_TEST);

impl LyNative for RegExpTest {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let regexp = get_regex!(self, args[0], hooks);

    Call::Ok(val!(regexp.is_match(&args[1].to_obj().to_str())))
  }
}

native_with_error!(RegExpMatch, REGEXP_MATCH);

impl LyNative for RegExpMatch {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let regexp = get_regex!(self, args[0], hooks);

    match regexp.find(&args[1].to_obj().to_str()) {
      Some(found) => Call::Ok(val!(hooks.manage_str(found.as_str()))),
      None => Call::Ok(VALUE_NIL),
    }
  }
}

native_with_error!(RegExpCaptures, REGEXP_CAPTURES);

impl LyNative for RegExpCaptures {
  fn call(&self, hooks: &mut Hooks, args: &[Value]) -> Call {
    let regexp = get_regex!(self, args[0], hooks);

    match regexp.captures(&args[1].to_obj().to_str()) {
      Some(captures) => {
        let mut results = hooks.manage_obj(list!());
        hooks.push_root(results);

        for capture in captures.iter().map(|sub_capture| match sub_capture {
          Some(sub_capture) => val!(hooks.manage_str(sub_capture.as_str())),
          None => VALUE_NIL,
        }) {
          results.push(capture, &hooks.as_gc());
        }

        hooks.pop_roots(1);
        Call::Ok(val!(results))
      },
      None => Call::Ok(VALUE_NIL),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use laythe_core::object::Class;

  fn regexp_instance(hooks: &mut Hooks, pattern: &str) -> Value {
    let mut regexp_class = Class::bare(hooks.manage_str(REGEXP_CLASS_NAME));
    regexp_class.add_field(hooks.manage_str(REGEXP_FIELD_PATTERN));
    regexp_class.add_field(hooks.manage_str(REGEXP_FIELD_FLAGS));

    let regexp = hooks.manage_obj(hooks.manage_obj(regexp_class));
    let init = RegExpInit::native(&hooks.as_gc());
    init
      .call(hooks, &[val!(regexp), val!(hooks.manage_str(pattern))])
      .unwrap();

    val!(regexp)
  }

  mod test {
    use laythe_core::value::{VALUE_FALSE, VALUE_TRUE};

    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let this = regexp_instance(&mut hooks, "[0-9]{3}");
      let regexp_test = RegExpTest::native(&hooks.as_gc(), error);

      let pass = val!(hooks.manage_str("123"));
      let failure = val!(hooks.manage_str("abc"));

      let result = regexp_test.call(&mut hooks, &[this, pass]).unwrap();
      assert_eq!(result, VALUE_TRUE);

      let result = regexp_test.call(&mut hooks, &[this, failure]).unwrap();
      assert_eq!(result, VALUE_FALSE);
    }
  }

  mod match_ {

    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let this = regexp_instance(&mut hooks, "[0-9]{3}");
      let regexp_capture = RegExpMatch::native(&hooks.as_gc(), error);

      let matched = val!(hooks.manage_str("   123 dude"));
      let unmatched = val!(hooks.manage_str("25 Main St."));

      let r = regexp_capture.call(&mut hooks, &[this, matched]).unwrap();
      assert!(r.is_obj_kind(ObjectKind::String));
      assert_eq!(r.to_obj().to_str(), hooks.manage_str("123"));

      let r = regexp_capture.call(&mut hooks, &[this, unmatched]).unwrap();
      assert!(r.is_nil());
    }
  }

  mod captures {
    use super::*;
    use crate::support::{test_error_class, MockedContext};

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let error = val!(test_error_class(&hooks.as_gc()));
      let this = regexp_instance(&mut hooks, "([0-9]{3}) [a-zA-Z]+");
      let regexp_captures = RegExpCaptures::native(&hooks.as_gc(), error);

      let example = val!(hooks.manage_str("   123 dude"));

      let r = regexp_captures.call(&mut hooks, &[this, example]).unwrap();

      assert!(r.is_obj_kind(ObjectKind::List));
      let list = r.to_obj().to_list();

      assert_eq!(list.len(), 2);
      assert_eq!(list[0].to_obj().to_str(), hooks.manage_str("123 dude"));
      assert_eq!(list[1].to_obj().to_str(), hooks.manage_str("123"));
    }
  }
}
