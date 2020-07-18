use crate::{native, support::export_and_insert};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{MetaData, Native, NativeMeta, NativeMetaBuilder},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

const PI: &str = "pi";
const E: &str = "e";

const SIN_META: NativeMetaBuilder = NativeMetaBuilder::fun("sin", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("val", ParameterKind::Number)]);

const COS_META: NativeMetaBuilder = NativeMetaBuilder::fun("cos", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("val", ParameterKind::Number)]);

const LN_META: NativeMetaBuilder = NativeMetaBuilder::fun("ln", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("val", ParameterKind::Number)]);

const ABS_META: NativeMetaBuilder = NativeMetaBuilder::fun("abs", Arity::Fixed(1))
  .with_params(&[ParameterBuilder::new("val", ParameterKind::Number)]);

const REM_META: NativeMetaBuilder = NativeMetaBuilder::fun("rem", Arity::Fixed(2)).with_params(&[
  ParameterBuilder::new("val", ParameterKind::Number),
  ParameterBuilder::new("divisor", ParameterKind::Number),
]);

const RAND_META: NativeMetaBuilder = NativeMetaBuilder::fun("rand", Arity::Fixed(0));

pub fn declare_math_module(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(PI.to_string()),
    val!(std::f64::consts::PI),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(E.to_string()),
    val!(std::f64::consts::E),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(SIN_META.name.to_string()),
    val!(hooks.manage(Box::new(Sin::from(hooks)) as Box<dyn Native>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(COS_META.name.to_string()),
    val!(hooks.manage(Box::new(Cos::from(hooks)) as Box<dyn Native>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(LN_META.name.to_string()),
    val!(hooks.manage(Box::new(Ln::from(hooks)) as Box<dyn Native>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ABS_META.name.to_string()),
    val!(hooks.manage(Box::new(Abs::from(hooks)) as Box<dyn Native>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(REM_META.name.to_string()),
    val!(hooks.manage(Box::new(Rem::from(hooks)) as Box<dyn Native>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(RAND_META.name.to_string()),
    val!(hooks.manage(Box::new(Rand::from(hooks)) as Box<dyn Native>)),
  )
}

pub fn define_math_module(_: &GcHooks, _: &mut Module) -> LyResult<()> {
  Ok(())
}

native!(Sin, SIN_META);

impl Native for Sin {
  #[cfg(not(feature = "wasm"))]
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    Ok(val!(args[0].to_num().sin()))
  }

  #[cfg(feature = "wasm")]
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    use js_sys::Math::sin;
    Ok(val!(sin(args[0].to_num())))
  }
}

native!(Cos, COS_META);

impl Native for Cos {
  #[cfg(not(feature = "wasm"))]
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    Ok(val!(args[0].to_num().cos()))
  }

  #[cfg(feature = "wasm")]
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    use js_sys::Math::cos;
    Ok(val!(cos(args[0].to_num())))
  }
}

native!(Ln, LN_META);

impl Native for Ln {
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    Ok(val!(args[0].to_num().ln()))
  }
}

native!(Abs, ABS_META);

impl Native for Abs {
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    Ok(val!(args[0].to_num().abs()))
  }
}

native!(Rem, REM_META);

impl Native for Rem {
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, args: &[Value]) -> CallResult {
    Ok(val!(args[0].to_num() % args[1].to_num()))
  }
}

native!(Rand, RAND_META);

impl Native for Rand {
  #[cfg(not(feature = "wasm"))]
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let val: f64 = rng.gen_range(0.0, 1.0);
    Ok(val!(val))
  }

  #[cfg(feature = "wasm")]
  fn call(&self, _hooks: &mut Hooks, _this: Option<Value>, _args: &[Value]) -> CallResult {
    use js_sys::Math::random;
    Ok(val!(random()))
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::MockedContext;

  mod abs {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let ln = Abs::from(&hooks);

      assert_eq!(ln.meta().name, "abs");
      assert_eq!(ln.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        ln.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let abs = Abs::from(&hooks.to_gc());

      match abs.call(&mut hooks, None, &[val!(-2.0)]) {
        Ok(res) => assert_eq!(res.to_num(), 2.0),
        Err(_) => panic!(),
      };

      match abs.call(&mut hooks, None, &[val!(2.0)]) {
        Ok(res) => assert_eq!(res.to_num(), 2.0),
        Err(_) => panic!(),
      };
    }
  }

  mod rem {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let rem = Rem::from(&hooks);

      assert_eq!(rem.meta().name, "rem");
      assert_eq!(rem.meta().signature.arity, Arity::Fixed(2));
      assert_eq!(
        rem.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
      assert_eq!(
        rem.meta().signature.parameters[1].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let rem = Rem::from(&hooks);
      let values = &[val!(3.0), val!(2.0)];

      match rem.call(&mut hooks, None, values) {
        Ok(res) => assert_eq!(res.to_num(), 1.0),
        Err(_) => panic!(),
      };

      let values = &[val!(-3.0), val!(2.0)];

      match rem.call(&mut hooks, None, values) {
        Ok(res) => assert_eq!(res.to_num(), -1.0),
        Err(_) => panic!(),
      };

      let values = &[val!(3.0), val!(-2.0)];

      match rem.call(&mut hooks, None, values) {
        Ok(res) => assert_eq!(res.to_num(), 1.0),
        Err(_) => panic!(),
      };
    }
  }

  mod sin {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let sin = Sin::from(&hooks);

      assert_eq!(sin.meta().name, "sin");
      assert_eq!(sin.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        sin.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let sin = Sin::from(&hooks);
      let values = &[val!(std::f64::consts::PI)];

      match sin.call(&mut hooks, None, values) {
        Ok(res) => assert!(res.to_num().abs() < 0.0000001),
        Err(_) => panic!(),
      };
    }
  }

  mod cos {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let cos = Cos::from(&hooks);

      assert_eq!(cos.meta().name, "cos");
      assert_eq!(cos.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        cos.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let cos = Cos::from(&hooks);
      let values = &[val!(std::f64::consts::FRAC_PI_2)];

      match cos.call(&mut hooks, None, values) {
        Ok(res) => assert!(res.to_num().abs() < 0.0000001),
        Err(_) => panic!(),
      };
    }
  }

  mod ln {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let ln = Ln::from(&hooks);

      assert_eq!(ln.meta().name, "ln");
      assert_eq!(ln.meta().signature.arity, Arity::Fixed(1));
      assert_eq!(
        ln.meta().signature.parameters[0].kind,
        ParameterKind::Number
      );
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let ln = Ln::from(&hooks);
      let values = &[val!(std::f64::consts::E)];

      match ln.call(&mut hooks, None, values) {
        Ok(res) => assert!((res.to_num() - 1.0).abs() < 0.0000001),
        Err(_) => panic!(),
      };
    }
  }

  mod rand {
    use super::*;

    #[test]
    fn new() {
      let mut context = MockedContext::default();
      let hooks = GcHooks::new(&mut context);

      let rand = Rand::from(&hooks);

      assert_eq!(rand.meta().name, "rand");
      assert_eq!(rand.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let rand = Rand::from(&hooks);

      for _ in 0..10 {
        match rand.call(&mut hooks, None, &[]) {
          Ok(res) => {
            let num = res.to_num();
            assert!(num >= 0.0 && num < 1.0);
          }
          Err(_) => panic!(),
        };
      }
    }
  }
}
