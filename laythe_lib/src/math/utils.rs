use crate::support::export_and_insert;
use laythe_core::{
  hooks::{GcHooks, Hooks},
  module::Module,
  native::{NativeFun, NativeMeta},
  signature::{Arity, Parameter, ParameterKind},
  value::Value,
  CallResult, LyResult,
};
use laythe_env::{managed::Trace, stdio::Stdio};

const PI: &str = "pi";
const E: &str = "e";

const SIN_META: NativeMeta = NativeMeta::new(
  "sin",
  Arity::Fixed(1),
  &[Parameter::new("val", ParameterKind::Number)],
);
const COS_META: NativeMeta = NativeMeta::new(
  "cos",
  Arity::Fixed(1),
  &[Parameter::new("val", ParameterKind::Number)],
);
const LN_META: NativeMeta = NativeMeta::new(
  "ln",
  Arity::Fixed(1),
  &[Parameter::new("val", ParameterKind::Number)],
);
const ABS_META: NativeMeta = NativeMeta::new(
  "abs",
  Arity::Fixed(1),
  &[Parameter::new("val", ParameterKind::Number)],
);
const REM_META: NativeMeta = NativeMeta::new(
  "rem",
  Arity::Fixed(2),
  &[
    Parameter::new("val", ParameterKind::Number),
    Parameter::new("divisor", ParameterKind::Number),
  ],
);
const RAND_META: NativeMeta = NativeMeta::new("rand", Arity::Fixed(0), &[]);

pub fn declare_math_module(hooks: &GcHooks, self_module: &mut Module) -> LyResult<()> {
  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(PI.to_string()),
    Value::from(std::f64::consts::PI),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(E.to_string()),
    Value::from(std::f64::consts::E),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(SIN_META.name.to_string()),
    Value::from(hooks.manage(Box::new(Sin()) as Box<dyn NativeFun>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(COS_META.name.to_string()),
    Value::from(hooks.manage(Box::new(Cos()) as Box<dyn NativeFun>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(LN_META.name.to_string()),
    Value::from(hooks.manage(Box::new(Ln()) as Box<dyn NativeFun>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(ABS_META.name.to_string()),
    Value::from(hooks.manage(Box::new(Abs()) as Box<dyn NativeFun>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(REM_META.name.to_string()),
    Value::from(hooks.manage(Box::new(Rem()) as Box<dyn NativeFun>)),
  )?;

  export_and_insert(
    hooks,
    self_module,
    hooks.manage_str(RAND_META.name.to_string()),
    Value::from(hooks.manage(Box::new(Rand()) as Box<dyn NativeFun>)),
  )
}

pub fn define_math_module(_: &GcHooks, _: &mut Module) -> LyResult<()> {
  Ok(())
}

#[derive(Clone, Debug, Trace)]
pub struct Sin();

impl NativeFun for Sin {
  fn meta(&self) -> &NativeMeta {
    &SIN_META
  }

  #[cfg(not(feature = "wasm"))]
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> CallResult {
    Ok(Value::from(args[0].to_num().sin()))
  }

  #[cfg(feature = "wasm")]
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> CallResult {
    use js_sys::Math::sin;
    Ok(Value::from(sin(args[0].to_num())))
  }
}

#[derive(Clone, Debug, Trace)]
pub struct Cos();

impl NativeFun for Cos {
  fn meta(&self) -> &NativeMeta {
    &COS_META
  }

  #[cfg(not(feature = "wasm"))]
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> CallResult {
    Ok(Value::from(args[0].to_num().cos()))
  }

  #[cfg(feature = "wasm")]
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> CallResult {
    use js_sys::Math::cos;
    Ok(Value::from(cos(args[0].to_num())))
  }
}

#[derive(Clone, Debug, Trace)]
pub struct Ln();

impl NativeFun for Ln {
  fn meta(&self) -> &NativeMeta {
    &LN_META
  }

  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> CallResult {
    Ok(Value::from(args[0].to_num().ln()))
  }
}

#[derive(Clone, Debug, Trace)]
pub struct Abs();

impl NativeFun for Abs {
  fn meta(&self) -> &NativeMeta {
    &ABS_META
  }

  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> CallResult {
    Ok(Value::from(args[0].to_num().abs()))
  }
}

#[derive(Clone, Debug, Trace)]
pub struct Rem();

impl NativeFun for Rem {
  fn meta(&self) -> &NativeMeta {
    &REM_META
  }

  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> CallResult {
    Ok(Value::from(args[0].to_num() % args[1].to_num()))
  }
}

#[derive(Clone, Debug, Trace)]
pub struct Rand();

impl NativeFun for Rand {
  fn meta(&self) -> &NativeMeta {
    &RAND_META
  }

  #[cfg(not(feature = "wasm"))]
  fn call(&self, _hooks: &mut Hooks, _args: &[Value]) -> CallResult {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let val: f64 = rng.gen_range(0.0, 1.0);
    Ok(Value::from(val))
  }

  #[cfg(feature = "wasm")]
  fn call(&self, _hooks: &mut Hooks, _args: &[Value]) -> CallResult {
    use js_sys::Math::random;
    Ok(Value::from(random()))
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
      let ln = Abs();

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

      let abs = Abs();

      match abs.call(&mut hooks, &[Value::from(-2.0)]) {
        Ok(res) => assert_eq!(res.to_num(), 2.0),
        Err(_) => panic!(),
      };

      match abs.call(&mut hooks, &[Value::from(2.0)]) {
        Ok(res) => assert_eq!(res.to_num(), 2.0),
        Err(_) => panic!(),
      };
    }
  }

  mod rem {
    use super::*;

    #[test]
    fn new() {
      let rem = Rem();

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

      let rem = Rem();
      let values = &[Value::from(3.0), Value::from(2.0)];

      match rem.call(&mut hooks, values) {
        Ok(res) => assert_eq!(res.to_num(), 1.0),
        Err(_) => panic!(),
      };

      let values = &[Value::from(-3.0), Value::from(2.0)];

      match rem.call(&mut hooks, values) {
        Ok(res) => assert_eq!(res.to_num(), -1.0),
        Err(_) => panic!(),
      };

      let values = &[Value::from(3.0), Value::from(-2.0)];

      match rem.call(&mut hooks, values) {
        Ok(res) => assert_eq!(res.to_num(), 1.0),
        Err(_) => panic!(),
      };
    }
  }

  mod sin {
    use super::*;

    #[test]
    fn new() {
      let sin = Sin();

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

      let sin = Sin();
      let values = &[Value::from(std::f64::consts::PI)];

      match sin.call(&mut hooks, values) {
        Ok(res) => assert!(res.to_num().abs() < 0.0000001),
        Err(_) => panic!(),
      };
    }
  }

  mod cos {
    use super::*;

    #[test]
    fn new() {
      let cos = Cos();

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

      let cos = Cos();
      let values = &[Value::from(std::f64::consts::FRAC_PI_2)];

      match cos.call(&mut hooks, values) {
        Ok(res) => assert!(res.to_num().abs() < 0.0000001),
        Err(_) => panic!(),
      };
    }
  }

  mod ln {
    use super::*;

    #[test]
    fn new() {
      let ln = Ln();

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

      let ln = Ln();
      let values = &[Value::from(std::f64::consts::E)];

      match ln.call(&mut hooks, values) {
        Ok(res) => assert!((res.to_num() - 1.0).abs() < 0.0000001),
        Err(_) => panic!(),
      };
    }
  }

  mod rand {
    use super::*;

    #[test]
    fn new() {
      let rand = Rand();

      assert_eq!(rand.meta().name, "rand");
      assert_eq!(rand.meta().signature.arity, Arity::Fixed(0));
    }

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let rand = Rand();

      for _ in 0..10 {
        match rand.call(&mut hooks, &[]) {
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
