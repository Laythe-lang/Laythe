use crate::{
  native,
  support::{export_and_insert, export_and_insert_native},
  StdResult,
};
use laythe_core::{
  hooks::{GcHooks, Hooks},
  managed::Trace,
  module::Module,
  object::{LyNative, Native, NativeMetaBuilder},
  signature::{Arity, ParameterBuilder, ParameterKind},
  val,
  value::Value,
  Call, Ref,
};
use std::io::Write;

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

const MAX_META: NativeMetaBuilder =
  NativeMetaBuilder::fun("max", Arity::Variadic(1)).with_params(&[
    ParameterBuilder::new("val", ParameterKind::Number),
    ParameterBuilder::new("vals", ParameterKind::Number),
  ]);

const MIN_META: NativeMetaBuilder =
  NativeMetaBuilder::fun("min", Arity::Variadic(1)).with_params(&[
    ParameterBuilder::new("val", ParameterKind::Number),
    ParameterBuilder::new("vals", ParameterKind::Number),
  ]);

const REM_META: NativeMetaBuilder = NativeMetaBuilder::fun("rem", Arity::Fixed(2)).with_params(&[
  ParameterBuilder::new("val", ParameterKind::Number),
  ParameterBuilder::new("divisor", ParameterKind::Number),
]);

const POW_META: NativeMetaBuilder = NativeMetaBuilder::fun("pow", Arity::Fixed(2)).with_params(&[
  ParameterBuilder::new("val", ParameterKind::Number),
  ParameterBuilder::new("power", ParameterKind::Number),
]);

const RAND_META: NativeMetaBuilder = NativeMetaBuilder::fun("rand", Arity::Fixed(0));

pub fn declare_math_module(hooks: &GcHooks, module: Ref<Module>) -> StdResult<()> {
  export_and_insert(
    hooks,
    module,
    hooks.manage_str(PI),
    val!(std::f64::consts::PI),
  )?;

  export_and_insert(
    hooks,
    module,
    hooks.manage_str(E),
    val!(std::f64::consts::E),
  )?;

  export_and_insert_native(hooks, module, Sin::native(hooks))?;
  export_and_insert_native(hooks, module, Cos::native(hooks))?;
  export_and_insert_native(hooks, module, Ln::native(hooks))?;
  export_and_insert_native(hooks, module, Abs::native(hooks))?;
  export_and_insert_native(hooks, module, Max::native(hooks))?;
  export_and_insert_native(hooks, module, Min::native(hooks))?;
  export_and_insert_native(hooks, module, Rem::native(hooks))?;
  export_and_insert_native(hooks, module, Pow::native(hooks))?;
  export_and_insert_native(hooks, module, Rand::native(hooks))
}

pub fn define_math_module(_: &GcHooks, _: Ref<Module>) -> StdResult<()> {
  Ok(())
}

native!(Sin, SIN_META);

impl LyNative for Sin {
  #[cfg(not(feature = "wasm"))]
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num().sin()))
  }

  #[cfg(feature = "wasm")]
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    use js_sys::Math::sin;
    Call::Ok(val!(sin(args[0].to_num())))
  }
}

native!(Cos, COS_META);

impl LyNative for Cos {
  #[cfg(not(feature = "wasm"))]
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num().cos()))
  }

  #[cfg(feature = "wasm")]
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    use js_sys::Math::cos;
    Call::Ok(val!(cos(args[0].to_num())))
  }
}

native!(Ln, LN_META);

impl LyNative for Ln {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num().ln()))
  }
}

native!(Abs, ABS_META);

impl LyNative for Abs {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num().abs()))
  }
}

native!(Rem, REM_META);

impl LyNative for Rem {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num() % args[1].to_num()))
  }
}

native!(Pow, POW_META);

impl LyNative for Pow {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[0].to_num().powf(args[1].to_num())))
  }
}

native!(Max, MAX_META);

impl LyNative for Max {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[1..]
      .iter()
      .map(|x| x.to_num())
      .fold(args[0].to_num(), f64::max)))
  }
}

native!(Min, MIN_META);

impl LyNative for Min {
  fn call(&self, _hooks: &mut Hooks, args: &[Value]) -> Call {
    Call::Ok(val!(args[1..]
      .iter()
      .map(|x| x.to_num())
      .fold(args[0].to_num(), f64::min)))
  }
}

native!(Rand, RAND_META);

impl LyNative for Rand {
  #[cfg(not(feature = "wasm"))]
  fn call(&self, _hooks: &mut Hooks, _args: &[Value]) -> Call {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let val: f64 = rng.gen_range(0.0..1.0);
    Call::Ok(val!(val))
  }

  #[cfg(feature = "wasm")]
  fn call(&self, _hooks: &mut Hooks, _args: &[Value]) -> Call {
    use js_sys::Math::random;

    #[allow(unused_unsafe)]
    Call::Ok(val!(unsafe { random() }))
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::support::MockedContext;

  mod abs {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let abs = Abs::native(&hooks.as_gc());

      let r = abs.call(&mut hooks, &[val!(-2.0)]).unwrap();
      assert_eq!(r.to_num(), 2.0);

      let r = abs.call(&mut hooks, &[val!(2.0)]).unwrap();
      assert_eq!(r.to_num(), 2.0)
    }
  }

  mod rem {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let rem = Rem::native(&hooks.as_gc());
      let values = &[val!(3.0), val!(2.0)];

      let r = rem.call(&mut hooks, values).unwrap();
      assert_eq!(r.to_num(), 1.0);

      let values = &[val!(-3.0), val!(2.0)];

      let r = rem.call(&mut hooks, values).unwrap();
      assert_eq!(r.to_num(), -1.0);

      let values = &[val!(3.0), val!(-2.0)];
      let r = rem.call(&mut hooks, values).unwrap();
      assert_eq!(r.to_num(), 1.0)
    }
  }

  mod sin {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let sin = Sin::native(&hooks.as_gc());
      let values = &[val!(std::f64::consts::PI)];

      let r = sin.call(&mut hooks, values).unwrap();
      assert!(r.to_num().abs() < 0.0000001)
    }
  }

  mod cos {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let cos = Cos::native(&hooks.as_gc());
      let values = &[val!(std::f64::consts::FRAC_PI_2)];

      let r = cos.call(&mut hooks, values).unwrap();
      assert!(r.to_num().abs() < 0.0000001);
    }
  }

  mod ln {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let ln = Ln::native(&hooks.as_gc());
      let values = &[val!(std::f64::consts::E)];
      let r = ln.call(&mut hooks, values).unwrap();
      assert!((r.to_num() - 1.0).abs() < 0.0000001);
    }
  }

  mod rand {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let rand = Rand::native(&hooks.as_gc());

      for _ in 0..10 {
        let r = rand.call(&mut hooks, &[]).unwrap();
        let num = r.to_num();
        assert!((0.0..1.0).contains(&num));
      }
    }
  }

  mod max {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let max = Max::native(&hooks.as_gc());

      assert_eq!(Ok(val!(1.0)), max.call(&mut hooks, &[val!(1.0)]));
      assert_eq!(
        Ok(val!(10.0)),
        max.call(&mut hooks, &[val!(1.0), val!(10.0), val!(5.0)])
      );
    }
  }

  mod min {
    use super::*;

    #[test]
    fn call() {
      let mut context = MockedContext::default();
      let mut hooks = Hooks::new(&mut context);

      let min = Min::native(&hooks.as_gc());

      assert_eq!(Ok(val!(1.0)), min.call(&mut hooks, &[val!(1.0)]));
      assert_eq!(
        Ok(val!(1.0)),
        min.call(&mut hooks, &[val!(1.0), val!(10.0), val!(5.0)])
      );
    }
  }
}
