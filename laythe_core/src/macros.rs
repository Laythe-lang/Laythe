#[macro_export]
macro_rules! list {
  () => {{
    use $crate::VecBuilder;

    VecBuilder::new(&[] as &[Value], 4)
  }};
  ( $x:expr ) => {{
    use $crate::VecBuilder;

    let len = $x.len();
    VecBuilder::new($x, std::cmp::max(len, 4))
  }};
}

#[macro_export]
macro_rules! to_obj_kind {
  ($o:expr, Channel) => {
    $o.to_channel()
  };
  ($o:expr, Class) => {
    $o.to_class()
  };
  ($o:expr, Closure) => {
    $o.to_closure()
  };
  ($o:expr, Fun) => {
    $o.to_fun()
  };
  ($o:expr, Instance) => {
    $o.to_instance()
  };
  ($o:expr, Enumerator) => {
    $o.to_enumerator()
  };
  ($o:expr, List) => {
    $o.to_list()
  };
  ($o:expr, Map) => {
    $o.to_map()
  };
  ($o:expr, Method) => {
    $o.to_method()
  };
  ($o:expr, Native) => {
    $o.to_native()
  };
  ($o:expr, String) => {
    $o.to_str()
  };
  ($o:expr, LyBox) => {
    $o.to_box()
  };
  ($o:expr, Tuple) => {
    $o.to_tuple()
  };
}

#[macro_export]
macro_rules! match_obj {
  (($scrutinee:expr) {
    $(ObjectKind::$obj_kind:ident($p:pat) => $e:expr,)*
    $(_ => $d:expr,)?
  }) => {
    {
      use $crate::to_obj_kind;

      let object: &$crate::ObjectRef = $scrutinee;
      match object.kind() {
        $(ObjectKind::$obj_kind => {
          let $p = to_obj_kind!(object, $obj_kind);
          $e
        })*
        $(_ => $d)?
      }
    }
  };
}

#[macro_export]
macro_rules! val {
  ( $x:expr ) => {
    $crate::value::Value::from($x)
  };
}

#[macro_export]
macro_rules! impl_debug_heap {
  ( $x:ty ) => {
    impl $crate::managed::DebugHeap for $x {
      fn fmt_heap(&self, f: &mut std::fmt::Formatter, _depth: usize) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
      }
    }
  };
}

#[macro_export]
macro_rules! if_let_obj {
  (ObjectKind::$obj_kind:ident($p:pat) = ($v:expr) $b:block) => {{
    use $crate::to_obj_kind;

    let val: Value = $v;
    if val.is_obj() {
      let obj = val.to_obj();

      if obj.is_kind(ObjectKind::$obj_kind) {
        let $p = to_obj_kind!(obj, $obj_kind);
        $b
      }
    }
  }};
  (ObjectKind::$obj_kind:ident(mut $p:pat) = ($v:expr) $b:block) => {{
    use $crate::to_obj_kind;

    let val: Value = $v;
    if val.is_obj() {
      let obj = val.to_obj();

      if obj.is_kind(ObjectKind::$obj_kind) {
        let mut $p = to_obj_kind!(obj, $obj_kind);
        $b
      }
    }
  }};
  (ObjectKind::$obj_kind:ident($p:pat) = ($v:expr) $b1:block else $b2:block) => {{
    use $crate::to_obj_kind;

    let val: Value = $v;
    if val.is_obj() {
      let obj = val.to_obj();

      if obj.is_kind(ObjectKind::$obj_kind) {
        let $p = to_obj_kind!(obj, $obj_kind);
        $b1
      } else $b2
    } else $b2
  }};
  (ObjectKind::$obj_kind:ident(mut $p:pat) = ($v:expr) $b1:block else $b2:block) => {{
    use $crate::to_obj_kind;

    let val: Value = $v;
    if val.is_obj() {
      let obj = val.to_obj();

      if obj.is_kind(ObjectKind::$obj_kind) {
        let mut $p = to_obj_kind!(obj, $obj_kind);
        $b1
      } else $b2
    } else $b2
  }};
}
