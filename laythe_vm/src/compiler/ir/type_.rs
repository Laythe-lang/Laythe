use std::{collections::HashMap, fmt::Display, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
  Any,
  Nil,
  Boolean,
  Number,
  String,
  Fun(Rc<FunType>),
  Class(Rc<ClassType>),
}

impl Type {
  pub fn is_subtype(&self, other_type: &Type) -> bool {
    match (self, other_type) {
      (Type::Any, _) => true,
      (_, Type::Any) => true,
      (Type::Nil, Type::Nil) => true,
      (Type::Boolean, Type::Boolean) => true,
      (Type::Number, Type::Number) => true,
      (Type::String, Type::String) => true,
      (Type::Fun(self_fun), Type::Fun(other_fun)) => {
        if self_fun.params.len() == other_fun.params.len() {
          let params_subclass = other_fun
            .params
            .iter()
            .zip(other_fun.params.iter())
            .all(|(other_param, self_param)| self_param.is_subtype(other_param));

          if params_subclass {
            other_fun.ret.is_subtype(&self_fun.ret)
          } else {
            false
          }
        } else {
          false
        }
      },
      (Type::Class(self_class), Type::Class(other_class)) => {
        other_class
          .properties
          .iter()
          .all(
            |(key, other_property_type)| match self_class.properties.get(key) {
              Some(self_property_type) => self_property_type.is_subtype(other_property_type),
              None => false,
            },
          )
      },
      _ => false,
    }
  }
}

impl Default for Type {
  fn default() -> Self {
    Self::Any
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunType {
  params: Vec<Type>,
  ret: Type,
}

impl FunType {
  pub fn new(params: Vec<Type>, ret: Type) -> Self {
    Self { params, ret }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassType {
  properties: HashMap<String, Type>,
  methods: HashMap<String, FunType>,
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Type::Any => f.write_str("any"),
      Type::Nil => f.write_str("nil"),
      Type::Boolean => f.write_str("bool"),
      Type::Number => f.write_str("number"),
      Type::String => f.write_str("string"),
      Type::Fun(_) => f.write_str("TODO function"),
      Type::Class(_) => f.write_str("TODO class"),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  mod is_subtype {
    use super::*;

    #[test]
    fn any_subclass_to_all() {
      assert!(Type::Any.is_subtype(&Type::Boolean));
    }
  }
}
