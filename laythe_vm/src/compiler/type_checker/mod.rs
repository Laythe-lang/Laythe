use std::{mem, rc::Rc};

use super::ir::{
  ast::{self, Decl, Expr, Primary, Scoped, Span, Spanned, Stmt, Symbol},
  symbol_table::SymbolTable,
  token::Token,
  type_::{FunType, Type},
};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
  source::{Source, VmFileId},
  FeResult,
};

// https://jaked.org/blog/2021-09-15-Reconstructing-TypeScript-part-1

pub struct TypeChecker<'src> {
  /// All the errors found during resolution
  errors: Vec<Diagnostic<VmFileId>>,

  /// The file id for the current file
  file_id: VmFileId,

  /// The current stack of symbol tables
  tables: Vec<SymbolTable<'src>>,

  /// read only reference to the allocator
  source: &'src Source,
}

impl<'src> TypeChecker<'src> {
  pub fn new(source: &'src Source, file_id: VmFileId) -> Self {
    Self {
      errors: vec![],
      file_id,
      tables: vec![],
      source,
    }
  }

  pub fn check(mut self, ast: &mut ast::Module<'src>) -> FeResult<()> {
    self.scope(ast, |_self, _ast| {
      // resolve variables at module scope
      for decl in &mut _ast.decls {
        _self.decl(decl);
      }
    });

    if self.errors.is_empty() {
      Ok(())
    } else {
      Err(self.errors)
    }
  }

  /// Resolve a declaration
  fn decl(&mut self, decl: &mut Decl<'src>) {
    match decl {
      Decl::Symbol(symbol) => self.symbol(symbol),
      Decl::Export(export) => self.symbol(export),
      Decl::Stmt(stmt) => self.stmt(stmt),
      _ => (),
    }
  }

  /// Resolve a symbol declaration
  fn symbol(&mut self, symbol: &mut Symbol<'src>) {
    match symbol {
      Symbol::Class(class) => self.class(class),
      Symbol::Fun(fun) => self.fun(fun),
      Symbol::Let(let_) => self.let_(let_),
      // Symbol::Trait(_) => todo!(),
      // Symbol::TypeDecl(_) => todo!(),
      _ => (), // _ => todo!(),
    };
  }

  fn class(&mut self, class: &mut ast::Class<'src>) {
    self.scope(class, |_self, _class| {
      // process the initializer
      if let Some(init) = &mut _class.init {
        _self.function(init);
      }

      // process methods
      for method in &mut _class.methods {
        _self.function(method);
      }

      // process static methods
      for static_method in &mut _class.static_methods {
        _self.function(static_method);
      }
    });
  }

  fn fun(&mut self, fun: &mut ast::Fun<'src>) {
    let fun_type = self.call_sig(&fun.call_sig);
    let name = fun.name.as_ref().expect("Expected function name");
    self.set_type(name, fun_type);

    self.function(fun);
  }

  fn let_(&mut self, let_: &mut ast::Let<'src>) {
    let derived_type = match &let_.value {
      Some(v) => self.expr(v),
      None => Type::Nil,
    };

    let let_type = match &let_.type_ {
      Some(ast_type) => {
        let declared_type = self.type_(ast_type);

        if !derived_type.is_subtype(&declared_type) {
          match &let_.value {
            Some(v) => {
              let mut labels = vec![Label::primary(self.file_id, v.span())
                .with_message(&format!("Expression evaluated to {}", derived_type))];

              if let Some(type_) = &let_.type_ {
                labels.push(
                  Label::primary(self.file_id, type_.span()).with_message(&format!(
                    "But symbol {} was declared {}",
                    let_.name.str(),
                    declared_type
                  )),
                )
              }

              self.error_with_context(
                &format!("Invalid expression for type {}", declared_type),
                labels,
              )
            },
            None => {
              let mut labels = vec![];

              if let Some(type_) = &let_.type_ {
                labels.push(
                  Label::secondary(self.file_id, type_.span()).with_message(&format!(
                    "Symbol {} was declared {}",
                    let_.name.str(),
                    declared_type
                  )),
                )
              }

              self.error_with_context(
                &format!("No value was given for type {}", declared_type),
                labels,
              )
            },
          }
        }

        declared_type
      },
      None => derived_type,
    };

    self.set_type(&let_.name, let_type);
  }

  /// Type check this statement
  fn stmt(&mut self, stmt: &mut Stmt<'src>) {
    match stmt {
      Stmt::Expr(expr) => {
        self.expr(expr);
      },
      _ => (),
    }
  }

  /// Type check an expression
  fn expr(&self, expr: &Expr<'src>) -> Type {
    match expr {
      Expr::Atom(atom) => self.atom(atom),
      _ => Type::Any,
    }
  }

  fn block(&mut self, block: &mut ast::Block<'src>) {
    for decl in block.decls.iter_mut() {
      self.decl(decl);
    }
  }

  /// Type check a function
  fn function(&mut self, fun: &mut ast::Fun<'src>) {
    self.scope(fun, |_self, _fun| {
      match &mut _fun.body {
        ast::FunBody::Block(block) => _self.block(block),
        ast::FunBody::Expr(expr) => {
          _self.expr(expr);
        },
      };
    });
  }

  /// Set the functions arity from the call signature
  fn call_sig(&mut self, call_sig: &ast::CallSignature<'src>) -> Type {
    let mut param_types: Vec<Type> = Vec::with_capacity(call_sig.params.len());

    for param in &call_sig.params {
      let param_type = param
        .type_
        .as_ref()
        .map(|type_| self.type_(&type_))
        .unwrap_or(Type::Any);

      param_types.push(param_type.clone());
    }

    let ret_type = call_sig
      .ret_type
      .as_ref()
      .map(|type_| self.type_(&type_))
      .unwrap_or(Type::Any)
      .clone();

    Type::Fun(Rc::new(FunType::new(param_types, ret_type)))
  }

  /// Resolve an atom expression
  fn atom(&self, atom: &ast::Atom<'src>) -> Type {
    let type_ = match &atom.primary {
      Primary::False(_) => Type::Boolean,
      Primary::Nil(_) => Type::Nil,
      Primary::Number(_) => Type::Number,
      Primary::String(_) => Type::String,
      Primary::True(_) => Type::Boolean,
      Primary::Ident(token) => self.identifier(token),
      _ => Type::Any,
    };

    if atom.trailers.is_empty() {
      type_
    } else {
      Type::Any
    }
  }

  fn type_(&self, ast_type: &ast::Type) -> Type {
    match ast_type {
      ast::Type::Primitive(primitive) => self.primitive(primitive),
      _ => Type::Any,
    }
  }

  fn identifier(&self, token: &Token<'src>) -> Type {
    self.get_type(token)
  }

  fn primitive(&self, primitive: &ast::Primitive) -> Type {
    match primitive {
      ast::Primitive::Nil(_) => Type::Nil,
      ast::Primitive::Number(_) => Type::Number,
      ast::Primitive::Bool(_) => Type::Boolean,
      ast::Primitive::String(_) => Type::String,
      ast::Primitive::Any(_) => Type::Any,
    }
  }

  fn scope<T: Scoped<'src>>(&mut self, scoped: &mut T, cb: impl FnOnce(&mut Self, &mut T)) {
    let table = mem::replace(scoped.symbols_mut(), SymbolTable::new(self.source.vec()));

    self.begin_scope(table);
    cb(self, scoped);

    scoped.set_symbols(self.end_scope());
  }

  fn begin_scope(&mut self, table: SymbolTable<'src>) {
    self.tables.push(table);
  }

  fn end_scope(&mut self) -> SymbolTable<'src> {
    self.tables.pop().unwrap()
  }

  /// resolve a type of a variable
  fn get_type(&self, name: &Token<'src>) -> Type {
    for table in self.tables.iter().rev() {
      if let Some(symbol) = table.get(name.str()) {
        return symbol.type_().clone();
      }
    }

    Type::Any
  }

  /// set a type of a variable
  fn set_type(&mut self, name: &Token<'src>, symbol_type: Type) {
    for table in self.tables.iter_mut().rev() {
      if let Some(symbol) = table.get_mut(name.str()) {
        symbol.set_symbol_type(symbol_type);
        return;
      }
    }
  }

  /// Indicate an error with additional context
  fn error_with_context(&mut self, message_primary: &str, labels: Vec<Label<VmFileId>>) {
    let error = Diagnostic::error()
      .with_message(message_primary)
      .with_labels(labels);

    self.errors.push(error);
  }

  /// Indicate an error occurred at the previous index
  fn error(&mut self, message: &str, span: Option<Span>) {
    let error = Diagnostic::error().with_message(message);

    let error = match span {
      Some(span) => error.with_labels(vec![Label::primary(self.file_id, span)]),
      None => error,
    };

    self.errors.push(error);
  }
}

// fn check(expr: &ast::Expr, t: &Type) -> Result<(), String> {
//   if 10 < 3 {
//     return Ok(())
//   }

//   Ok(())
// }

// fn check_instance(expr: &ast::Expr, t: &Type) -> Result<(), String> {

// }
