use bumpalo::boxed::Box;
use bumpalo::collections::vec::Vec;
use std::{ops::Range, usize};

use super::{symbol_table::SymbolTable, token::Token};

/// A visitor pattern for the Laythe ast.
/// Not sure if this currently provides any value as enum
/// already know there variants. May still be useful
/// to ensure each node has cold associated with it
pub trait Visitor<'a> {
  type Result;

  fn visit(&mut self, module: &Module) -> Self::Result;

  fn visit_decl(&mut self, decl: &Decl) -> Self::Result;
  fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Result;
  fn visit_expr(&mut self, expr: &Expr) -> Self::Result;
  fn visit_primary(&mut self, primary: &Primary) -> Self::Result;

  fn visit_symbol(&mut self, symbol: &Symbol) -> Self::Result;
  fn visit_export(&mut self, export: &Symbol) -> Self::Result;
  fn visit_error(&mut self, error: &[Token<'a>]) -> Self::Result;

  fn visit_class(&mut self, class: &Class) -> Self::Result;
  fn visit_fun(&mut self, fun: &Fun) -> Self::Result;
  fn visit_let(&mut self, let_: &Let) -> Self::Result;
  fn visit_method(&mut self, method: &Fun) -> Self::Result;
  fn visit_static_method(&mut self, static_method: &Fun) -> Self::Result;

  fn visit_import(&mut self, import: &Import) -> Self::Result;
  fn visit_for(&mut self, for_: &For) -> Self::Result;
  fn visit_while(&mut self, while_: &While) -> Self::Result;
  fn visit_if(&mut self, if_: &If) -> Self::Result;
  fn visit_launch(&mut self, launch: &Launch) -> Self::Result;
  fn visit_return(&mut self, return_: &Return) -> Self::Result;
  fn visit_continue(&mut self, continue_: &Token<'a>) -> Self::Result;
  fn visit_break(&mut self, break_: &Token<'a>) -> Self::Result;
  fn visit_try(&mut self, try_: &Try) -> Self::Result;
  fn visit_raise(&mut self, raise: &Raise) -> Self::Result;
  fn visit_block(&mut self, block: &Block) -> Self::Result;

  fn visit_assign(&mut self, assign: &Assign) -> Self::Result;
  fn visit_drain(&mut self, drain: &Send) -> Self::Result;
  fn visit_assign_binary(&mut self, assign: &AssignBinary) -> Self::Result;
  fn visit_ternary(&mut self, ternary: &Ternary) -> Self::Result;
  fn visit_binary(&mut self, binary: &Binary) -> Self::Result;
  fn visit_unary(&mut self, unary: &Unary) -> Self::Result;
  fn visit_atom(&mut self, atom: &Atom) -> Self::Result;

  fn visit_index(&mut self, index: &Index) -> Self::Result;
  fn visit_call(&mut self, call: &Call) -> Self::Result;
  fn visit_access(&mut self, access: &Access) -> Self::Result;
  fn visit_call_sig(&mut self, call_sig: &CallSignature) -> Self::Result;

  fn visit_true(&mut self, token: &Token<'a>) -> Self::Result;
  fn visit_false(&mut self, token: &Token<'a>) -> Self::Result;
  fn visit_nil(&mut self, token: &Token<'a>) -> Self::Result;
  fn visit_number(&mut self, token: &Token<'a>) -> Self::Result;
  fn visit_string(&mut self, token: &Token<'a>) -> Self::Result;
  fn visit_channel(&mut self, token: &Channel<'a>) -> Self::Result;
  fn visit_interpolation(&mut self, string_interp: &Interpolation) -> Self::Result;
  fn visit_ident(&mut self, token: &Token<'a>) -> Self::Result;
  fn visit_instance_access(&mut self, token: &InstanceAccess<'a>) -> Self::Result;
  fn visit_self(&mut self, token: &Token<'a>) -> Self::Result;
  fn visit_super(&mut self, token: &Super) -> Self::Result;
  fn visit_lambda(&mut self, fun: &Fun) -> Self::Result;
  fn visit_list(&mut self, items: &Collection) -> Self::Result;
  fn visit_tuple(&mut self, items: &Collection) -> Self::Result;
  fn visit_map(&mut self, kvps: &Map) -> Self::Result;
}

/// A visitor for each type should typically be used together
/// with the `Visitor` trait above when this pass needs type information
pub trait TypeVisitor {
  type Result;

  fn visit_trait(&mut self, trait_: &Trait) -> Self::Result;
  fn visit_type_decl(&mut self, type_decl: &TypeDecl) -> Self::Result;

  fn visit_type(&mut self, type_: &Type) -> Self::Result;
  fn visit_type_params(&mut self, type_params: &[TypeParam]) -> Self::Result;
  fn visit_type_member(&mut self, type_member: &TypeMember) -> Self::Result;
  fn visit_type_method(&mut self, type_method: &TypeMethod) -> Self::Result;
  fn visit_union(&mut self, union: &Union) -> Self::Result;
  fn visit_intersection(&mut self, intersection: &Intersection) -> Self::Result;
  fn visit_list_type(&mut self, list_type: &ListType) -> Self::Result;
  fn visit_type_ref(&mut self, type_ref: &TypeRef) -> Self::Result;

  fn visit_primitive(&mut self, primitive: &Primitive) -> Self::Result;
}

/// An object that can specify it's start and end position and optionally the full range
/// Implementors of ranges only need to implement `start` ad `end`
pub trait Spanned {
  /// The starting line of this node
  fn start(&self) -> u32;

  /// The ending line of this node
  fn end(&self) -> u32;

  /// The full range of this node
  fn span(&self) -> Span {
    Span {
      start: self.start(),
      end: self.end(),
    }
  }
}

/// Representing the start and end of a node. Typically this would
/// be a multi line control flow or a function declaration
#[derive(Default, PartialEq, Eq, Debug, Copy, Clone)]
pub struct Span {
  pub start: u32,
  pub end: u32,
}

impl Span {
  pub fn new(start: u32, end: u32) -> Self {
    Self { start, end }
  }
}

impl From<Span> for Range<usize> {
  fn from(span: Span) -> Self {
    (span.start as usize)..(span.end as usize)
  }
}

pub struct Module<'a> {
  pub symbols: SymbolTable<'a>,
  pub decls: Vec<'a, Decl<'a>>,
}

impl<'a> Module<'a> {
  pub fn new(decls: Vec<'a, Decl<'a>>, symbols: SymbolTable<'a>) -> Self {
    Self { symbols, decls }
  }
}

impl<'a> Spanned for Module<'a> {
  fn start(&self) -> u32 {
    self.decls.first().map_or(0, |first| first.start())
  }

  fn end(&self) -> u32 {
    self.decls.last().map_or(0, |last| last.end())
  }
}

pub enum Decl<'a> {
  Symbol(Box<'a, Symbol<'a>>),
  Export(Box<'a, Symbol<'a>>),
  Stmt(Box<'a, Stmt<'a>>),
  Error(Box<'a, [Token<'a>]>),
}

impl<'a> Spanned for Decl<'a> {
  fn start(&self) -> u32 {
    match self {
      Decl::Symbol(symbol) => symbol.start(),
      Decl::Export(export) => export.start(),
      Decl::Stmt(stmt) => stmt.start(),
      Decl::Error(error) => error.first().map_or(0, |first| first.start()),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Decl::Symbol(symbol) => symbol.end(),
      Decl::Export(export) => export.end(),
      Decl::Stmt(stmt) => stmt.end(),
      Decl::Error(error) => error.last().map_or(0, |last| last.start()),
    }
  }
}

pub enum Symbol<'a> {
  Class(Box<'a, Class<'a>>),
  Fun(Fun<'a>),
  Let(Let<'a>),
  Trait(Trait<'a>),
  TypeDecl(TypeDecl<'a>),
}

impl<'a> Spanned for Symbol<'a> {
  fn start(&self) -> u32 {
    match self {
      Symbol::Class(class) => class.start(),
      Symbol::Fun(fun) => fun.start(),
      Symbol::Let(let_) => let_.start(),
      Symbol::Trait(trait_) => trait_.start(),
      Symbol::TypeDecl(type_) => type_.start(),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Symbol::Class(class) => class.end(),
      Symbol::Fun(fun) => fun.end(),
      Symbol::Let(let_) => let_.end(),
      Symbol::Trait(trait_) => trait_.end(),
      Symbol::TypeDecl(type_) => type_.end(),
    }
  }
}

pub struct Class<'a> {
  pub name: Token<'a>,
  pub range: Span,
  pub type_params: Vec<'a, TypeParam<'a>>,
  pub super_class: Option<ClassType<'a>>,
  pub type_members: Vec<'a, TypeMember<'a>>,
  pub symbols: SymbolTable<'a>,
  // pub fields:
  pub init: Option<Fun<'a>>,
  pub methods: Vec<'a, Fun<'a>>,
  pub static_methods: Vec<'a, Fun<'a>>,
}

impl<'a> Class<'a> {
  #[allow(clippy::too_many_arguments)]
  pub fn new(
    name: Token<'a>,
    range: Span,
    type_params: Vec<'a, TypeParam<'a>>,
    super_class: Option<ClassType<'a>>,
    type_members: Vec<'a, TypeMember<'a>>,
    symbols: SymbolTable<'a>,
    init: Option<Fun<'a>>,
    methods: Vec<'a, Fun<'a>>,
    static_methods: Vec<'a, Fun<'a>>,
  ) -> Self {
    Self {
      name,
      range,
      type_params,
      super_class,
      type_members,
      symbols,
      init,
      methods,
      static_methods,
    }
  }
}

impl<'a> Spanned for Class<'a> {
  fn span(&self) -> Span {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Fun<'a> {
  pub name: Option<Token<'a>>,
  pub call_sig: CallSignature<'a>,
  pub symbols: SymbolTable<'a>,
  pub body: FunBody<'a>,
}

impl<'a> Fun<'a> {
  pub fn new(
    name: Option<Token<'a>>,
    call_sig: CallSignature<'a>,
    symbols: SymbolTable<'a>,
    body: FunBody<'a>,
  ) -> Self {
    Self {
      name,
      call_sig,
      symbols,
      body,
    }
  }
}

impl<'a> Spanned for Fun<'a> {
  fn start(&self) -> u32 {
    match &self.name {
      Some(name) => name.start(),
      None => self.call_sig.start(),
    }
  }

  fn end(&self) -> u32 {
    match &self.body {
      FunBody::Block(block) => block.end(),
      FunBody::Expr(expr) => expr.end(),
    }
  }
}

pub enum FunBody<'a> {
  Block(Box<'a, Block<'a>>),
  Expr(Box<'a, Expr<'a>>),
}

pub struct Let<'a> {
  pub name: Token<'a>,
  pub type_: Option<Type<'a>>,
  pub value: Option<Expr<'a>>,
}

impl<'a> Let<'a> {
  pub fn new(name: Token<'a>, type_: Option<Type<'a>>, value: Option<Expr<'a>>) -> Self {
    Self { name, type_, value }
  }
}

impl<'a> Spanned for Let<'a> {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    match &self.value {
      Some(value) => value.end(),
      None => match &self.type_ {
        Some(type_) => type_.end(),
        None => self.name.end(),
      },
    }
  }
}

pub struct Trait<'a> {
  pub range: Span,
  pub name: Token<'a>,
  pub params: Vec<'a, TypeParam<'a>>,
  pub members: Vec<'a, TypeMember<'a>>,
  pub methods: Vec<'a, TypeMethod<'a>>,
}

impl<'a> Trait<'a> {
  pub fn new(
    range: Span,
    name: Token<'a>,
    params: Vec<'a, TypeParam<'a>>,
    members: Vec<'a, TypeMember<'a>>,
    methods: Vec<'a, TypeMethod<'a>>,
  ) -> Self {
    Self {
      range,
      name,
      params,
      members,
      methods,
    }
  }
}

impl<'a> Spanned for Trait<'a> {
  fn span(&self) -> Span {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct TypeDecl<'a> {
  pub name: Token<'a>,
  pub type_params: Vec<'a, TypeParam<'a>>,
  pub type_: Type<'a>,
}

impl<'a> TypeDecl<'a> {
  pub fn new(name: Token<'a>, type_params: Vec<'a, TypeParam<'a>>, type_: Type<'a>) -> Self {
    Self {
      name,
      type_params,
      type_,
    }
  }
}

impl<'a> Spanned for TypeDecl<'a> {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    self.type_.end()
  }
}

pub enum Stmt<'a> {
  Expr(Box<'a, Expr<'a>>),
  ImplicitReturn(Box<'a, Expr<'a>>),
  Import(Box<'a, Import<'a>>),
  For(Box<'a, For<'a>>),
  If(Box<'a, If<'a>>),
  Launch(Box<'a, Launch<'a>>),
  Raise(Box<'a, Raise<'a>>),
  Return(Box<'a, Return<'a>>),
  Continue(Box<'a, Token<'a>>),
  Break(Box<'a, Token<'a>>),
  While(Box<'a, While<'a>>),
  Try(Box<'a, Try<'a>>),
}

impl<'a> Spanned for Stmt<'a> {
  fn start(&self) -> u32 {
    match self {
      Stmt::Expr(expr) => expr.start(),
      Stmt::ImplicitReturn(expr) => expr.start(),
      Stmt::Import(import) => import.start(),
      Stmt::For(for_) => for_.start(),
      Stmt::If(if_) => if_.start(),
      Stmt::Launch(launch) => launch.start(),
      Stmt::Return(return_) => return_.start(),
      Stmt::Continue(continue_) => continue_.start(),
      Stmt::Break(break_) => break_.start(),
      Stmt::While(while_) => while_.start(),
      Stmt::Try(try_) => try_.start(),
      Stmt::Raise(raise) => raise.start(),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Stmt::Expr(expr) => expr.end(),
      Stmt::ImplicitReturn(expr) => expr.end(),
      Stmt::Import(import) => import.end(),
      Stmt::For(for_) => for_.end(),
      Stmt::If(if_) => if_.end(),
      Stmt::Launch(launch) => launch.end(),
      Stmt::Return(return_) => return_.end(),
      Stmt::Continue(continue_) => continue_.end(),
      Stmt::Break(break_) => break_.end(),
      Stmt::While(while_) => while_.end(),
      Stmt::Try(try_) => try_.end(),
      Stmt::Raise(raise) => raise.end(),
    }
  }
}

pub enum ImportStem<'a> {
  None,
  Rename(Token<'a>),
  Symbols(Vec<'a, ImportSymbol<'a>>),
}

pub struct ImportSymbol<'a> {
  pub symbol: Token<'a>,
  pub rename: Option<Token<'a>>,
}

impl<'a> ImportSymbol<'a> {
  pub fn new(symbol: Token<'a>, rename: Option<Token<'a>>) -> Self {
    Self { symbol, rename }
  }
}

impl<'a> Spanned for ImportSymbol<'a> {
  fn start(&self) -> u32 {
    self.symbol.start()
  }

  fn end(&self) -> u32 {
    self
      .rename
      .as_ref()
      .map(|rename| rename.end())
      .unwrap_or_else(|| self.symbol.end())
  }
}

pub struct Import<'a> {
  pub path: Vec<'a, Token<'a>>,
  pub stem: ImportStem<'a>,
}

impl<'a> Import<'a> {
  pub fn new(path: Vec<'a, Token<'a>>, stem: ImportStem<'a>) -> Self {
    assert!(!path.is_empty());
    Self { path, stem }
  }

  pub fn path(&self) -> &[Token<'a>] {
    &self.path
  }
}

impl<'a> Spanned for Import<'a> {
  fn start(&self) -> u32 {
    self.path.first().unwrap().start()
  }

  fn end(&self) -> u32 {
    match &self.stem {
      ImportStem::None => self.path.last().unwrap().end(),
      ImportStem::Rename(rename) => rename.end(),
      ImportStem::Symbols(symbols) => {
        if symbols.is_empty() {
          self.path.last().unwrap().end()
        } else {
          symbols.last().unwrap().end()
        }
      }
    }
  }
}

pub struct For<'a> {
  pub item: Token<'a>,
  pub iter: Expr<'a>,
  pub symbols: SymbolTable<'a>,
  pub body: Block<'a>,
}

impl<'a> For<'a> {
  pub fn new(item: Token<'a>, iter: Expr<'a>, symbols: SymbolTable<'a>, body: Block<'a>) -> Self {
    Self {
      item,
      iter,
      symbols,
      body,
    }
  }
}

impl<'a> Spanned for For<'a> {
  fn start(&self) -> u32 {
    self.item.start()
  }

  fn end(&self) -> u32 {
    self.body.end()
  }
}

pub struct If<'a> {
  pub cond: Expr<'a>,
  pub body: Block<'a>,
  pub else_: Option<Else<'a>>,
}

impl<'a> If<'a> {
  pub fn new(cond: Expr<'a>, body: Block<'a>, else_: Option<Else<'a>>) -> Self {
    Self { cond, body, else_ }
  }
}

impl<'a> Spanned for If<'a> {
  fn start(&self) -> u32 {
    self.cond.start()
  }

  fn end(&self) -> u32 {
    self.else_.as_ref().map_or_else(
      || self.cond.end(),
      |else_| match else_ {
        Else::If(if_) => if_.end(),
        Else::Block(block) => block.end(),
      },
    )
  }
}

pub enum Else<'a> {
  If(Box<'a, If<'a>>),
  Block(Block<'a>),
}

pub struct Launch<'a> {
  pub closure: Expr<'a>,
}

impl<'a> Launch<'a> {
  pub fn new(closure: Expr<'a>) -> Self {
    Self { closure }
  }
}

impl<'a> Spanned for Launch<'a> {
  fn start(&self) -> u32 {
    self.closure.start()
  }

  fn end(&self) -> u32 {
    self.closure.end()
  }
}


pub struct Raise<'a> {
  pub error: Expr<'a>,
}

impl<'a> Raise<'a> {
  pub fn new(error: Expr<'a>) -> Self {
    Self { error }
  }
}

impl<'a> Spanned for Raise<'a> {
  fn start(&self) -> u32 {
    self.error.start()
  }

  fn end(&self) -> u32 {
    self.error.end()
  }
}

pub struct Return<'a> {
  pub return_: Token<'a>,
  pub value: Option<Expr<'a>>,
}

impl<'a> Return<'a> {
  pub fn new(return_: Token<'a>, value: Option<Expr<'a>>) -> Self {
    Self { return_, value }
  }
}

impl<'a> Spanned for Return<'a> {
  fn start(&self) -> u32 {
    self.return_.start()
  }

  fn end(&self) -> u32 {
    self
      .value
      .as_ref()
      .map_or_else(|| self.return_.end(), |value| value.end())
  }
}

pub struct While<'a> {
  pub cond: Expr<'a>,
  pub body: Block<'a>,
}

impl<'a> While<'a> {
  pub fn new(cond: Expr<'a>, body: Block<'a>) -> Self {
    Self { cond, body }
  }
}

impl<'a> Spanned for While<'a> {
  fn start(&self) -> u32 {
    self.cond.start()
  }

  fn end(&self) -> u32 {
    self.body.end()
  }
}

pub struct Try<'a> {
  pub block: Block<'a>,
  pub catch: Block<'a>,
}

impl<'a> Try<'a> {
  pub fn new(block: Block<'a>, catch: Block<'a>) -> Self {
    Self { block, catch }
  }
}

impl<'a> Spanned for Try<'a> {
  fn start(&self) -> u32 {
    self.block.start()
  }

  fn end(&self) -> u32 {
    self.block.end()
  }
}

pub struct Block<'a> {
  pub range: Span,
  pub symbols: SymbolTable<'a>,
  pub decls: Vec<'a, Decl<'a>>,
}

impl<'a> Block<'a> {
  pub fn new(range: Span, symbols: SymbolTable<'a>, decls: Vec<'a, Decl<'a>>) -> Self {
    Self {
      range,
      symbols,
      decls,
    }
  }
}

impl<'a> Spanned for Block<'a> {
  fn span(&self) -> Span {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct CallSignature<'a> {
  pub range: Span,
  pub type_params: Vec<'a, TypeParam<'a>>,
  pub params: Vec<'a, Param<'a>>,
  pub return_type: Option<Type<'a>>,
}

impl<'a> CallSignature<'a> {
  pub fn new(
    range: Span,
    type_params: Vec<'a, TypeParam<'a>>,
    params: Vec<'a, Param<'a>>,
    return_type: Option<Type<'a>>,
  ) -> Self {
    Self {
      range,
      type_params,
      params,
      return_type,
    }
  }
}

impl<'a> Spanned for CallSignature<'a> {
  fn span(&self) -> Span {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Param<'a> {
  pub name: Token<'a>,
  pub type_: Option<Type<'a>>,
}

impl<'a> Param<'a> {
  pub fn new(name: Token<'a>, type_: Option<Type<'a>>) -> Self {
    Self { name, type_ }
  }
}

impl<'a> Spanned for Param<'a> {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    self
      .type_
      .as_ref()
      .map_or_else(|| self.name.end(), |type_| type_.end())
  }
}

pub enum Expr<'a> {
  Assign(Box<'a, Assign<'a>>),
  AssignBinary(Box<'a, AssignBinary<'a>>),
  Atom(Box<'a, Atom<'a>>),
  Binary(Box<'a, Binary<'a>>),
  Ternary(Box<'a, Ternary<'a>>),
  Send(Box<'a, Send<'a>>),
  Unary(Box<'a, Unary<'a>>),
}

impl<'a> Spanned for Expr<'a> {
  fn start(&self) -> u32 {
    match self {
      Expr::Assign(assign) => assign.start(),
      Expr::AssignBinary(assign_binary) => assign_binary.start(),
      Expr::Atom(atom) => atom.start(),
      Expr::Binary(binary) => binary.start(),
      Expr::Ternary(ternary) => ternary.start(),
      Expr::Send(drain) => drain.start(),
      Expr::Unary(unary) => unary.start(),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Expr::Assign(assign) => assign.end(),
      Expr::AssignBinary(assign_binary) => assign_binary.end(),
      Expr::Atom(atom) => atom.end(),
      Expr::Binary(binary) => binary.end(),
      Expr::Ternary(ternary) => ternary.end(),
      Expr::Send(drain) => drain.end(),
      Expr::Unary(unary) => unary.end(),
    }
  }
}

pub struct Assign<'a> {
  pub lhs: Expr<'a>,
  pub rhs: Expr<'a>,
}

impl<'a> Assign<'a> {
  pub fn new(lhs: Expr<'a>, rhs: Expr<'a>) -> Self {
    Self { lhs, rhs }
  }
}

impl<'a> Spanned for Assign<'a> {
  fn start(&self) -> u32 {
    self.lhs.start()
  }

  fn end(&self) -> u32 {
    self.rhs.end()
  }
}

pub struct Send<'a> {
  pub lhs: Expr<'a>,
  pub rhs: Expr<'a>,
}

impl<'a> Send<'a> {
  pub fn new(lhs: Expr<'a>, rhs: Expr<'a>) -> Self {
    Self { lhs, rhs }
  }
}

impl<'a> Spanned for Send<'a> {
  fn start(&self) -> u32 {
    self.lhs.start()
  }

  fn end(&self) -> u32 {
    self.rhs.end()
  }
}

pub struct Ternary<'a> {
  pub cond: Expr<'a>,
  pub then: Expr<'a>,
  pub else_: Expr<'a>,
}

impl<'a> Ternary<'a> {
  pub fn new(cond: Expr<'a>, then: Expr<'a>, else_: Expr<'a>) -> Ternary<'a> {
    Self { cond, then, else_ }
  }
}

impl<'a> Spanned for Ternary<'a> {
  fn start(&self) -> u32 {
    self.cond.start()
  }

  fn end(&self) -> u32 {
    self.else_.end()
  }
}

pub enum AssignBinaryOp {
  Add,
  Sub,
  Mul,
  Div,
}

pub struct AssignBinary<'a> {
  pub lhs: Expr<'a>,
  pub op: AssignBinaryOp,
  pub rhs: Expr<'a>,
}

impl<'a> AssignBinary<'a> {
  pub fn new(lhs: Expr<'a>, op: AssignBinaryOp, rhs: Expr<'a>) -> Self {
    Self { lhs, op, rhs }
  }
}

impl<'a> Spanned for AssignBinary<'a> {
  fn start(&self) -> u32 {
    self.lhs.start()
  }

  fn end(&self) -> u32 {
    self.rhs.end()
  }
}

pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Lt,
  LtEq,
  Gt,
  GtEq,
  Eq,
  Ne,
  And,
  Or,
}

pub struct Binary<'a> {
  pub op: BinaryOp,
  pub lhs: Expr<'a>,
  pub rhs: Expr<'a>,
}

impl<'a> Binary<'a> {
  pub fn new(op: BinaryOp, lhs: Expr<'a>, rhs: Expr<'a>) -> Binary<'a> {
    Self { op, lhs, rhs }
  }
}

impl<'a> Spanned for Binary<'a> {
  fn start(&self) -> u32 {
    self.lhs.start()
  }

  fn end(&self) -> u32 {
    self.rhs.end()
  }
}

pub enum UnaryOp {
  Not,
  Negate,
  Receive,
}

pub struct Unary<'a> {
  pub op: UnaryOp,
  pub expr: Expr<'a>,
}

impl<'a> Unary<'a> {
  pub fn new(op: UnaryOp, expr: Expr<'a>) -> Self {
    Self { op, expr }
  }
}

impl<'a> Spanned for Unary<'a> {
  fn span(&self) -> Span {
    self.expr.span()
  }

  fn start(&self) -> u32 {
    self.expr.start()
  }

  fn end(&self) -> u32 {
    self.expr.end()
  }
}

pub struct Atom<'a> {
  pub primary: Primary<'a>,
  pub trailers: Vec<'a, Trailer<'a>>,
}

impl<'a> Atom<'a> {
  pub fn new(primary: Primary<'a>, trailers: Vec<'a, Trailer<'a>>) -> Self {
    Self { primary, trailers }
  }
}

impl<'a> Spanned for Atom<'a> {
  fn start(&self) -> u32 {
    self.primary.start()
  }

  fn end(&self) -> u32 {
    self
      .trailers
      .last()
      .map_or_else(|| self.primary.end(), |last| last.end())
  }
}

pub enum Trailer<'a> {
  Call(Box<'a, Call<'a>>),
  Index(Box<'a, Index<'a>>),
  Access(Box<'a, Access<'a>>),
}

impl<'a> Spanned for Trailer<'a> {
  fn start(&self) -> u32 {
    match self {
      Trailer::Call(call) => call.start(),
      Trailer::Index(index) => index.start(),
      Trailer::Access(access) => access.start(),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Trailer::Call(call) => call.end(),
      Trailer::Index(index) => index.end(),
      Trailer::Access(access) => access.end(),
    }
  }
}

pub struct Call<'a> {
  pub range: Span,
  pub args: Vec<'a, Expr<'a>>,
}

impl<'a> Call<'a> {
  pub fn new(range: Span, args: Vec<'a, Expr<'a>>) -> Self {
    Self { range, args }
  }
}

impl<'a> Spanned for Call<'a> {
  fn span(&self) -> Span {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Index<'a> {
  pub index: Expr<'a>,
}

impl<'a> Index<'a> {
  pub fn new(index: Expr<'a>) -> Self {
    Self { index }
  }
}

impl<'a> Spanned for Index<'a> {
  fn span(&self) -> Span {
    self.index.span()
  }

  fn start(&self) -> u32 {
    self.index.start()
  }

  fn end(&self) -> u32 {
    self.index.end()
  }
}

pub struct Access<'a> {
  pub prop: Token<'a>,
}

impl<'a> Access<'a> {
  pub fn new(prop: Token<'a>) -> Self {
    Self { prop }
  }
}

impl<'a> Spanned for Access<'a> {
  fn start(&self) -> u32 {
    self.prop.start()
  }

  fn end(&self) -> u32 {
    self.prop.end()
  }
}

pub enum Primary<'a> {
  Channel(Channel<'a>),
  False(Token<'a>),
  Grouping(Box<'a, Expr<'a>>),
  Tuple(Collection<'a>),
  Ident(Token<'a>),
  InstanceAccess(InstanceAccess<'a>),
  Interpolation(Box<'a, Interpolation<'a>>),
  Lambda(Box<'a, Fun<'a>>),
  List(Collection<'a>),
  Map(Map<'a>),
  Nil(Token<'a>),
  Number(Token<'a>),
  Self_(Token<'a>),
  String(Token<'a>),
  Super(Super<'a>),
  True(Token<'a>),
}

impl<'a> Spanned for Primary<'a> {
  fn start(&self) -> u32 {
    match self {
      Primary::Channel(channel) => channel.start(),
      Primary::False(false_) => false_.start(),
      Primary::Grouping(grouping) => grouping.start(),
      Primary::Ident(ident) => ident.start(),
      Primary::InstanceAccess(instance_access) => instance_access.start(),
      Primary::Interpolation(string) => string.start(),
      Primary::Lambda(lambda) => lambda.start(),
      Primary::List(list) => list.start(),
      Primary::Tuple(tuple) => tuple.start(),
      Primary::Map(map) => map.start(),
      Primary::Nil(nil_) => nil_.start(),
      Primary::Number(nil_) => nil_.start(),
      Primary::Self_(self_) => self_.start(),
      Primary::String(string) => string.start(),
      Primary::Super(super_) => super_.start(),
      Primary::True(true_) => true_.start(),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Primary::Channel(channel) => channel.end(),
      Primary::False(false_) => false_.end(),
      Primary::Grouping(grouping) => grouping.end(),
      Primary::Ident(ident) => ident.end(),
      Primary::InstanceAccess(instance_access) => instance_access.end(),
      Primary::Interpolation(string) => string.end(),
      Primary::Lambda(lambda) => lambda.end(),
      Primary::List(list) => list.end(),
      Primary::Tuple(tuple) => tuple.end(),
      Primary::Map(map) => map.end(),
      Primary::Nil(nil_) => nil_.end(),
      Primary::Number(nil_) => nil_.end(),
      Primary::Self_(self_) => self_.end(),
      Primary::String(string) => string.end(),
      Primary::Super(super_) => super_.end(),
      Primary::True(true_) => true_.end(),
    }
  }
}

pub struct Interpolation<'a> {
  pub start: Token<'a>,
  pub segments: Vec<'a, StringSegments<'a>>,
  pub end: Token<'a>,
}

pub enum StringSegments<'a> {
  Token(Token<'a>),
  Expr(Box<'a, Expr<'a>>),
}

impl<'a> Interpolation<'a> {
  pub fn new(start: Token<'a>, segments: Vec<'a, StringSegments<'a>>, end: Token<'a>) -> Self {
    Self {
      start,
      segments,
      end,
    }
  }
}

impl<'a> Spanned for Interpolation<'a> {
  fn start(&self) -> u32 {
    self.start.start()
  }

  fn end(&self) -> u32 {
    self.end.end()
  }
}

pub struct Collection<'a> {
  pub range: Span,
  pub items: Vec<'a, Expr<'a>>,
}

impl<'a> Collection<'a> {
  pub fn new(range: Span, items: Vec<'a, Expr<'a>>) -> Self {
    Self { range, items }
  }
}

impl<'a> Spanned for Collection<'a> {
  fn span(&self) -> Span {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Map<'a> {
  pub range: Span,
  pub entries: Vec<'a, (Expr<'a>, Expr<'a>)>,
}

impl<'a> Map<'a> {
  pub fn new(range: Span, kvp: Vec<'a, (Expr<'a>, Expr<'a>)>) -> Self {
    Self {
      range,
      entries: kvp,
    }
  }
}

impl<'a> Spanned for Map<'a> {
  fn span(&self) -> Span {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Super<'a> {
  pub super_: Token<'a>,
  pub access: Token<'a>,
}

impl<'a> Super<'a> {
  pub fn new(super_: Token<'a>, access: Token<'a>) -> Self {
    Self { super_, access }
  }
}

impl<'a> Spanned for Super<'a> {
  fn start(&self) -> u32 {
    self.super_.start()
  }

  fn end(&self) -> u32 {
    self.access.end()
  }
}

pub struct Channel<'a> {
  pub span: Span,
  pub expr: Option<Expr<'a>>,
}

impl<'a> Channel<'a> {
  pub fn new(span: Span, expr: Option<Expr<'a>>) -> Self {
    Self { span, expr }
  }
}

impl<'a> Spanned for Channel<'a> {
  fn start(&self) -> u32 {
    self.span.start
  }

  fn end(&self) -> u32 {
    self.span.end
  }
}

pub struct InstanceAccess<'a> {
  pub access: Token<'a>,
}

impl<'a> InstanceAccess<'a> {
  pub fn new(access: Token<'a>) -> Self {
    Self { access }
  }

  pub fn property(&self) -> &str {
    &self.access.str()[1..]
  }
}

impl<'a> Spanned for InstanceAccess<'a> {
  fn start(&self) -> u32 {
    self.access.start()
  }

  fn end(&self) -> u32 {
    self.access.end()
  }
}



pub struct TypeParam<'a> {
  pub name: Token<'a>,
  pub constraint: Option<Type<'a>>,
}

impl<'a> TypeParam<'a> {
  pub fn new(name: Token<'a>, constraint: Option<Type<'a>>) -> Self {
    Self { name, constraint }
  }
}

impl<'a> Spanned for TypeParam<'a> {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    self
      .constraint
      .as_ref()
      .map_or_else(|| self.name.end(), |constraint| constraint.span().end)
  }
}

pub enum Type<'a> {
  Union(Box<'a, Union<'a>>),
  Intersection(Box<'a, Intersection<'a>>),
  Fun(Box<'a, CallSignature<'a>>),
  List(Box<'a, ListType<'a>>),
  Ref(Box<'a, TypeRef<'a>>),
  Primitive(Primitive<'a>),
}

impl<'a> Spanned for Type<'a> {
  fn start(&self) -> u32 {
    match self {
      Type::Union(union) => union.start(),
      Type::Intersection(intersection) => intersection.start(),
      Type::Fun(fun) => fun.start(),
      Type::List(list) => list.start(),
      Type::Ref(ref_) => ref_.start(),
      Type::Primitive(primitive) => primitive.start(),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Type::Union(union) => union.end(),
      Type::Intersection(intersection) => intersection.end(),
      Type::Fun(fun) => fun.end(),
      Type::List(list) => list.end(),
      Type::Ref(ref_) => ref_.end(),
      Type::Primitive(primitive) => primitive.end(),
    }
  }
}

pub struct ClassType<'a> {
  pub type_ref: TypeRef<'a>,
}

impl<'a> ClassType<'a> {
  pub fn new(type_ref: TypeRef<'a>) -> Self {
    Self { type_ref }
  }
}

impl<'a> Spanned for ClassType<'a> {
  fn start(&self) -> u32 {
    self.type_ref.start()
  }

  fn end(&self) -> u32 {
    self.type_ref.end()
  }
}

pub struct TypeRef<'a> {
  pub name: Token<'a>,
  pub type_args: Vec<'a, Type<'a>>,
}

impl<'a> TypeRef<'a> {
  pub fn new(name: Token<'a>, type_args: Vec<'a, Type<'a>>) -> Self {
    Self { name, type_args }
  }
}

impl<'a> Spanned for TypeRef<'a> {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    self
      .type_args
      .last()
      .map_or_else(|| self.name.end(), |arg| arg.end())
  }
}

pub struct TypeMember<'a> {
  pub name: Token<'a>,
  pub type_: Type<'a>,
}

impl<'a> TypeMember<'a> {
  pub fn new(name: Token<'a>, type_: Type<'a>) -> Self {
    Self { name, type_ }
  }
}

impl<'a> Spanned for TypeMember<'a> {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    self.type_.end()
  }
}

pub struct TypeMethod<'a> {
  pub name: Token<'a>,
  pub call_sig: CallSignature<'a>,
}

impl<'a> TypeMethod<'a> {
  pub fn new(name: Token<'a>, call_sig: CallSignature<'a>) -> Self {
    Self { name, call_sig }
  }
}

impl<'a> Spanned for TypeMethod<'a> {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    self.call_sig.end()
  }
}

pub struct Union<'a> {
  pub lhs: Type<'a>,
  pub rhs: Type<'a>,
}

impl<'a> Union<'a> {
  pub fn new(lhs: Type<'a>, rhs: Type<'a>) -> Self {
    Self { lhs, rhs }
  }
}

impl<'a> Spanned for Union<'a> {
  fn start(&self) -> u32 {
    self.lhs.start()
  }

  fn end(&self) -> u32 {
    self.rhs.end()
  }
}

pub struct Intersection<'a> {
  pub lhs: Type<'a>,
  pub rhs: Type<'a>,
}

impl<'a> Intersection<'a> {
  pub fn new(lhs: Type<'a>, rhs: Type<'a>) -> Self {
    Self { lhs, rhs }
  }
}

impl<'a> Spanned for Intersection<'a> {
  fn start(&self) -> u32 {
    self.lhs.start()
  }

  fn end(&self) -> u32 {
    self.rhs.end()
  }
}

pub struct ListType<'a> {
  pub item_type: Type<'a>,
}

impl<'a> ListType<'a> {
  pub fn new(item_type: Type<'a>) -> Self {
    Self { item_type }
  }
}

impl<'a> Spanned for ListType<'a> {
  fn start(&self) -> u32 {
    self.item_type.start()
  }

  fn end(&self) -> u32 {
    self.item_type.end()
  }
}

pub enum Primitive<'a> {
  Nil(Token<'a>),
  Number(Token<'a>),
  Bool(Token<'a>),
  String(Token<'a>),
  Any(Token<'a>),
}

impl<'a> Spanned for Primitive<'a> {
  fn start(&self) -> u32 {
    match self {
      Primitive::Nil(t) => t,
      Primitive::Number(t) => t,
      Primitive::Bool(t) => t,
      Primitive::String(t) => t,
      Primitive::Any(t) => t,
    }
    .start()
  }

  fn end(&self) -> u32 {
    match self {
      Primitive::Nil(t) => t,
      Primitive::Number(t) => t,
      Primitive::Bool(t) => t,
      Primitive::String(t) => t,
      Primitive::Any(t) => t,
    }
    .end()
  }
}
