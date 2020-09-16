use crate::token::Token;

/// A visitor pattern for the Laythe ast. 
/// Not sure if this currently provides any value as enum 
/// already know there variants. May still be useful 
/// to ensure each node has cold associated with it
pub trait Visitor {
  type Result;

  fn visit(&mut self, module: &Module) -> Self::Result;

  fn visit_decl(&mut self, decl: &Decl) -> Self::Result;
  fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Result;
  fn visit_expr(&mut self, expr: &Expr) -> Self::Result;
  fn visit_primary(&mut self, primary: &Primary) -> Self::Result;

  fn visit_symbol(&mut self, symbol: &Symbol) -> Self::Result;
  fn visit_export(&mut self, export: &Symbol) -> Self::Result;
  fn visit_error(&mut self, error: &[Token]) -> Self::Result;

  fn visit_class(&mut self, class: &Class) -> Self::Result;
  fn visit_fun(&mut self, fun: &Fun) -> Self::Result;
  fn visit_let(&mut self, let_: &Let) -> Self::Result;
  fn visit_method(&mut self, method: &Fun) -> Self::Result;
  fn visit_static_method(&mut self, static_method: &Fun) -> Self::Result;

  fn visit_import(&mut self, import: &Import) -> Self::Result;
  fn visit_for(&mut self, for_: &For) -> Self::Result;
  fn visit_while(&mut self, while_: &While) -> Self::Result;
  fn visit_if(&mut self, if_: &If) -> Self::Result;
  fn visit_return(&mut self, return_: &Return) -> Self::Result;
  fn visit_try(&mut self, try_: &Try) -> Self::Result;
  fn visit_block(&mut self, block: &Block) -> Self::Result;

  fn visit_assign(&mut self, assign: &Assign) -> Self::Result;
  fn visit_binary(&mut self, binary: &Binary) -> Self::Result;
  fn visit_unary(&mut self, unary: &Unary) -> Self::Result;
  fn visit_atom(&mut self, atom: &Atom) -> Self::Result;

  fn visit_index(&mut self, index: &Index) -> Self::Result;
  fn visit_call(&mut self, call: &Call) -> Self::Result;
  fn visit_access(&mut self, access: &Access) -> Self::Result;
  fn visit_call_sig(&mut self, call_sig: &CallSignature) -> Self::Result;

  fn visit_assign_block(&mut self, block: &Block) -> Self::Result;
  fn visit_true(&mut self, token: &Token) -> Self::Result;
  fn visit_false(&mut self, token: &Token) -> Self::Result;
  fn visit_nil(&mut self, token: &Token) -> Self::Result;
  fn visit_number(&mut self, token: &Token) -> Self::Result;
  fn visit_string(&mut self, token: &Token) -> Self::Result;
  fn visit_ident(&mut self, token: &Token) -> Self::Result;
  fn visit_self(&mut self, token: &Token) -> Self::Result;
  fn visit_super(&mut self, token: &Super) -> Self::Result;
  fn visit_lambda(&mut self, fun: &Fun) -> Self::Result;
  fn visit_list(&mut self, items: &List) -> Self::Result;
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
  fn visit_union(&mut self, union: &Union) -> Self::Result;
  fn visit_intersection(&mut self, intersection: &Intersection) -> Self::Result;
  fn visit_list_type(&mut self, list_type: &ListType) -> Self::Result;
  fn visit_type_ref(&mut self, type_ref: &TypeRef) -> Self::Result;

  fn visit_primitive(&mut self, primitive: &Primitive) -> Self::Result;
}

/// An object that can specify it's start and end position and optionally the full range
/// Implementors of ranges only need to implement `start` ad `end`
pub trait Ranged {
  /// The starting line of this node
  fn start(&self) -> u32;

  /// The ending line of this node
  fn end(&self) -> u32;

  /// The full range of this node
  fn range(&self) -> Range {
    Range {
      start: self.start(),
      end: self.end(),
    }
  }
}

/// Representing the start and end of a node. Typically this would
/// be a multi line control flow or a function declaration
#[derive(Default, Copy, Clone)]
pub struct Range {
  pub start: u32,
  pub end: u32,
}

#[derive(Default)]
pub struct Module {
  pub decls: Vec<Decl>,
}

impl Module {
  pub fn new(decls: Vec<Decl>) -> Self {
    Self { decls }
  }
}

impl Ranged for Module {
  fn start(&self) -> u32 {
    self.decls.first().map_or(1, |first| first.start())
  }

  fn end(&self) -> u32 {
    self.decls.last().map_or(1, |last| last.end())
  }
}

pub enum Decl {
  Symbol(Box<Symbol>),
  Export(Box<Symbol>),
  Stmt(Box<Stmt>),
  Error(Box<[Token]>),
}

impl Ranged for Decl {
  fn start(&self) -> u32 {
    match self {
      Decl::Symbol(symbol) => symbol.start(),
      Decl::Export(export) => export.start(),
      Decl::Stmt(stmt) => stmt.start(),
      Decl::Error(error) => error.first().map_or(0, |first| first.line),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Decl::Symbol(symbol) => symbol.end(),
      Decl::Export(export) => export.end(),
      Decl::Stmt(stmt) => stmt.end(),
      Decl::Error(error) => error.last().map_or(0, |last| last.line),
    }
  }
}

pub enum Symbol {
  Class(Class),
  Fun(Fun),
  Let(Let),
  Trait(Trait),
  TypeDecl(TypeDecl),
}

impl Ranged for Symbol {
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

pub struct Class {
  pub name: Option<Token>,
  pub range: Range,
  pub type_params: Vec<TypeParam>,
  pub super_class: Option<ClassType>,
  pub type_members: Vec<TypeMember>,
  pub init: Option<Fun>,
  pub methods: Vec<Fun>,
  pub static_methods: Vec<Fun>,
}

impl Class {
  pub fn new(
    name: Option<Token>,
    range: Range,
    type_params: Vec<TypeParam>,
    super_class: Option<ClassType>,
    type_members: Vec<TypeMember>,
    init: Option<Fun>,
    methods: Vec<Fun>,
    static_methods: Vec<Fun>,
  ) -> Self {
    Self {
      name,
      range,
      type_params,
      super_class,
      type_members,
      init,
      methods,
      static_methods,
    }
  }
}

impl Ranged for Class {
  fn range(&self) -> Range {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Fun {
  pub name: Option<Token>,
  pub call_sig: CallSignature,
  pub body: FunBody,
}

impl Fun {
  pub fn new(name: Option<Token>, call_sig: CallSignature, body: FunBody) -> Self {
    Self {
      name,
      call_sig,
      body,
    }
  }
}

impl Ranged for Fun {
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

pub enum FunBody {
  Block(Box<Block>),
  Expr(Box<Expr>),
}

pub struct Let {
  pub name: Token,
  pub type_: Option<Type>,
  pub value: Option<Expr>,
}

impl Let {
  pub fn new(name: Token, type_: Option<Type>, value: Option<Expr>) -> Self {
    Self { name, type_, value }
  }
}

impl Ranged for Let {
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

pub struct Trait {
  pub range: Range,
  pub name: Token,
  pub params: Vec<TypeParam>,
  pub members: Vec<TypeMember>,
}

impl Trait {
  pub fn new(range: Range, name: Token, params: Vec<TypeParam>, members: Vec<TypeMember>) -> Self {
    Self {
      range,
      name,
      params,
      members,
    }
  }
}

impl Ranged for Trait {
  fn range(&self) -> Range {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct TypeDecl {
  pub name: Token,
  pub type_params: Vec<TypeParam>,
  pub type_: Type,
}

impl TypeDecl {
  pub fn new(name: Token, type_params: Vec<TypeParam>, type_: Type) -> Self {
    Self {
      name,
      type_params,
      type_,
    }
  }
}

impl Ranged for TypeDecl {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    self.type_.end()
  }
}

pub enum Stmt {
  Expr(Box<Expr>),
  Import(Box<Import>),
  For(Box<For>),
  If(Box<If>),
  Return(Box<Return>),
  While(Box<While>),
  Try(Box<Try>),
}

impl Ranged for Stmt {
  fn start(&self) -> u32 {
    match self {
      Stmt::Expr(expr) => expr.start(),
      Stmt::Import(import) => import.start(),
      Stmt::For(for_) => for_.start(),
      Stmt::If(if_) => if_.start(),
      Stmt::Return(return_) => return_.start(),
      Stmt::While(while_) => while_.start(),
      Stmt::Try(try_) => try_.start(),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Stmt::Expr(expr) => expr.end(),
      Stmt::Import(import) => import.end(),
      Stmt::For(for_) => for_.end(),
      Stmt::If(if_) => if_.end(),
      Stmt::Return(return_) => return_.end(),
      Stmt::While(while_) => while_.end(),
      Stmt::Try(try_) => try_.end(),
    }
  }
}

pub struct Import {
  pub imported: Token,
  pub path: Token,
}

impl Import {
  pub fn new(imported: Token, path: Token) -> Self {
    Self { imported, path }
  }
}

impl Ranged for Import {
  fn start(&self) -> u32 {
    self.imported.start()
  }

  fn end(&self) -> u32 {
    self.path.end()
  }
}

pub struct For {
  pub item: Token,
  pub iter: Expr,
  pub body: Block,
}

impl For {
  pub fn new(item: Token, iter: Expr, body: Block) -> Self {
    Self { item, iter, body }
  }
}

impl Ranged for For {
  fn start(&self) -> u32 {
    self.item.start()
  }

  fn end(&self) -> u32 {
    self.body.end()
  }
}

pub struct If {
  pub cond: Expr,
  pub body: Block,
  pub else_: Option<Else>,
}

impl If {
  pub fn new(cond: Expr, body: Block, else_body: Option<Else>) -> Self {
    Self {
      cond,
      body,
      else_: else_body,
    }
  }
}

impl Ranged for If {
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

pub enum Else {
  If(Box<If>),
  Block(Block),
}

pub struct Return {
  pub return_: Token,
  pub value: Option<Expr>,
}

impl Return {
  pub fn new(return_: Token, value: Option<Expr>) -> Self {
    Self { return_, value }
  }
}

impl Ranged for Return {
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

pub struct While {
  pub cond: Expr,
  pub body: Block,
}

impl While {
  pub fn new(cond: Expr, body: Block) -> Self {
    Self { cond, body }
  }
}

impl Ranged for While {
  fn start(&self) -> u32 {
    self.cond.start()
  }

  fn end(&self) -> u32 {
    self.body.end()
  }
}

pub struct Try {
  pub block: Block,
  pub catch: Block,
}

impl Try {
  pub fn new(block: Block, catch: Block) -> Self {
    Self { block, catch }
  }
}

impl Ranged for Try {
  fn start(&self) -> u32 {
    self.block.start()
  }

  fn end(&self) -> u32 {
    self.block.end()
  }
}

pub struct Block {
  pub range: Range,
  pub decls: Vec<Decl>,
}

impl Block {
  pub fn new(range: Range, decls: Vec<Decl>) -> Self {
    Self { range, decls }
  }
}

impl Ranged for Block {
  fn range(&self) -> Range {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

#[derive(Default)]
pub struct CallSignature {
  pub range: Range,
  pub type_params: Vec<TypeParam>,
  pub params: Vec<Param>,
  pub return_type: Option<Type>,
}

impl CallSignature {
  pub fn new(
    range: Range,
    type_params: Vec<TypeParam>,
    params: Vec<Param>,
    return_type: Option<Type>,
  ) -> Self {
    Self {
      range,
      type_params,
      params,
      return_type,
    }
  }
}

impl Ranged for CallSignature {
  fn range(&self) -> Range {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Param {
  pub name: Token,
  pub type_: Option<Type>,
}

impl Param {
  pub fn new(name: Token, type_: Option<Type>) -> Self {
    Self { name, type_ }
  }
}

impl Ranged for Param {
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

pub enum Expr {
  Assign(Box<Assign>),
  Binary(Box<Binary>),
  Unary(Box<Unary>),
  Atom(Box<Atom>),
}

impl Ranged for Expr {
  fn start(&self) -> u32 {
    match self {
      Expr::Assign(assign) => assign.start(),
      Expr::Binary(binary) => binary.start(),
      Expr::Unary(unary) => unary.start(),
      Expr::Atom(atom) => atom.start(),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Expr::Assign(assign) => assign.end(),
      Expr::Binary(binary) => binary.end(),
      Expr::Unary(unary) => unary.end(),
      Expr::Atom(atom) => atom.end(),
    }
  }
}

pub struct Assign {
  pub lhs: Expr,
  pub rhs: Expr,
}

impl Assign {
  pub fn new(lhs: Expr, rhs: Expr) -> Self {
    Self { lhs, rhs }
  }
}

impl Ranged for Assign {
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
  Multi,
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

pub struct Binary {
  pub op: BinaryOp,
  pub lhs: Expr,
  pub rhs: Expr,
}

impl Binary {
  pub fn new(op: BinaryOp, lhs: Expr, rhs: Expr) -> Binary {
    Self { op, lhs, rhs }
  }
}

impl Ranged for Binary {
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
}

pub struct Unary {
  pub op: UnaryOp,
  pub expr: Expr,
}

impl Unary {
  pub fn new(op: UnaryOp, expr: Expr) -> Self {
    Self { op, expr }
  }
}

impl Ranged for Unary {
  fn range(&self) -> Range {
    self.expr.range()
  }

  fn start(&self) -> u32 {
    self.expr.start()
  }

  fn end(&self) -> u32 {
    self.expr.end()
  }
}

pub struct Atom {
  pub primary: Primary,
  pub trailers: Vec<Trailer>,
}

impl Atom {
  pub fn new(primary: Primary) -> Self {
    Self {
      primary,
      trailers: vec![],
    }
  }
}

impl Ranged for Atom {
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

pub enum Trailer {
  Call(Box<Call>),
  Index(Box<Index>),
  Access(Box<Access>),
}

impl Ranged for Trailer {
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

pub struct Call {
  pub range: Range,
  pub args: Vec<Expr>,
}

impl Call {
  pub fn new(range: Range, args: Vec<Expr>) -> Self {
    Self { range, args }
  }
}

impl Ranged for Call {
  fn range(&self) -> Range {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Index {
  pub index: Expr,
}

impl Index {
  pub fn new(index: Expr) -> Self {
    Self { index }
  }
}

impl Ranged for Index {
  fn range(&self) -> Range {
    self.index.range()
  }

  fn start(&self) -> u32 {
    self.index.start()
  }

  fn end(&self) -> u32 {
    self.index.end()
  }
}

pub struct Access {
  pub prop: Token,
}

impl Access {
  pub fn new(prop: Token) -> Self {
    Self { prop }
  }
}

impl Ranged for Access {
  fn start(&self) -> u32 {
    self.prop.start()
  }

  fn end(&self) -> u32 {
    self.prop.end()
  }
}

pub enum Primary {
  AssignBlock(Block),
  True(Token),
  False(Token),
  Nil(Token),
  Number(Token),
  Grouping(Box<Expr>),
  String(Token),
  Ident(Token),
  Self_(Token),
  Super(Super),
  Lambda(Box<Fun>),
  List(List),
  Map(Map),
}

impl Ranged for Primary {
  fn start(&self) -> u32 {
    match self {
      Primary::AssignBlock(block) => block.start(),
      Primary::True(true_) => true_.start(),
      Primary::False(false_) => false_.start(),
      Primary::Nil(nil_) => nil_.start(),
      Primary::Number(nil_) => nil_.start(),
      Primary::Grouping(grouping) => grouping.start(),
      Primary::String(string) => string.start(),
      Primary::Ident(ident) => ident.start(),
      Primary::Self_(self_) => self_.start(),
      Primary::Super(super_) => super_.start(),
      Primary::Lambda(lambda) => lambda.start(),
      Primary::List(list) => list.start(),
      Primary::Map(map) => map.start(),
    }
  }

  fn end(&self) -> u32 {
    match self {
      Primary::AssignBlock(block) => block.end(),
      Primary::True(true_) => true_.end(),
      Primary::False(false_) => false_.end(),
      Primary::Nil(nil_) => nil_.end(),
      Primary::Number(nil_) => nil_.end(),
      Primary::Grouping(grouping) => grouping.end(),
      Primary::String(string) => string.end(),
      Primary::Ident(ident) => ident.end(),
      Primary::Self_(self_) => self_.end(),
      Primary::Super(super_) => super_.end(),
      Primary::Lambda(lambda) => lambda.end(),
      Primary::List(list) => list.end(),
      Primary::Map(map) => map.end(),
    }
  }
}

pub struct List {
  pub range: Range,
  pub items: Vec<Expr>,
}

impl List {
  pub fn new(range: Range, items: Vec<Expr>) -> Self {
    Self { range, items }
  }
}

impl Ranged for List {
  fn range(&self) -> Range {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Map {
  pub range: Range,
  pub entries: Vec<(Expr, Expr)>,
}

impl Map {
  pub fn new(range: Range, kvp: Vec<(Expr, Expr)>) -> Self {
    Self {
      range,
      entries: kvp,
    }
  }
}

impl Ranged for Map {
  fn range(&self) -> Range {
    self.range
  }

  fn start(&self) -> u32 {
    self.range.start
  }

  fn end(&self) -> u32 {
    self.range.end
  }
}

pub struct Super {
  pub super_: Token,
  pub access: Token,
}

impl Super {
  pub fn new(super_: Token, access: Token) -> Self {
    Self { super_, access }
  }
}

impl Ranged for Super {
  fn start(&self) -> u32 {
    self.super_.start()
  }

  fn end(&self) -> u32 {
    self.access.end()
  }
}

pub struct TypeParam {
  pub name: Token,
  pub constraint: Option<Type>,
}

impl TypeParam {
  pub fn new(name: Token, constraint: Option<Type>) -> Self {
    Self { name, constraint }
  }
}

impl Ranged for TypeParam {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    self
      .constraint
      .as_ref()
      .map_or_else(|| self.name.end(), |constraint| constraint.range().end)
  }
}

pub enum Type {
  Union(Box<Union>),
  Intersection(Box<Intersection>),
  Fun(Box<CallSignature>),
  List(Box<ListType>),
  Ref(Box<TypeRef>),
  Primitive(Primitive),
}

impl Ranged for Type {
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

pub struct ClassType {
  pub type_ref: TypeRef,
}

impl ClassType {
  pub fn new(type_ref: TypeRef) -> Self {
    Self { type_ref }
  }
}

impl Ranged for ClassType {
  fn start(&self) -> u32 {
    self.type_ref.start()
  }

  fn end(&self) -> u32 {
    self.type_ref.end()
  }
}

pub struct TypeRef {
  pub name: Token,
  pub type_args: Vec<Type>,
}

impl TypeRef {
  pub fn new(name: Token, type_args: Vec<Type>) -> Self {
    Self { name, type_args }
  }
}

impl Ranged for TypeRef {
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

pub struct TypeMember {
  pub name: Token,
  pub type_: Type,
}

impl TypeMember {
  pub fn new(name: Token, type_: Type) -> Self {
    Self { name, type_ }
  }
}

impl Ranged for TypeMember {
  fn start(&self) -> u32 {
    self.name.start()
  }

  fn end(&self) -> u32 {
    self.type_.end()
  }
}

pub struct Union {
  pub lhs: Type,
  pub rhs: Type,
}

impl Union {
  pub fn new(lhs: Type, rhs: Type) -> Self {
    Self { lhs, rhs }
  }
}

impl Ranged for Union {
  fn start(&self) -> u32 {
    self.lhs.start()
  }

  fn end(&self) -> u32 {
    self.rhs.end()
  }
}

pub struct Intersection {
  pub lhs: Type,
  pub rhs: Type,
}

impl Intersection {
  pub fn new(lhs: Type, rhs: Type) -> Self {
    Self { lhs, rhs }
  }
}

impl Ranged for Intersection {
  fn start(&self) -> u32 {
    self.lhs.start()
  }

  fn end(&self) -> u32 {
    self.rhs.end()
  }
}

pub struct ListType {
  pub item_type: Type,
}

impl ListType {
  pub fn new(item_type: Type) -> Self {
    Self { item_type }
  }
}

impl Ranged for ListType {
  fn start(&self) -> u32 {
    self.item_type.start()
  }

  fn end(&self) -> u32 {
    self.item_type.end()
  }
}

pub enum Primitive {
  Nil(Token),
  Number(Token),
  Bool(Token),
  String(Token),
  Any(Token),
}

impl Ranged for Primitive {
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

impl Ranged for Token {
  fn start(&self) -> u32 {
    self.line
  }

  fn end(&self) -> u32 {
    self.line
  }
}
