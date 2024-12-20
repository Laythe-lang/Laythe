use super::ir::{
  ast::{self, Decl, Expr, Primary, Span, Spanned, Stmt, Symbol, Trailer},
  symbol_table::{self, AddSymbolResult, SymbolState, SymbolTable},
  token::{Lexeme, Token, TokenKind},
};
use crate::{
  source::{Source, VmFileId},
  FeResult,
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use laythe_core::{
  constants::{ITER_VAR, OBJECT, SELF, SUPER, UNINITIALIZED_VAR}, module::{self, ImportError}, object::{FunKind, LyStr}, value::Value, Allocator, Ref
};
use laythe_lib::global::ERROR_CLASS_NAME;
use std::vec;

/// A placeholder token to fill the first slot for non method functions
const SELF_TOKEN: &Token<'static> = &Token::new(TokenKind::Error, Lexeme::Slice(SELF), 0, 0);
const UNINITIALIZED_TOKEN: &Token<'static> =
  &Token::new(TokenKind::Error, Lexeme::Slice(UNINITIALIZED_VAR), 0, 0);

/// Tracks data about a class
#[derive(Debug, Clone, Default)]
struct ClassInfo {
  /// The current function kind for this class
  fun_kind: Option<FunKind>,
}

/// A wrapper struct providing depth along with the
/// underlying symbol table
struct TrackedSymbolTable<'a> {
  /// How many functions is this symbol table located
  fun_depth: i32,

  /// How many scopes is this symbol table located
  scope_depth: i32,

  /// The underlying symbol table
  table: SymbolTable<'a>,
}

pub struct Resolver<'a, 'src> {
  /// The global module
  global_module: Ref<module::Module>,

  /// The current module
  current_module: Ref<module::Module>,

  /// read only reference to the allocator
  gc: &'a Allocator,

  /// read only reference to the allocator
  source: &'src Source,

  /// The info on the current class
  classes: Vec<ClassInfo>,

  /// Are we resolving for a repl
  repl: bool,

  /// All the errors found during resolution
  errors: Vec<Diagnostic<VmFileId>>,

  /// The file id for the current file
  file_id: VmFileId,

  /// The current function depth
  fun_depth: i32,

  /// The current scope depth
  scope_depth: i32,

  /// stack of current symbol tables
  tables: Vec<TrackedSymbolTable<'src>>,
}

impl<'a, 'src> Resolver<'a, 'src> {
  /// Create a new resolver at the module scope level. This struct will
  /// modify the parsers ast to provide information on symbols in each scope
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::{
  ///   compiler::Resolver,
  ///   source::{Source, VM_FILE_TEST_ID},
  /// };
  /// use laythe_core::{
  ///   Allocator,
  ///   hooks::{NoContext, GcHooks},
  ///   module::Module,
  ///   object::Class,
  /// };
  /// use std::path::PathBuf;
  ///
  /// let mut context = NoContext::default();
  /// let hooks = GcHooks::new(&mut context);
  ///
  /// let name = hooks.manage_str("module");
  /// let class = hooks.manage_obj(Class::bare(name));
  /// let module_path = "module/path";
  /// let module = hooks.manage(Module::new(&hooks, class, module_path, 0));
  ///
  /// let file_id = VM_FILE_TEST_ID;
  /// let source = Source::new(hooks.manage_str("print('10');"));
  /// let gc = context.done();
  ///
  /// let compiler = Resolver::new(module, module, &gc, &source, file_id, false);
  /// ```
  pub fn new(
    global_module: Ref<module::Module>,
    current_module: Ref<module::Module>,
    gc: &'a Allocator,
    source: &'src Source,
    file_id: VmFileId,
    repl: bool,
  ) -> Self {
    let table = SymbolTable::new(source.vec());

    Self {
      file_id,
      global_module,
      current_module,
      gc,
      source,
      repl,
      errors: vec![],
      classes: vec![],
      fun_depth: 0,
      scope_depth: 0,
      tables: vec![TrackedSymbolTable {
        fun_depth: 0,
        scope_depth: 0,
        table,
      }],
    }
  }

  /// Modify the provided ast with information about resolved variables
  pub fn resolve(mut self, ast: &mut ast::Module<'src>) -> FeResult<()> {
    // preemptively declare all module scoped variables
    self.declare_module_scoped(ast);

    // resolve variables at module scope
    for decl in &mut ast.decls {
      self.decl(decl);
    }

    // attach module scoped variable to top level
    // ast node
    ast.symbols = self.end_scope();

    if self.errors.is_empty() {
      Ok(())
    } else {
      Err(self.errors)
    }
  }

  /// Declare all module scoped variables
  fn declare_module_scoped(&mut self, ast: &mut ast::Module<'src>) {
    // Declare existing symbols
    let mut symbols: Vec<(LyStr, Value, usize)> = self.current_module.symbols_by_name().collect();
    symbols.sort_by(|(_, _, id1), (_, _, id2)| id1.cmp(id2));

    for (name, _, _) in symbols {
      let token = Token::new(TokenKind::Identifier, Lexeme::Owned(name.to_string()), 0, 0);
      self.redeclare_existing_variable(&token);
    }

    self.declare_module_variable(UNINITIALIZED_TOKEN);
    self.define_variable(UNINITIALIZED_TOKEN);

    for decl in &mut ast.decls {
      self.decl_module(decl);
    }
  }

  /// scope the provide closure
  fn scope(&mut self, cb: impl FnOnce(&mut Self)) -> SymbolTable<'src> {
    self.begin_scope();
    cb(self);
    self.end_scope()
  }

  /// Increase the scope depth
  fn begin_scope(&mut self) {
    self.scope_depth += 1;
    self.tables.push(TrackedSymbolTable {
      table: SymbolTable::new(self.source.vec()),
      fun_depth: self.fun_depth,
      scope_depth: self.scope_depth,
    });
  }

  /// Decrease the scope depth
  fn end_scope(&mut self) -> SymbolTable<'src> {
    self.scope_depth -= 1;
    self.tables.pop().expect("Expected symbol table").table
  }

  /// Get a mutable reference to the current class info
  fn class_info(&self) -> Option<&ClassInfo> {
    self.classes.last()
  }

  /// Get a mutable reference to the current class info
  fn class_info_mut(&mut self) -> Option<&mut ClassInfo> {
    self.classes.last_mut()
  }

  /// Get a mutable reference to the current tacked symbol table
  fn table_mut(&mut self) -> &mut TrackedSymbolTable<'src> {
    self.tables.last_mut().expect("Expected symbol table")
  }

  /// Declare a variable
  fn declare_variable(&mut self, name: &Token<'src>) {
    // module scoped variables should already have been declared
    if self.tables.len() == 1 {
      return;
    }

    self.add_symbol_to_table(name.str(), name.span());
  }

  fn redeclare_existing_variable(&mut self, name: &Token<'src>) {
    debug_assert!(self.tables.len() == 1);

    self.add_symbol_to_table(name.str(), name.span());

    let table = self.table_mut();
    let symbol = table.table.get_mut(name.str()).expect("Expected symbol");
    symbol.already_initialize()
  }

  /// Declare a module scoped variable
  fn declare_module_variable(&mut self, name: &Token<'src>) {
    debug_assert!(self.tables.len() == 1);

    self.add_symbol_to_table(name.str(), name.span());
  }

  /// Add a symbol to the current table
  fn add_symbol_to_table(&mut self, name: &str, span: Span) {
    let table = self.table_mut();

    if let AddSymbolResult::DuplicateSymbol(local) = table.table.add_symbol(name, span) {
      self.duplicate_declaration(&local, name, span)
    }
  }

  fn duplicate_declaration(&mut self, existing: &symbol_table::Symbol, name: &str, span: Span) {
    const MESSAGE: &str = "Variable with this name already declared in this scope.";
    const PRIMARY_LABEL: &str = "Declared a second time here";

    match existing.span() {
      Some(span) => self.error_with_context(
        MESSAGE,
        vec![
          Label::primary(self.file_id, span).with_message(PRIMARY_LABEL),
          Label::secondary(self.file_id, span)
            .with_message(format!("{} was originally declared here", name)),
        ],
      ),
      None => {
        if self.repl {
          let error = Diagnostic::error()
            .with_message(MESSAGE)
            .with_labels(vec![Label::primary(self.file_id, span)])
            .with_notes(vec![
              "This symbol was declared previously in the repl session".to_string(),
            ]);

          self.add_error(error);
        } else {
          panic!("Unexpected missing span")
        }
      },
    }
  }

  /// resolve a named variable
  fn resolve_variable(&mut self, name: &Token<'src>) {
    for table in self.tables.iter_mut().rev() {
      if let Some(symbol) = table.table.get_mut(name.str()) {
        match symbol.state() {
          SymbolState::Uninitialized => {
            // If we are module scoped we'll defer to a runtime error
            // instead
            if table.scope_depth > 0 {
              self.error(
                "Cannot read local variable in its own initializer.",
                Some(name.span()),
              );
            }
          },
          SymbolState::LocalInitialized => {
            if table.fun_depth < self.fun_depth {
              symbol.capture();
            }
          },
          _ => (),
        }

        return;
      }
    }

    // Check if symbol if found in the global scope if it is
    // Add it as a global symbol. If not throw an error
    if self
      .gc
      .has_str(name.str())
      .ok_or(ImportError::SymbolDoesNotExist)
      .and_then(|interned_name| {
        self
          .global_module
          .get_exported_symbol_by_name(interned_name)
          .ok_or(ImportError::SymbolDoesNotExist)
      })
      .is_ok()
    {
      let table = &mut self.tables.first_mut().unwrap();
      table.table.add_symbol_from_global(name.str(), name.span());
    } else {
      self.error(
        &format!("Attempted to access undeclared variable {}", name.str()),
        Some(name.span()),
      )
    }
  }

  /// Define a variable
  fn define_variable(&mut self, name: &Token<'src>) {
    let scope_depth = self.tables.len();

    let table = self.table_mut();
    let symbol = table.table.get_mut(name.str()).expect("Expected symbol");

    if scope_depth == 1 {
      symbol.module_initialize()
    } else {
      symbol.initialize()
    }
  }

  /// Indicate an error with additional context
  fn error_with_context(&mut self, message_primary: &str, labels: Vec<Label<VmFileId>>) {
    let error = Diagnostic::error()
      .with_message(message_primary)
      .with_labels(labels);

    self.add_error(error);
  }

  /// Indicate an error occurred at he current index
  fn error_at_current(&mut self, message: &str, span: Option<Span>) {
    self.error_at(message, span);
  }

  /// Indicate an error occurred at the previous index
  fn error(&mut self, message: &str, span: Option<Span>) {
    self.error_at(message, span);
  }

  /// Print an error to the console for a user to address
  fn error_at(&mut self, message: &str, span: Option<Span>) {
    let error = Diagnostic::error().with_message(message);

    let error = match span {
      Some(span) => error.with_labels(vec![Label::primary(self.file_id, span)]),
      None => error,
    };

    self.add_error(error);
  }

  fn add_error(&mut self, error: Diagnostic<VmFileId>) {
    self.errors.push(error);
  }

  /// Resolve a declaration
  fn decl(&mut self, decl: &mut Decl<'src>) {
    match decl {
      Decl::Symbol(symbol) => self.symbol(symbol),
      Decl::Export(export) => self.export(export),
      Decl::Stmt(stmt) => self.stmt(stmt),
      Decl::Error(_) => (),
    }
  }

  /// Resolve a module scoped declaration
  fn decl_module(&mut self, decl: &Decl<'src>) {
    debug_assert!(self.tables.len() == 1);

    match decl {
      Decl::Symbol(symbol) => self.symbol_module(symbol),
      Decl::Export(export) => self.export_module(export),
      Decl::Stmt(stmt) => {
        if let Stmt::Import(import) = &**stmt {
          self.import_module(import);
        }
      },
      Decl::Error(_) => (),
    }
  }

  /// Resolve a statement
  fn stmt(&mut self, stmt: &mut Stmt<'src>) {
    match stmt {
      Stmt::Expr(expr) => {
        self.expr(expr);
      },
      Stmt::ImplicitReturn(expr) => {
        self.expr(expr);
      },
      Stmt::Import(import) => self.import(import),
      Stmt::For(for_) => self.for_(for_),
      Stmt::If(if_) => self.if_(if_),
      Stmt::Return(return_) => self.return_(return_),
      Stmt::Launch(launch) => self.launch(launch),
      Stmt::Raise(raise) => self.raise(raise),
      Stmt::While(while_) => self.while_(while_),
      Stmt::Try(try_) => self.try_(try_),
      Stmt::Continue(_) => (),
      Stmt::Break(_) => (),
    }
  }

  /// Resolve an expression
  fn expr(&mut self, expr: &mut Expr<'src>) {
    match expr {
      Expr::Assign(assign) => self.assign(assign),
      Expr::Send(drain) => self.send(drain),
      Expr::AssignBinary(assign_binary) => self.assign_binary(assign_binary),
      Expr::Ternary(ternary) => self.ternary(ternary),
      Expr::Binary(binary) => self.binary(binary),
      Expr::Unary(unary) => self.unary(unary),
      Expr::Atom(atom) => self.atom(atom),
    }
  }

  /// Resolve a symbol declaration
  fn symbol(&mut self, symbol: &mut Symbol<'src>) {
    match symbol {
      Symbol::Class(class) => self.class(class),
      Symbol::Fun(fun) => self.fun(fun),
      Symbol::Let(let_) => self.let_(let_),
      Symbol::Trait(_) => (),
      Symbol::TypeDecl(_) => (),
    };
  }

  /// Resolve a module scoped symbol declaration
  fn symbol_module(&mut self, symbol: &Symbol<'src>) {
    debug_assert!(self.tables.len() == 1);

    match symbol {
      Symbol::Class(class) => self.class_module(class),
      Symbol::Fun(fun) => self.fun_module(fun),
      Symbol::Let(let_) => self.let_module(let_),
      Symbol::Trait(_) => (),
      Symbol::TypeDecl(_) => (),
    };
  }

  /// Resolve an export declaration
  fn export(&mut self, export: &mut Symbol<'src>) {
    if self.tables.len() > 1 {
      self.error_at_current(
        "Can only export from the module scope.",
        Some(export.span()),
      )
    }

    if self.repl {
      self.error_at_current("Can't export from the repl.", Some(export.span()))
    }

    self.symbol(export);
  }

  /// Resolve a module scoped export declaration
  fn export_module(&mut self, export: &Symbol<'src>) {
    debug_assert!(self.tables.len() == 1);

    self.symbol_module(export);
  }

  /// Resolve a class declaration
  fn class(&mut self, class: &mut ast::Class<'src>) {
    // declare the class by name
    let name = &class.name;
    self.declare_variable(name);
    self.define_variable(name);

    // Add this class information to stack
    self.classes.push(ClassInfo::default());

    // handle the case where a super class exists
    let span = if let Some(super_class) = &class.super_class {
      self.resolve_variable(&super_class.type_ref.name);
      super_class.type_ref.name.span()
    } else {
      self.resolve_variable(&Token::new(
        TokenKind::Identifier,
        Lexeme::Slice(OBJECT),
        class.name.start(),
        class.name.end(),
      ));
      class.name.span()
    };

    let super_token = Token::new(TokenKind::Super, Lexeme::Slice(SUPER), span.start, span.end);

    // start a new scope with the super keyword present
    self.begin_scope();

    self.declare_variable(&super_token);
    self.define_variable(&super_token);

    self.resolve_variable(name);

    // process the initializer
    if let Some(init) = &mut class.init {
      self.method(init, FunKind::Initializer);
    }

    // process methods
    for method in &mut class.methods {
      self.method(method, FunKind::Method);
    }

    // process static methods
    for static_method in &mut class.static_methods {
      self.static_method(static_method);
    }

    // if we have a super drop the extra scope with super
    class.symbols = self.end_scope();

    // restore the enclosing class compiler
    self.classes.pop();
  }

  /// Resolve a class declaration
  fn class_module(&mut self, class: &ast::Class<'src>) {
    // declare the class by name
    let name = &class.name;
    self.declare_module_variable(name);
  }

  /// Resolve a method
  fn method(&mut self, method: &mut ast::Fun<'src>, fun_kind: FunKind) {
    self.class_info_mut().expect("Expected class info").fun_kind = Some(fun_kind);

    self.function(method, fun_kind);
  }

  /// Resolve a static method
  fn static_method(&mut self, static_method: &mut ast::Fun<'src>) {
    self.class_info_mut().expect("Expected class info").fun_kind = Some(FunKind::StaticMethod);

    self.function(static_method, FunKind::StaticMethod);
  }

  /// Resolve a plain function
  fn fun(&mut self, fun: &mut ast::Fun<'src>) {
    let name = fun.name.as_ref().expect("Expected function name");
    self.declare_variable(name);
    self.define_variable(name);

    self.function(fun, FunKind::Fun);
  }

  /// Resolve a plain function
  fn fun_module(&mut self, fun: &ast::Fun<'src>) {
    let name = fun.name.as_ref().expect("Expected function name");
    self.declare_module_variable(name);
  }

  /// Resolve a let binding
  fn let_(&mut self, let_: &mut ast::Let<'src>) {
    self.declare_variable(&let_.name);

    if let Some(v) = &mut let_.value {
      self.expr(v)
    }

    self.define_variable(&let_.name);
  }

  /// Resolve a let binding
  fn let_module(&mut self, let_: &ast::Let<'src>) {
    self.declare_module_variable(&let_.name);
  }

  /// Resolve a function objects that presents, functions, methods
  /// and lambdas
  fn function(&mut self, fun: &mut ast::Fun<'src>, fun_kind: FunKind) {
    self.fun_depth += 1;
    self.begin_scope();

    match fun_kind {
      FunKind::Method | FunKind::Initializer => {
        self.declare_variable(SELF_TOKEN);
        self.define_variable(SELF_TOKEN)
      },
      FunKind::Fun | FunKind::StaticMethod => {
        self.declare_variable(UNINITIALIZED_TOKEN);
        self.define_variable(UNINITIALIZED_TOKEN)
      },
      FunKind::Script => (),
    }
    self.call_sig(&fun.call_sig);

    match &mut fun.body {
      ast::FunBody::Block(block) => self.block(block),
      ast::FunBody::Expr(expr) => {
        self.expr(expr);
      },
    };

    fun.symbols = self.end_scope();
    self.fun_depth -= 1;
  }

  /// Resolve an import statement
  fn import(&mut self, import: &ast::Import<'src>) {
    match &import.stem {
      ast::ImportStem::None => {
        let name = import.path().last().expect("Expected path to be filled");
        self.declare_variable(name);
        self.define_variable(name)
      },
      ast::ImportStem::Rename(rename) => {
        self.declare_variable(rename);
        self.define_variable(rename);
      },
      ast::ImportStem::Symbols(symbols) => {
        for symbol in symbols {
          let name = symbol.rename.as_ref().unwrap_or(&symbol.symbol);

          self.declare_variable(name);
          self.define_variable(name);
        }
      },
    }
  }

  /// Resolve an import statement
  fn import_module(&mut self, import: &ast::Import<'src>) {
    match &import.stem {
      ast::ImportStem::None => {
        let name = import.path().last().expect("Expected path to be filled");
        self.declare_module_variable(name);
      },
      ast::ImportStem::Rename(rename) => {
        self.declare_module_variable(rename);
      },
      ast::ImportStem::Symbols(symbols) => {
        for symbol in symbols {
          let name = symbol.rename.as_ref().unwrap_or(&symbol.symbol);

          self.declare_module_variable(name);
        }
      },
    }
  }

  /// Resolve a for loop
  fn for_(&mut self, for_: &mut ast::For<'src>) {
    // new scope for full loop including loop variables
    for_.symbols = self.scope(|self_| {
      // token for hidden $iter variable
      let iterator_token = Token::new(
        TokenKind::Identifier,
        Lexeme::Slice(ITER_VAR),
        for_.iter.start(),
        for_.iter.end(),
      );

      // declare the hidden local $iter variable
      self_.declare_variable(&iterator_token);
      self_.define_variable(&iterator_token);

      self_.declare_variable(&for_.item);
      self_.define_variable(&for_.item);
      self_.expr(&mut for_.iter);

      // loop body
      for_.body.symbols = self_.scope(|self_| self_.block(&mut for_.body));
    });
  }

  /// Resolve a while loop
  fn while_(&mut self, while_: &mut ast::While<'src>) {
    self.expr(&mut while_.cond);
    while_.body.symbols = self.scope(|self_| self_.block(&mut while_.body));
  }

  /// Resolve a if statement
  fn if_(&mut self, if_: &mut ast::If<'src>) {
    self.expr(&mut if_.cond);
    if_.body.symbols = self.scope(|self_| self_.block(&mut if_.body));

    if let Some(else_) = &mut if_.else_ {
      match else_ {
        ast::Else::If(if_) => self.if_(if_),
        ast::Else::Block(block) => {
          block.symbols = self.scope(|self_| {
            self_.block(block);
          });
        },
      }
    }
  }

  /// Resolve a launch statement
  fn launch(&mut self, launch: &mut ast::Launch<'src>) {
    self.expr(&mut launch.closure);
  }

  /// Resolve a return statement
  fn return_(&mut self, return_: &mut ast::Return<'src>) {
    if let Some(v) = &mut return_.value {
      self.expr(v);
    }
  }

  /// Resolve a try catch block
  fn try_(&mut self, try_: &mut ast::Try<'src>) {
    try_.block.symbols = self.scope(|self_| self_.block(&mut try_.block));
    for catch in &mut try_.catches {
      catch.symbols = self.scope(|self_| self_.catch(catch))
    }
  }

  fn catch(&mut self, catch: &mut ast::Catch<'src>) {
    self.declare_variable(&catch.name);
    self.define_variable(&catch.name);

    if let Some(class) = &catch.class {
      self.resolve_variable(class)
    } else {
      self.resolve_variable(&Token::new(
        TokenKind::Identifier,
        Lexeme::Slice(ERROR_CLASS_NAME),
        catch.name.end(),
        catch.name.end(),
      ));
    }

    catch.block.symbols = self.scope(|self_| self_.block(&mut catch.block));
  }

  /// Resolve a raise statement
  fn raise(&mut self, raise: &mut ast::Raise<'src>) {
    self.expr(&mut raise.error);
  }

  /// Resolve a block
  fn block(&mut self, block: &mut ast::Block<'src>) {
    for decl in &mut block.decls {
      self.decl(decl);
    }
  }

  /// Resolve an assignment expression
  fn assign(&mut self, assign: &mut ast::Assign<'src>) {
    self.atom(&mut assign.lhs);
    self.expr(&mut assign.rhs);
  }

  fn send(&mut self, send: &mut ast::Send<'src>) {
    self.atom(&mut send.lhs);
    self.expr(&mut send.rhs);
  }

  /// Resolve a binary assignment expression
  fn assign_binary(&mut self, assign_binary: &mut ast::AssignBinary<'src>) {
    self.atom(&mut assign_binary.lhs);
    self.expr(&mut assign_binary.rhs);
  }

  /// Resolve a binary expression
  fn ternary(&mut self, ternary: &mut ast::Ternary<'src>) {
    self.expr(&mut ternary.cond);
    self.expr(&mut ternary.then);
    self.expr(&mut ternary.else_);
  }

  /// Resolve a binary expression
  fn binary(&mut self, binary: &mut ast::Binary<'src>) {
    self.expr(&mut binary.lhs);
    self.expr(&mut binary.rhs);
  }

  /// Resolve a unary expression
  fn unary(&mut self, unary: &mut ast::Unary<'src>) {
    self.expr(&mut unary.expr);
  }

  /// Resolve a call expression
  fn call(&mut self, call: &mut ast::Call<'src>) {
    for expr in &mut call.args {
      self.expr(expr);
    }
  }

  /// Resolve an indexing expression
  fn index(&mut self, index: &mut ast::Index<'src>) {
    self.expr(&mut index.index);
  }

  /// Resolve an atom expression
  fn atom(&mut self, atom: &mut ast::Atom<'src>) {
    match &mut atom.primary {
      Primary::Channel(token) => self.channel(token),
      Primary::Grouping(expr) => self.expr(expr),
      Primary::Interpolation(interpolation) => self.interpolation(interpolation),
      Primary::Ident(token) => self.identifier(token),
      Primary::InstanceAccess(instance_access) => self.instance_access(instance_access),
      Primary::Self_(token) => self.self_(token),
      Primary::Super(token) => self.super_(token),
      Primary::Lambda(fun) => self.lambda(fun),
      Primary::List(list) => self.collection(list),
      Primary::Tuple(tuple) => self.collection(tuple),
      Primary::Map(map) => self.map(map),
      Primary::False(_) => (),
      Primary::Nil(_) => (),
      Primary::Number(_) => (),
      Primary::String(_) => (),
      Primary::True(_) => (),
    }

    if let Some((first, rest)) = atom.trailers.split_first_mut() {
      match first {
        Trailer::Call(call) => self.call(call),
        Trailer::Index(index) => self.index(index),
        Trailer::Access(_) => {},
      }

      for trailer in rest.iter_mut() {
        match trailer {
          Trailer::Call(call) => self.call(call),
          Trailer::Index(index) => self.index(index),
          Trailer::Access(_) => (),
        };
      }
    }
  }

  /// Resolve a channel declaration
  fn channel(&mut self, channel: &mut ast::Channel<'src>) {
    if let Some(expr) = &mut channel.expr {
      self.expr(expr);
    }
  }

  /// Resolve a string token
  fn interpolation(&mut self, interpolation: &mut ast::Interpolation<'src>) {
    for segment in interpolation.segments.iter_mut() {
      if let ast::StringSegments::Expr(expr) = segment {
        self.expr(expr);
      }
    }
  }

  /// Resolve a identifer token
  fn identifier(&mut self, token: &Token<'src>) {
    self.resolve_variable(token);
  }

  /// Compile instance access self load
  fn instance_access(&mut self, instance_access: &ast::InstanceAccess<'src>) {
    self
      .class_info()
      .map(|class_compiler| class_compiler.fun_kind)
      .and_then(|fun_kind| {
        fun_kind.and_then(|fun_kind| match fun_kind {
          FunKind::Method | FunKind::Initializer => {
            self.resolve_variable(&Token::new(
              TokenKind::Self_,
              Lexeme::Slice(SELF),
              instance_access.start(),
              instance_access.start() + 1,
            ));
            Some(())
          },
          _ => None,
        })
      })
      .or_else(|| {
        self.error(
          "Cannot access property off 'self' with '@' outside of class instance methods.",
          Some(instance_access.span()),
        );
        None
      });
  }

  /// Compile the self token
  fn self_(&mut self, self_: &Token<'src>) {
    self
      .class_info()
      .map(|class_compiler| class_compiler.fun_kind)
      .and_then(|fun_kind| {
        fun_kind.and_then(|fun_kind| match fun_kind {
          FunKind::Method | FunKind::Initializer => {
            self.resolve_variable(self_);
            Some(())
          },
          _ => None,
        })
      })
      .or_else(|| {
        self.error(
          "Cannot use 'self' outside of class instance methods.",
          Some(self_.span()),
        );
        None
      });
  }

  /// Compile the super token
  fn super_(&mut self, super_: &ast::Super<'src>) {
    if self.class_info().is_none() {
      self.error(
        "Cannot use 'super' outside of a class.",
        Some(super_.super_.span()),
      );
    }

    // load self on top of stack
    self.resolve_variable(&Token::new(
      TokenKind::Self_,
      Lexeme::Slice(SELF),
      super_.end(),
      super_.end(),
    ));
    self.resolve_variable(&super_.super_);
  }

  /// Resolve a lambda expression
  fn lambda(&mut self, fun: &mut ast::Fun<'src>) {
    self.function(fun, FunKind::Fun);
  }

  /// Resolve a list literal
  fn collection(&mut self, list: &mut ast::Collection<'src>) {
    for item in list.items.iter_mut() {
      self.expr(item);
    }
  }

  /// Resolve a map literal
  fn map(&mut self, map: &mut ast::Map<'src>) {
    for (key, value) in map.entries.iter_mut() {
      self.expr(key);
      self.expr(value);
    }
  }

  /// Set the functions arity from the call signature
  fn call_sig(&mut self, call_sig: &ast::CallSignature<'src>) {
    for param in &call_sig.params {
      self.declare_variable(&param.name);
      self.define_variable(&param.name);
    }
  }
}

#[cfg(test)]
mod test {
  use laythe_core::{
    hooks::{GcHooks, NoContext},
    module::Module,
    object::Class,
    utils::IdEmitter, ObjRef,
  };
  use laythe_lib::create_std_lib;

  use crate::{
    compiler::Parser,
    source::{Source, VM_FILE_TEST_ID},
  };

  use super::*;

  pub fn test_class(hooks: &GcHooks, name: &str) -> ObjRef<Class> {
    let mut object_class = hooks.manage_obj(Class::bare(hooks.manage_str("Object")));
    let mut class_class = hooks.manage_obj(Class::bare(hooks.manage_str("Class")));
    class_class.inherit(hooks, object_class);

    let class_copy = class_class;
    class_class.set_meta(class_copy);

    // create object's meta class
    let mut object_meta_class = hooks.manage_obj(Class::bare(
      hooks.manage_str(format!("{} metaClass", &*object_class.name())),
    ));

    object_meta_class.inherit(hooks, class_class);
    object_meta_class.set_meta(class_class);

    object_class.set_meta(object_meta_class);

    Class::with_inheritance(hooks, hooks.manage_str(name), object_class)
  }

  fn dummy_module(hooks: &GcHooks) -> Ref<Module> {
    let module_class = test_class(hooks, "Module");
    hooks.push_root(module_class);

    let module = hooks.manage(Module::new(hooks, module_class, "example", 0));
    hooks.push_root(module);

    module
  }

  fn std_module(hooks: &GcHooks) -> Ref<Module> {
    let mut emitter = IdEmitter::default();
    let std_lib = create_std_lib(hooks, &mut emitter).expect("Standard library creation failed");
    std_lib.root_module()
  }

  fn test_repl_resolve(src: &str, test_assert: impl FnOnce(&ast::Module, FeResult<()>)) {
    test_resolve(src, true, false, test_assert)
  }

  fn test_file_std_resolve(src: &str, test_assert: impl FnOnce(&ast::Module, FeResult<()>)) {
    test_resolve(src, false, true, test_assert)
  }

  fn test_file_resolve(src: &str, test_assert: impl FnOnce(&ast::Module, FeResult<()>)) {
    test_resolve(src, false, false, test_assert)
  }

  fn test_resolve(
    src: &str,
    repl: bool,
    with_std: bool,
    test_assert: impl FnOnce(&ast::Module, FeResult<()>),
  ) {
    let context = NoContext::default();
    let hooks = &GcHooks::new(&context);

    let src = hooks.manage_str(src);
    hooks.push_root(src);
    let source = Source::new(hooks.manage_str(src));
    let (ast, _) = Parser::new(&source, VM_FILE_TEST_ID).parse();
    assert!(ast.is_ok(), "{}", src);
    let mut ast = ast.unwrap();

    let global_module = if with_std {
      std_module(hooks)
    } else {
      dummy_module(hooks)
    };

    let current_module = dummy_module(hooks);

    let gc = context.done();
    let resolver = Resolver::new(
      global_module,
      current_module,
      &gc,
      &source,
      VM_FILE_TEST_ID,
      repl,
    );

    let result = resolver.resolve(&mut ast);
    test_assert(&ast, result);
  }

  #[test]
  fn import() {
    let example = r#"
      import std.time;
    "#;

    test_file_resolve(example, |ast, result| {
      assert!(result.is_ok());

      let symbol = ast.symbols.get("time").unwrap();
      assert_eq!(symbol.state(), SymbolState::ModuleInitialized);
    });
  }

  #[test]
  fn import_rename() {
    let example = r#"
      import std.time as thing;
    "#;

    test_file_resolve(example, |ast, result| {
      assert!(result.is_ok());

      let symbol = ast.symbols.get("thing").unwrap();
      assert_eq!(symbol.state(), SymbolState::ModuleInitialized);
    });
  }

  #[test]
  fn import_symbols() {
    let example = r#"
      import std.time:{foo, bar};
    "#;

    test_file_resolve(example, |ast, result| {
      assert!(result.is_ok());

      let symbol1 = ast.symbols.get("foo").unwrap();
      let symbol2 = ast.symbols.get("bar").unwrap();

      assert_eq!(symbol1.state(), SymbolState::ModuleInitialized);
      assert_eq!(symbol2.state(), SymbolState::ModuleInitialized);
    });
  }

  #[test]
  fn import_symbols_rename() {
    let example = r#"
      import std.time:{foo as baz, bar as example};
    "#;

    test_file_resolve(example, |ast, result| {
      assert!(result.is_ok());

      let symbol1 = ast.symbols.get("baz").unwrap();
      let symbol2 = ast.symbols.get("example").unwrap();

      assert_eq!(symbol1.state(), SymbolState::ModuleInitialized);
      assert_eq!(symbol2.state(), SymbolState::ModuleInitialized);
    });
  }

  #[test]
  fn export_variable() {
    let example = "
      export let x = 10;
    ";

    test_file_resolve(example, |ast, result| {
      assert!(result.is_ok());

      let symbol = ast.symbols.get("x").unwrap();
      assert_eq!(symbol.state(), SymbolState::ModuleInitialized);
    });
  }

  #[test]
  fn export_fun() {
    let example = "
      export fn example() {}
    ";

    test_file_resolve(example, |ast, result| {
      assert!(result.is_ok());

      let symbol = ast.symbols.get("example").unwrap();
      assert_eq!(symbol.state(), SymbolState::ModuleInitialized);
    });
  }

  #[test]
  fn export_class() {
    let example = "
      export class example {}
    ";

    test_file_std_resolve(example, |ast, result| {
      assert!(result.is_ok());

      let symbol = ast.symbols.get("example").unwrap();
      assert_eq!(symbol.state(), SymbolState::ModuleInitialized);
    });
  }

  #[test]
  fn export_not_module_scoped() {
    let example = "
      if true {
        export let x = 10;
      }
    ";

    test_file_resolve(example, |_ast, result| {
      assert!(result.is_err());

      let errors = result.unwrap_err();
      assert_eq!(errors.len(), 1);
      assert_eq!(errors[0].message, "Can only export from the module scope.");
    });
  }

  #[test]
  fn export_repl() {
    let example = "
      export let x = 10;
    ";

    test_repl_resolve(example, |_ast, result| {
      assert!(result.is_err());

      let errors = result.unwrap_err();
      assert_eq!(errors.len(), 1);
      assert_eq!(errors[0].message, "Can't export from the repl.");
    });
  }

  #[test]
  fn capture() {
    let example = "
      let x = 10;

      fn capture() { x }

      if true {
        let x = 10;

        fn capture() { x }
      }
    ";

    test_repl_resolve(example, |ast, result| {
      assert!(result.is_ok());

      let x = ast.symbols.get("x").unwrap();
      let capture = ast.symbols.get("capture").unwrap();

      assert_eq!(x.state(), SymbolState::ModuleInitialized);
      assert_eq!(capture.state(), SymbolState::ModuleInitialized);

      match &ast.decls[2] {
        Decl::Stmt(stmt) => match &**stmt {
          Stmt::If(if_) => {
            let x = if_.body.symbols.get("x").unwrap();
            let capture = if_.body.symbols.get("capture").unwrap();

            assert_eq!(x.state(), SymbolState::LocalCaptured);
            assert_eq!(capture.state(), SymbolState::LocalInitialized);
          },
          _ => panic!(),
        },
        _ => panic!(),
      }
    });
  }

  #[test]
  fn self_capture_explicit() {
    let example = "
      class A {
        foo() {
          || self.a
        }
      }
    ";

    test_file_std_resolve(example, |ast, result| {
      assert!(result.is_ok());

      match &ast.decls[0] {
        Decl::Symbol(stmt) => match &**stmt {
          Symbol::Class(class) => {
            let foo = &class.methods[0];

            let self_ = foo.symbols.get(SELF).unwrap();

            assert_eq!(self_.state(), SymbolState::LocalCaptured);
          },
          _ => panic!(),
        },
        _ => panic!(),
      }
    });
  }

  #[test]
  fn self_capture_implicit() {
    let example = "
      class A {
        foo() {
          || @a
        }
      }
    ";

    test_file_std_resolve(example, |ast, result| {
      assert!(result.is_ok());

      match &ast.decls[0] {
        Decl::Symbol(stmt) => match &**stmt {
          Symbol::Class(class) => {
            let foo = &class.methods[0];

            let self_ = foo.symbols.get(SELF).unwrap();

            assert_eq!(self_.state(), SymbolState::LocalCaptured);
          },
          _ => panic!(),
        },
        _ => panic!(),
      }
    });
  }

  #[test]
  fn global_decl_symbols() {
    let example = "
      class example1 {}
      fn example2() {}
      let example3 = 10;
    ";

    test_file_std_resolve(example, |ast, result| {
      assert!(result.is_ok());

      let example1 = ast.symbols.get("example1").unwrap();
      let example2 = ast.symbols.get("example2").unwrap();
      let example3 = ast.symbols.get("example3").unwrap();

      assert_eq!(example1.state(), SymbolState::ModuleInitialized);
      assert_eq!(example2.state(), SymbolState::ModuleInitialized);
      assert_eq!(example3.state(), SymbolState::ModuleInitialized);
    });
  }

  #[test]
  fn try_catch_single() {
    let example = "
      try {
      } catch e: Error {

      }
    ";

    test_file_std_resolve(example, |ast, result| {
      assert!(result.is_ok());
      assert_eq!(ast.decls.len(), 1);

      match &ast.decls[0] {
        Decl::Stmt(stmt) => match &**stmt {
          Stmt::Try(try_) => {
            let captured = try_.catches.first().unwrap().symbols.get("e").unwrap();

            assert_eq!(captured.state(), SymbolState::LocalInitialized);
          },
          _ => panic!(),
        },
        _ => panic!(),
      }
    });
  }

  #[test]
  fn try_catch_capture_error() {
    let example = "
      fn foo() {
        try {
        } catch e: Error {
          fn bar () { e }
        }
      }
  ";

    test_file_std_resolve(example, |ast, result| {
      assert!(result.is_ok());
      assert_eq!(ast.decls.len(), 1);

      let symbol = ast.symbols.get("foo").unwrap();
      assert_eq!(symbol.state(), SymbolState::ModuleInitialized);

      match &ast.decls[0] {
        Decl::Symbol(symbol) => match &**symbol {
          Symbol::Fun(fun) => match &fun.body {
            ast::FunBody::Block(block) => match &block.decls[0] {
              Decl::Stmt(stmt) => match &**stmt {
                Stmt::Try(try_) => {
                  let captured = try_.catches.first().unwrap().symbols.get("e").unwrap();

                  assert_eq!(captured.state(), SymbolState::LocalCaptured);
                },
                _ => panic!(),
              },
              _ => panic!(),
            },
            _ => panic!(),
          },
          _ => panic!(),
        },
        _ => panic!(),
      }
    });
  }

  #[test]
  fn local_decl_symbols() {
    let example = "
      if true {
        class example1 {}
        fn example2() {}
        let example3 = 10;
      }
    ";

    test_file_std_resolve(example, |ast, result| {
      assert!(result.is_ok());
      assert_eq!(ast.decls.len(), 1);
      match &ast.decls[0] {
        Decl::Stmt(stmt) => match &**stmt {
          Stmt::If(if_) => {
            let example1 = if_.body.symbols.get("example1").unwrap();
            let example2 = if_.body.symbols.get("example2").unwrap();
            let example3 = if_.body.symbols.get("example3").unwrap();

            assert_eq!(example1.state(), SymbolState::LocalInitialized);
            assert_eq!(example2.state(), SymbolState::LocalInitialized);
            assert_eq!(example3.state(), SymbolState::LocalInitialized);
          },
          _ => panic!(),
        },
        _ => panic!(),
      }
    });
  }

  #[test]
  fn multiple_global_resolves() {
    let example = "
      print();
      print();
    ";

    test_file_std_resolve(example, |ast, result| {
      assert!(result.is_ok());
      assert!(ast.symbols.get("print").is_some());
      assert_eq!(
        ast.symbols.get("print").unwrap().state(),
        SymbolState::GlobalInitialized
      );
    });
  }
}
