use super::{
  ir::{
    ast::*,
    symbol_table::SymbolTable,
    token::{Lexeme, Token, TokenKind},
  },
  scanner::Scanner,
};
use crate::{
  source::{LineOffsets, Source, VmFileId},
  FeResult,
};
use bumpalo::boxed::Box;
use bumpalo::collections::vec::Vec;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use laythe_core::{constants::INIT, object::FunKind};
use std::mem;

type ParseResult<T> = Result<T, Diagnostic<VmFileId>>;

fn to_fe_result<T>(result: ParseResult<T>) -> FeResult<T> {
  match result {
    Ok(res) => Ok(res),
    Err(err) => Err(vec![err]),
  }
}

#[derive(Debug)]
enum BlockReturn {
  Can,
  Cannot,
}

const NUMBER_TYPE: &str = "number";
const BOOL_TYPE: &str = "bool";
const STRING_TYPE: &str = "string";
const ANY_TYPE: &str = "any";

/// The Laythe parser. This structure produces the Laythe
/// AST
pub struct Parser<'a> {
  /// The current token
  current: Token<'a>,

  /// The previous token
  previous: Token<'a>,

  /// The current name let statement
  let_name: Option<Token<'a>>,

  /// The source
  source: &'a Source,

  /// All errors that have been during parsing
  errors: std::vec::Vec<Diagnostic<VmFileId>>,

  /// Can we currently implicitly return
  block_return: BlockReturn,

  /// Current function kind
  fun_kind: FunKind,

  /// Current loop depth
  loop_depth: u16,

  /// Current scope depth
  scope_depth: u16,

  /// Struct for providing a stream of tokens
  scanner: Scanner<'a>,

  /// file id
  file_id: VmFileId,
}

impl<'a> Parser<'a> {
  /// Create a new instance of the parser from a source str
  pub fn new(source: &'a Source, file_id: VmFileId) -> Self {
    Self {
      scanner: Scanner::new(source),
      file_id,
      source,
      errors: std::vec::Vec::new(),
      fun_kind: FunKind::Script,
      block_return: BlockReturn::Cannot,
      scope_depth: 0,
      loop_depth: 0,
      let_name: None,
      previous: Token::new(TokenKind::Error, Lexeme::Slice("error"), 0, 0),
      current: Token::new(TokenKind::Error, Lexeme::Slice("error"), 0, 0),
    }
  }

  /// Parse the provide source string into a Laythe AST
  /// Return the top level module struct if successful
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::{
  ///   compiler::Parser,
  ///   source::{Source, VM_FILE_TEST_ID},
  /// };
  /// use laythe_native::stdio::StdioNative;
  /// use laythe_core::{Allocator, NO_GC};
  ///
  /// // an expression
  /// let mut gc = Allocator::default();
  /// let source = Source::new(gc.manage_str("3 / 2 + 10;", &NO_GC));
  ///
  /// let parser = Parser::new(&source, VM_FILE_TEST_ID);
  /// let (ast, _) = parser.parse();
  /// assert_eq!(ast.is_ok(), true);
  /// ```
  pub fn parse(mut self) -> (FeResult<Module<'a>>, LineOffsets) {
    (self.parse_inner(), self.scanner.line_offsets())
  }

  /// Allocate a node using the Source's bump allocator
  fn node<T>(&self, node: T) -> Box<'a, T> {
    self.source.node(node)
  }

  /// Allocate a vec using the Source's bump allocator
  fn vec<T>(&self) -> Vec<'a, T> {
    self.source.vec()
  }

  /// Allocate a new symbol table
  fn table(&self) -> SymbolTable<'a> {
    SymbolTable::new(self.vec())
  }

  fn parse_inner(&mut self) -> FeResult<Module<'a>> {
    to_fe_result(self.advance())?;

    // early exit if ""
    if let TokenKind::Eof = self.current.kind() {
      return Ok(Module::new(self.vec(), self.table()));
    }

    let mut decls = self.vec();
    while !to_fe_result(self.match_kind(TokenKind::Eof))? {
      decls.push(to_fe_result(self.decl())?)
    }

    self
      .consume(TokenKind::Eof, "Expected end of expression.")
      .map_err(|err| vec![err])?;

    if self.errors.is_empty() {
      Ok(Module::new(decls, self.table()))
    } else {
      Err(self.errors.clone())
    }
  }

  /// Parse a Laythe declaration, if an error occurred at a lower level attempt
  /// synchronize to provide more error messages
  fn decl(&mut self) -> ParseResult<Decl<'a>> {
    let decl = match self.current.kind() {
      TokenKind::Class => self
        .advance()
        .and_then(|()| self.class())
        .map(|class| Decl::Symbol(self.node(class))),
      TokenKind::Fun => self
        .advance()
        .and_then(|()| self.fun())
        .map(|fun| Decl::Symbol(self.node(fun))),
      TokenKind::Let => self
        .advance()
        .and_then(|()| self.let_())
        .map(|let_| Decl::Symbol(self.node(let_))),
      TokenKind::Trait => self
        .advance()
        .and_then(|()| self.trait_())
        .map(|trait_| Decl::Symbol(self.node(trait_))),
      TokenKind::Type => self
        .advance()
        .and_then(|()| self.type_decl())
        .map(|type_decl| Decl::Symbol(self.node(type_decl))),
      TokenKind::Export => self.advance().and_then(|()| self.export_declaration()),
      _ => self.stmt().map(|stmt| Decl::Stmt(self.node(stmt))),
    };

    decl.or_else(|error| self.synchronize(error))
  }

  /// Synchronize the parser to a sentinel token
  fn synchronize(&mut self, error: Diagnostic<VmFileId>) -> ParseResult<Decl<'a>> {
    self.errors.push(error);

    let mut tokens: Vec<Token> = self.vec();

    while self.current.kind() != TokenKind::Eof || self.previous.kind() == TokenKind::Semicolon {
      tokens.push(self.previous.clone());

      match self.current.kind() {
        TokenKind::Class
        | TokenKind::Fun
        | TokenKind::Let
        | TokenKind::For
        | TokenKind::While
        | TokenKind::If
        | TokenKind::Import
        | TokenKind::Export
        | TokenKind::Launch
        | TokenKind::Raise
        | TokenKind::Try
        | TokenKind::Trait
        | TokenKind::Type
        | TokenKind::Continue
        | TokenKind::Break
        | TokenKind::Return => {
          break;
        },
        _ => (),
      }

      self.advance()?;
    }

    Ok(Decl::Error(tokens.into_boxed_slice()))
  }

  /// Parse a statement
  fn stmt(&mut self) -> ParseResult<Stmt<'a>> {
    match self.current.kind() {
      TokenKind::Import => self.advance().and_then(|()| self.import()),
      TokenKind::Try => self.advance().and_then(|()| self.try_block()),
      TokenKind::Raise => self.advance().and_then(|()| self.raise()),
      TokenKind::If => self.advance().and_then(|()| self.if_()),
      TokenKind::Launch => self.advance().and_then(|()| self.launch()),
      TokenKind::For => self.advance().and_then(|()| self.for_()),
      TokenKind::While => self.advance().and_then(|()| self.while_()),
      TokenKind::Return => self.advance().and_then(|()| self.return_()),
      TokenKind::Continue => self.advance().and_then(|()| self.continue_()),
      TokenKind::Break => self.advance().and_then(|()| self.break_()),
      _ => self.expr_stmt(),
    }
  }

  /// Parse an expression
  fn expr(&mut self) -> ParseResult<Expr<'a>> {
    self.parse_precedence(Precedence::Assignment)
  }

  /// Parse a class declaration
  fn class(&mut self) -> ParseResult<Symbol<'a>> {
    let start = self.previous.start();

    self.consume(TokenKind::Identifier, "Expected class name.")?;
    let name = self.previous.clone();

    let type_params = if self.match_kind(TokenKind::Less)? {
      self.type_params()?
    } else {
      self.vec()
    };

    // see if this class inherits from another
    let super_class = if self.match_kind(TokenKind::Colon)? {
      self.consume(TokenKind::Identifier, "Expected superclass name.")?;
      let super_class = self.class_type()?;

      if name.str() == super_class.type_ref.name.str() {
        return self.error("A class cannot inherit from itself.");
      }

      Some(super_class)
    } else {
      None
    };

    // collect and categories all methods in the class
    self.consume_basic(TokenKind::LeftBrace, "Expected '{' before class body.")?;
    let mut type_members: Vec<TypeMember> = self.vec();
    let mut methods: Vec<Fun> = self.vec();
    let mut static_methods: Vec<Fun> = self.vec();
    let mut init: Option<Fun> = None;

    while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
      // We need to do a lookahead for ':' to determine
      // if we're looking a member or a method
      match self.current.kind() {
        // identifer may be method or member
        TokenKind::Identifier => {
          self.advance()?;
          let name = self.previous.clone();

          match self.current.kind() {
            TokenKind::Colon => {
              self.advance()?;
              let type_ = self.type_()?;
              self.consume_basic(
                TokenKind::Semicolon,
                "Expected ';' after class member declaration.",
              )?;
              type_members.push(TypeMember::new(name, type_));
            },
            _ => {
              let (fun_kind, method) = self.method(name, false)?;
              match fun_kind {
                FunKind::Method => methods.push(method),
                FunKind::Initializer => init = Some(method),
                _ => unreachable!(),
              }
            },
          }
        },

        // static we know must be a method
        TokenKind::Static => {
          self.advance()?;
          self.consume(
            TokenKind::Identifier,
            "Expected method name after 'static' keyword.",
          )?;
          let name = self.previous.clone();
          let (_, method) = self.method(name, true)?;
          static_methods.push(method);
        },
        _ => return self.error_current("Expected method or member declaration inside of class."),
      }
    }

    self.consume_basic(TokenKind::RightBrace, "Expected '}' after class body.")?;
    let end = self.previous.end();

    Ok(Symbol::Class(self.node(Class::new(
      name,
      Span { start, end },
      type_params,
      super_class,
      type_members,
      self.table(),
      init,
      methods,
      static_methods,
    ))))
  }

  /// Parse a function declaration
  fn fun(&mut self) -> ParseResult<Symbol<'a>> {
    let previous = mem::replace(&mut self.fun_kind, FunKind::Fun);

    self.consume(TokenKind::Identifier, "Expected function name.")?;
    let name = self.previous.clone();

    let type_params = if self.match_kind(TokenKind::Less)? {
      self.type_params()
    } else {
      Ok(self.vec())
    }?;

    let fun = self
      .function(name, type_params, BlockReturn::Can)
      .map(Symbol::Fun);

    self.fun_kind = previous;
    fun
  }

  /// Parse a variable declaration
  fn let_(&mut self) -> ParseResult<Symbol<'a>> {
    self.consume(TokenKind::Identifier, "Expected variable name.")?;
    let name = self.previous.clone();
    let previous_let_name = self.let_name.replace(name.clone());

    let type_ = if self.match_kind(TokenKind::Colon)? {
      Some(self.type_()?)
    } else {
      None
    };

    let value = if self.match_kind(TokenKind::Equal)? {
      Some(self.expr()?)
    } else {
      None
    };

    self.let_name = previous_let_name;

    self
      .consume_basic(
        TokenKind::Semicolon,
        "Expected ';' after variable declaration.",
      )
      .map(|()| Symbol::Let(Let::new(name, type_, value)))
  }

  /// Parse a trait declaration
  fn trait_(&mut self) -> ParseResult<Symbol<'a>> {
    self.consume(TokenKind::Identifier, "Expected trait name after 'trait'.")?;
    let name = self.previous.clone();

    let params = if self.match_kind(TokenKind::Less)? {
      self.type_params()
    } else {
      Ok(self.vec())
    }?;

    self.consume_basic(TokenKind::LeftBrace, "Expected '{' after trait name.")?;

    let mut members: Vec<TypeMember> = self.vec();
    let mut methods: Vec<TypeMethod> = self.vec();

    while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
      // We need to do a lookahead for ':' to determine
      // if we're looking a member or a method
      self.consume(
        TokenKind::Identifier,
        "Expected method or member declaration",
      )?;
      let name = self.previous.clone();

      match self.current.kind() {
        TokenKind::Colon => {
          self.advance()?;
          let type_ = self.type_()?;
          self.consume_basic(
            TokenKind::Semicolon,
            "Expected ';' after class member declaration.",
          )?;
          members.push(TypeMember::new(name, type_));
        },
        TokenKind::Less | TokenKind::LeftParen => {
          self.advance()?;
          let type_params = if self.match_kind(TokenKind::Less)? {
            self.type_params()?
          } else {
            self.vec()
          };
          let params = self.call_params(TokenKind::RightParen)?;
          let call_sig = self.call_signature(params, type_params)?;
          self.consume_basic(
            TokenKind::Semicolon,
            "Expected ';' after class member declaration.",
          )?;
          methods.push(TypeMethod::new(name, call_sig));
        },
        _ => self.error_at(
          self.current.clone(),
          "Expected member or method declaration inside trait.",
        )?,
      }
    }

    self.consume_basic(TokenKind::RightBrace, "Expected '}' after trait body.")?;

    let range = Span {
      start: name.start(),
      end: self.previous.end(),
    };
    Ok(Symbol::Trait(Trait::new(
      range, name, params, members, methods,
    )))
  }

  /// Parse a trait declaration
  fn type_decl(&mut self) -> ParseResult<Symbol<'a>> {
    self.consume(TokenKind::Identifier, "Expected type name after 'type'.")?;
    let name = self.previous.clone();

    let params = if self.match_kind(TokenKind::Less)? {
      self.type_params()
    } else {
      Ok(self.vec())
    }?;

    self.consume_basic(TokenKind::Equal, "Expected '=' after type name.")?;

    let type_ = self.type_()?;
    self.consume_basic(TokenKind::Semicolon, "Expected ';' after type declaration.")?;

    Ok(Symbol::TypeDecl(TypeDecl::new(name, params, type_)))
  }

  /// Parse a symbol export declaration
  fn export_declaration(&mut self) -> ParseResult<Decl<'a>> {
    let symbol = match self.current.kind() {
      TokenKind::Class => self.advance().and_then(|()| self.class()),
      TokenKind::Fun => self.advance().and_then(|()| self.fun()),
      TokenKind::Let => self.advance().and_then(|()| self.let_()),
      TokenKind::Trait => self.advance().and_then(|()| self.trait_()),
      TokenKind::Type => self.advance().and_then(|()| self.type_decl()),
      _ => self.error_current("Can only export variable, function or class declarations."),
    }?;

    Ok(Decl::Export(self.node(symbol)))
  }

  /// Parse an import statement
  fn import(&mut self) -> ParseResult<Stmt<'a>> {
    if self.scope_depth > 0 {
      return self.error_current("Can only import from the module scope.");
    }

    match self.current.kind() {
      TokenKind::Identifier | TokenKind::Self_ => self.advance(),
      _ => self.error_current("Expected package name following import."),
    }?;

    let mut path = self.vec();
    path.push(self.previous.clone());

    while self.match_kind(TokenKind::Dot)? {
      self.consume(TokenKind::Identifier, "Expect import path after '.'")?;
      path.push(self.previous.clone())
    }

    let stem = match self.current.kind() {
      TokenKind::Colon => self.advance().and_then(|()| {
        self.consume(TokenKind::LeftBrace, "Expected '{' after ':'.")?;
        let mut symbols = self.vec();

        while !self.check(TokenKind::RightBrace) {
          self.consume(
            TokenKind::Identifier,
            "Expected symbol identifier in import.",
          )?;
          let symbol = self.previous.clone();

          if self.match_kind(TokenKind::As)? {
            self.consume(TokenKind::Identifier, "Expected identifier after 'as'.")?;
            symbols.push(ImportSym::new(symbol, Some(self.previous.clone())))
          } else {
            symbols.push(ImportSym::new(symbol, None))
          }

          if !self.match_kind(TokenKind::Comma)? {
            break;
          }
        }

        self.consume(
          TokenKind::RightBrace,
          "Expected '}' following import symbols",
        )?;
        Ok(ImportStem::Symbols(symbols))
      }),
      TokenKind::As => self.advance().and_then(|()| {
        self.consume(TokenKind::Identifier, "Expected identifier after 'as'.")?;
        Ok(ImportStem::Rename(self.previous.clone()))
      }),
      _ => Ok(ImportStem::None),
    }?;

    self
      .consume_basic(TokenKind::Semicolon, "Expected ';' after value.")
      .map(|()| Stmt::Import(self.node(Import::new(path, stem))))
  }

  /// Parse a try catch block
  fn try_block(&mut self) -> ParseResult<Stmt<'a>> {
    let block = self
      .consume(TokenKind::LeftBrace, "Expected '{' after try.")
      .and_then(|()| self.block(BlockReturn::Cannot))?;

    let mut catches: Vec<Catch> = self.vec();

    while self.match_kind(TokenKind::Catch)? {
      self.consume(TokenKind::Identifier, "Expected variable name.")?;
      let name = self.previous.clone();

      let class = if self.match_kind(TokenKind::Colon)? {
        self.consume(TokenKind::Identifier, "Expected class name.")?;
        Some(self.previous.clone())
      } else {
        None
      };

      let block = self
        .consume(TokenKind::LeftBrace, "Expected '{' after catch expression.")
        .and_then(|()| self.block(BlockReturn::Cannot))?;

      let table: SymbolTable<'_> = self.table();

      catches.push(Catch::new(name, class, table, block));
    }

    if catches.is_empty() {
      return self.error("Expect 'catch' after try block ");
    }

    Ok(Stmt::Try(self.node(Try::new(block, catches))))
  }

  /// Parse a raise statement
  fn raise(&mut self) -> ParseResult<Stmt<'a>> {
    let error = self.expr()?;

    self.consume_basic(TokenKind::Semicolon, "Expected ';' launch call.")?;
    Ok(Stmt::Raise(self.node(Raise::new(error))))
  }

  /// Parse a launch statement
  fn launch(&mut self) -> ParseResult<Stmt<'a>> {
    let closure = self.expr()?;

    if let Expr::Atom(atom) = &closure {
      match atom.trailers.last() {
        Some(Trailer::Call(_)) => {
          self.consume_basic(TokenKind::Semicolon, "Expected ';' launch call.")?;
          Ok(Stmt::Launch(self.node(Launch::new(closure))))
        },
        _ => self.error("Expected call following 'launch'."),
      }
    } else {
      self.error("Expected call following 'launch'.")
    }
  }

  /// Parse a if statement
  fn if_(&mut self) -> ParseResult<Stmt<'a>> {
    // parse the condition
    let cond = self.expr()?;
    self.consume_basic(TokenKind::LeftBrace, "Expected '{' after condition.")?;

    // parse then branch
    let body = self.block(BlockReturn::Cannot)?;

    // parse else branch if it exists
    let else_body = if self.match_kind(TokenKind::Else)? {
      if self.match_kind(TokenKind::LeftBrace)? {
        Some(Else::Block(self.block(BlockReturn::Cannot)?))
      } else if self.match_kind(TokenKind::If)? {
        if let Stmt::If(if_) = self.if_()? {
          Some(Else::If(if_))
        } else {
          unreachable!()
        }
      } else {
        return self.error_current("Expected 'if' or block following else.");
      }
    } else {
      None
    };

    Ok(Stmt::If(self.node(If::new(cond, body, else_body))))
  }

  /// Increment the loop depth
  fn loop_<T>(&mut self, cb: impl FnOnce(&mut Self) -> T) -> T {
    self.loop_depth += 1;
    let result = cb(self);
    self.loop_depth -= 1;
    result
  }

  /// Parse a for loop
  fn for_(&mut self) -> ParseResult<Stmt<'a>> {
    let table: SymbolTable<'_> = self.table();

    self.loop_(|self_| {
      self_.consume(TokenKind::Identifier, "Expected identifer after 'for'.")?;
      let item = self_.previous.clone();

      self_.consume(TokenKind::In, "Expected 'in' following for loop variable.")?;
      let iter = self_.expr()?;

      self_
        .consume(TokenKind::LeftBrace, "Expected '{' after iterable.")
        .and_then(|()| self_.block(BlockReturn::Cannot))
        .map(|body| Stmt::For(self_.node(For::new(item, iter, table, body))))
    })
  }

  /// Parse while statement
  fn while_(&mut self) -> ParseResult<Stmt<'a>> {
    self.loop_(|self_| {
      let cond = self_.expr()?;
      self_.consume(TokenKind::LeftBrace, "Expected '{' after while condition.")?;

      let body = self_.block(BlockReturn::Cannot)?;
      Ok(Stmt::While(self_.node(While::new(cond, body))))
    })
  }

  /// Parse a return statement
  fn return_(&mut self) -> ParseResult<Stmt<'a>> {
    if let FunKind::Script = self.fun_kind {
      return self.error("Cannot return from outside of a function or method.");
    }

    let return_ = self.previous.clone();
    if self.match_kind(TokenKind::Semicolon)? {
      Ok(Stmt::Return(self.node(Return::new(return_, None))))
    } else {
      let expr = self.expr()?;
      let result = self
        .consume_basic(TokenKind::Semicolon, "Expected ';' after return value.")
        .map(|()| Stmt::Return(self.node(Return::new(return_, Some(expr)))));

      if let FunKind::Initializer = self.fun_kind {
        self.error("Cannot return from outside of a function or method.")
      } else {
        result
      }
    }
  }

  fn continue_(&mut self) -> ParseResult<Stmt<'a>> {
    if self.loop_depth == 0 {
      return self.error("Cannot continue from outside of a loop.");
    }

    let continue_ = self.previous.clone();
    self.consume_basic(TokenKind::Semicolon, "Expected ';' after continue.")?;
    Ok(Stmt::Continue(self.node(continue_)))
  }

  fn break_(&mut self) -> ParseResult<Stmt<'a>> {
    if self.loop_depth == 0 {
      return self.error("Cannot break from outside of a loop.");
    }

    let break_ = self.previous.clone();
    self.consume_basic(TokenKind::Semicolon, "Expected ';' after break.")?;
    Ok(Stmt::Break(self.node(break_)))
  }

  /// Parse an expression statement
  fn expr_stmt(&mut self) -> ParseResult<Stmt<'a>> {
    let expr = self.expr()?;
    if self.match_kind(TokenKind::Semicolon)? {
      Ok(Stmt::Expr(self.node(expr)))
    } else {
      if let BlockReturn::Cannot = self.block_return {
        return self.error("Expected ';' after expression");
      }

      Ok(Stmt::ImplicitReturn(self.node(expr)))
    }
  }

  /// Parse an expression using a Pratt parser
  fn parse_precedence(&mut self, precedence: Precedence) -> ParseResult<Expr<'a>> {
    self.advance()?;

    let can_assign = precedence <= Precedence::Assignment;
    let prefix_fn = get_prefix(self.previous.kind()).op;

    // apply some prefix action
    let mut expr = match prefix_fn {
      Some(prefix) => self.prefix(prefix, can_assign),
      None => return self.error("Expected expression."),
    }?;

    // while we still have binding power keep applying infix operations
    while precedence <= get_infix(self.current.kind()).precedence {
      self.advance()?;
      let infix_fn = get_infix(self.previous.kind()).op;

      expr = match infix_fn {
        Some(infix) => self.infix(infix, expr, can_assign),
        None => return self.error("Expected expression."),
      }?;
    }

    // provide error if we failed to assign at some point
    if can_assign && self.match_kind(TokenKind::Equal)? {
      return self.error("Invalid assignment target.");
    }

    Ok(expr)
  }

  /// Execute an prefix action
  fn prefix(&mut self, action: Prefix, can_assign: bool) -> ParseResult<Expr<'a>> {
    match action {
      Prefix::Channel => self.channel(),
      Prefix::Grouping => self.grouping(),
      Prefix::Interpolation => self.interpolation(),
      Prefix::Lambda => self.lambda(),
      Prefix::List => self.list(),
      Prefix::Literal => Ok(self.literal()),
      Prefix::Map => self.map(),
      Prefix::Number => Ok(self.number()),
      Prefix::Self_ => Ok(self.self_()),
      Prefix::String => Ok(self.string()),
      Prefix::Super => self.super_(),
      Prefix::Unary => self.unary(),
      Prefix::Variable => self.variable(can_assign),
      Prefix::InstanceAccess => self.instance_access(can_assign),
    }
  }

  /// Execute an infix action
  fn infix(&mut self, action: Infix, lhs: Expr<'a>, can_assign: bool) -> ParseResult<Expr<'a>> {
    match action {
      Infix::And => self.and(lhs),
      Infix::Binary => self.binary(lhs),
      Infix::Ternary => self.ternary(lhs),
      Infix::Call => self.call(lhs),
      Infix::Index => self.index(lhs, can_assign),
      Infix::Dot => self.dot(lhs, can_assign),
      Infix::Or => self.or(lhs),
    }
  }

  /// Parse a block statement
  fn block(&mut self, block_return: BlockReturn) -> ParseResult<Block<'a>> {
    let start = self.previous.start();

    self.scope_depth += 1;
    let block_return = mem::replace(&mut self.block_return, block_return);

    let mut decls: Vec<Decl> = self.vec();
    while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
      decls.push(self.decl().inspect_err(|_| {
        self.scope_depth -= 1;
      })?);
    }

    if let BlockReturn::Can = self.block_return {
      if let Some((_, rest)) = decls.split_last() {
        for decl in rest {
          if let Decl::Stmt(stmt) = decl {
            if let Stmt::ImplicitReturn(return_) = &**stmt {
              return self.error_at::<Block<'a>>(
                Token::new(
                  TokenKind::Error,
                  Lexeme::Slice(""),
                  return_.start(),
                  return_.end(),
                ),
                "Implicit return can only appear at the end of a block.",
              );
            }
          }
        }
      }
    }

    self.scope_depth -= 1;
    self.block_return = block_return;

    let end = self.current.end();

    self
      .consume(TokenKind::RightBrace, "Expected '}' after block.")
      .map(|()| Block::new(Span { start, end }, self.table(), decls))
  }

  fn ternary(&mut self, cond: Expr<'a>) -> ParseResult<Expr<'a>> {
    let then = self.expr()?;
    self.consume_basic(TokenKind::Colon, "Expected ':' after return value.")?;
    let else_ = self.expr()?;

    Ok(Expr::Ternary(self.node(Ternary::new(cond, then, else_))))
  }

  /// Parse a binary expression
  fn binary(&mut self, lhs: Expr<'a>) -> ParseResult<Expr<'a>> {
    let operator_kind = self.previous.kind();
    let precedence = get_infix(operator_kind).precedence.higher();
    let rhs = self.parse_precedence(precedence)?;

    let op = match operator_kind {
      TokenKind::BangEqual => BinaryOp::Ne,
      TokenKind::EqualEqual => BinaryOp::Eq,
      TokenKind::Greater => BinaryOp::Gt,
      TokenKind::GreaterEqual => BinaryOp::GtEq,
      TokenKind::Less => BinaryOp::Lt,
      TokenKind::LessEqual => BinaryOp::LtEq,
      TokenKind::Plus => BinaryOp::Add,
      TokenKind::Minus => BinaryOp::Sub,
      TokenKind::Star => BinaryOp::Mul,
      TokenKind::Slash => BinaryOp::Div,
      _ => unreachable!("Invalid operator"),
    };

    Ok(Expr::Binary(self.node(Binary::new(op, lhs, rhs))))
  }

  /// Parse an and expression
  fn and(&mut self, lhs: Expr<'a>) -> ParseResult<Expr<'a>> {
    let rhs = self.parse_precedence(Precedence::And)?;
    Ok(Expr::Binary(self.node(Binary::new(
      BinaryOp::And,
      lhs,
      rhs,
    ))))
  }

  /// Parse an or expression
  fn or(&mut self, lhs: Expr<'a>) -> ParseResult<Expr<'a>> {
    let rhs = self.parse_precedence(Precedence::Or)?;
    Ok(Expr::Binary(self.node(Binary::new(BinaryOp::Or, lhs, rhs))))
  }

  /// Parse a unary expression
  fn unary(&mut self) -> ParseResult<Expr<'a>> {
    let operator_kind = self.previous.kind();
    let expr = self.parse_precedence(Precedence::Unary)?;

    let op = match operator_kind {
      TokenKind::Minus => UnaryOp::Negate,
      TokenKind::Bang => UnaryOp::Not,
      TokenKind::LeftArrow => UnaryOp::Receive,
      _ => unimplemented!("Unexpected unary operator"),
    };

    Ok(Expr::Unary(self.node(Unary::new(op, expr))))
  }

  /// Parse a call invocation
  fn call(&mut self, mut lhs: Expr<'a>) -> ParseResult<Expr<'a>> {
    let start = self.previous.start();
    let args = self.consume_arguments(None, TokenKind::RightParen, u8::MAX as usize)?;
    self.consume_basic(TokenKind::RightParen, "Expected ')' after arguments")?;

    let range = Span {
      start,
      end: self.previous.end(),
    };

    if let Expr::Atom(atom) = &mut lhs {
      atom
        .trailers
        .push(Trailer::Call(self.node(Call::new(range, args))))
    } else {
      return self.error("Expected expression. TODO can you get here?");
    }

    Ok(lhs)
  }

  /// Parse an index on an atom
  fn index(&mut self, mut expr: Expr<'a>, can_assign: bool) -> ParseResult<Expr<'a>> {
    let indexer = self.expr()?;
    self.consume_basic(TokenKind::RightBracket, "Expected ']' after index")?;

    if let Expr::Atom(atom) = &mut expr {
      atom
        .trailers
        .push(Trailer::Index(self.node(Index::new(indexer))))
    } else {
      return self.error("Expected expression. TODO can you get here?");
    }

    if can_assign {
      if let Expr::Atom(atom) = expr {
        expr = self.assign(Box::into_inner(atom))?;
      } else {
        return self.error("Expected expression. TODO can you get here?");
      }
    }

    Ok(expr)
  }

  /// Parse a property access
  fn dot(&mut self, mut expr: Expr<'a>, can_assign: bool) -> ParseResult<Expr<'a>> {
    self.consume(TokenKind::Identifier, "Expected property name after '.'.")?;

    if let Expr::Atom(atom) = &mut expr {
      atom.trailers.push(Trailer::Access(
        self.node(Access::new(self.previous.clone())),
      ))
    } else {
      return self.error("Expected expression. TODO can you get here?");
    }

    if can_assign {
      if let Expr::Atom(atom) = expr {
        expr = self.assign(Box::into_inner(atom))?;
      } else {
        return self.error("Expected expression. TODO can you get here?");
      }
    }

    Ok(expr)
  }

  /// Parse a lambda expression
  fn lambda(&mut self) -> ParseResult<Expr<'a>> {
    // parse function parameters
    let call_sig = if self.previous.kind() == TokenKind::Or {
      self.call_signature(self.vec(), self.vec())?
    } else {
      let params = self.call_params(TokenKind::Pipe)?;
      self.call_signature(params, self.vec())?
    };

    let previous = mem::replace(&mut self.fun_kind, FunKind::Fun);
    let lambda = self.fun_body(BlockReturn::Can).map(|body| {
      self.atom_expr(Primary::Lambda(self.node(Fun::new(
        self.let_name.clone(),
        call_sig,
        self.table(),
        body,
      ))))
    });

    self.fun_kind = previous;
    lambda
  }

  fn channel(&mut self) -> ParseResult<Expr<'a>> {
    let start = self.previous.start();
    self.consume_basic(TokenKind::LeftParen, "Expected '(' after 'chan'")?;

    let mut expr = None;
    if !self.match_kind(TokenKind::RightParen)? {
      expr = Some(self.expr()?);
      self.consume_basic(TokenKind::RightParen, "Expected ')'")?;
    }

    let span = Span {
      start,
      end: self.previous.end(),
    };

    Ok(self.atom_expr(Primary::Channel(Channel::new(span, expr))))
  }

  /// Parse a grouping expression
  fn grouping(&mut self) -> ParseResult<Expr<'a>> {
    let start = self.previous.start();
    if self.match_kind(TokenKind::RightParen)? {
      let range = Span {
        start,
        end: self.previous.end(),
      };

      return Ok(self.atom_expr(Primary::Tuple(Collection::new(range, self.vec()))));
    }

    let expr = self.expr()?;
    if self.match_kind(TokenKind::Comma)? {
      let items = self.consume_arguments(Some(expr), TokenKind::RightParen, u16::MAX as usize)?;
      self.consume_basic(TokenKind::RightParen, "Expected ')' after expression")?;

      let range = Span {
        start,
        end: self.previous.end(),
      };

      Ok(self.atom_expr(Primary::Tuple(Collection::new(range, items))))
    } else {
      self.consume_basic(TokenKind::RightParen, "Expected ')' after expression")?;

      Ok(self.atom_expr(Primary::Grouping(self.node(expr))))
    }
  }

  /// Compile a variable statement
  fn variable(&mut self, can_assign: bool) -> ParseResult<Expr<'a>> {
    let mut expr = self.atom_expr(Primary::Ident(self.previous.clone()));

    if can_assign {
      if let Expr::Atom(atom) = expr {
        expr = self.assign(Box::into_inner(atom))?;
      } else {
        return self.error("Expected expression. TODO can you get here?");
      }
    }

    Ok(expr)
  }

  /// Compile a variable statement
  fn instance_access(&mut self, can_assign: bool) -> ParseResult<Expr<'a>> {
    let atom = Atom::new(
      Primary::InstanceAccess(InstanceAccess::new(self.previous.clone())),
      self.vec(),
    );

    if can_assign {
      self.assign(atom)
    } else {
      Ok(Expr::Atom(self.node(atom)))
    }
  }

  /// Parse a class's self identifier
  fn self_(&mut self) -> Expr<'a> {
    self.atom_expr(Primary::Self_(self.previous.clone()))
  }

  /// Parse a class' super identifer
  fn super_(&mut self) -> ParseResult<Expr<'a>> {
    let super_ = self.previous.clone();
    self.consume_basic(TokenKind::Dot, "Expected '.' after super.")?;
    self.consume(TokenKind::Identifier, "Expected identifier after '.'.")?;

    let access = self.previous.clone();
    Ok(self.atom_expr(Primary::Super(Super::new(super_, access))))
  }

  /// Parse a list literal
  fn list(&mut self) -> ParseResult<Expr<'a>> {
    let start = self.previous.start();
    let items = self.consume_arguments(None, TokenKind::RightBracket, u16::MAX as usize)?;
    self.consume_basic(TokenKind::RightBracket, "Expected ']' after arguments")?;

    let range = Span {
      start,
      end: self.previous.end(),
    };
    Ok(self.atom_expr(Primary::List(Collection::new(range, items))))
  }

  /// Parse a map literal
  fn map(&mut self) -> ParseResult<Expr<'a>> {
    let start = self.previous.start();
    let mut entries: Vec<(Expr, Expr)> = self.vec();

    while !self.check(TokenKind::RightBrace) {
      let key = self.expr()?;
      self.consume_basic(TokenKind::Colon, "Expected ':' after map key")?;
      let value = self.expr()?;

      if entries.len() == u16::MAX as usize {
        return self.error(&format!(
          "Cannot have more than {} key value pairs in map literal",
          entries.len()
        ));
      }

      entries.push((key, value));

      if !self.match_kind(TokenKind::Comma)? {
        break;
      }
    }

    self
      .consume(TokenKind::RightBrace, "Expected '}' after map")
      .map(|()| {
        self.atom_expr(Primary::Map(Map::new(
          Span {
            start,
            end: self.previous.end(),
          },
          entries,
        )))
      })
  }

  /// Parse a number literal
  fn number(&self) -> Expr<'a> {
    self.atom_expr(Primary::Number(self.previous.clone()))
  }

  /// Parse a string literal
  fn string(&self) -> Expr<'a> {
    self.atom_expr(Primary::String(self.previous.clone()))
  }

  /// Parse a string literal
  fn interpolation(&mut self) -> ParseResult<Expr<'a>> {
    let start = self.previous.clone();

    let mut segments: Vec<StringSegments> = self.vec();
    loop {
      if segments.len() == u16::MAX as usize {
        return self.error(&format!(
          "Cannot have more than {} segments in a string interpolation",
          segments.len()
        ));
      }

      match self.current.kind() {
        TokenKind::StringSegment => {
          self.advance()?;
          segments.push(StringSegments::Token(self.previous.clone()))
        },
        TokenKind::StringEnd => {
          break;
        },
        _ => {
          let expr = self.expr()?;
          segments.push(StringSegments::Expr(self.node(expr)))
        },
      }
    }
    self.consume(TokenKind::StringEnd, "Unterminated interpolated string.")?;
    let end = self.previous.clone();

    Ok(self.atom_expr(Primary::Interpolation(
      self.node(Interpolation::new(start, segments, end)),
    )))
  }

  /// Parse a literal
  fn literal(&self) -> Expr<'a> {
    let previous = self.previous.clone();
    match self.previous.kind() {
      TokenKind::True => self.atom_expr(Primary::True(previous)),
      TokenKind::False => self.atom_expr(Primary::False(previous)),
      TokenKind::Nil => self.atom_expr(Primary::Nil(previous)),
      _ => unreachable!("Unexpected token kind {:?}", previous.kind()),
    }
  }

  /// Create an atom expression from a primary
  fn atom_expr(&self, primary: Primary<'a>) -> Expr<'a> {
    Expr::Atom(self.node(self.atom(primary)))
  }

  /// Create an atom expression from a primary
  fn atom(&self, primary: Primary<'a>) -> Atom<'a> {
    Atom::new(primary, self.vec())
  }

  /// Parse a calls parameters
  fn call_params(&mut self, stop_kind: TokenKind) -> ParseResult<Vec<'a, Param<'a>>> {
    let mut params = self.vec();
    let mut arity: u16 = 0;

    if !self.check(stop_kind) {
      loop {
        arity += 1;
        if arity == u8::MAX as u16 {
          return self.error_current("Cannot have more than 255 parameters.");
        }

        self.consume(TokenKind::Identifier, "Expected parameter name.")?;
        let name = self.previous.clone();

        let type_ = if self.match_kind(TokenKind::Colon)? {
          Some(self.type_()?)
        } else {
          None
        };

        params.push(Param::new(name, type_));

        if !self.match_kind(TokenKind::Comma)? {
          break;
        }
      }
    }

    let stop_char = match stop_kind {
      TokenKind::RightParen => ")",
      TokenKind::Pipe => "|",
      _ => unreachable!("Unexpected stop token encountered."),
    };

    self.consume_basic(
      stop_kind,
      &format!("Expected {stop_char} after parameter list."),
    )?;

    Ok(params)
  }

  /// Parse the current functions call signature
  fn call_signature(
    &mut self,
    params: Vec<'a, Param<'a>>,
    type_params: Vec<'a, TypeParam<'a>>,
  ) -> ParseResult<CallSignature<'a>> {
    let start = type_params
      .first()
      .map_or_else(|| self.previous.start(), |first| first.start());

    let return_type = if self.match_kind(TokenKind::RightArrow)? {
      Some(self.type_()?)
    } else {
      None
    };

    let range = Span {
      start,
      end: self.previous.end(),
    };
    Ok(CallSignature::new(range, type_params, params, return_type))
  }

  fn assign(&mut self, atom: Atom<'a>) -> ParseResult<Expr<'a>> {
    match self.current.kind() {
      TokenKind::Equal => self
        .advance()
        .and_then(|()| self.expr())
        .map(|rhs| Expr::Assign(self.node(Assign::new(atom, rhs)))),
      TokenKind::LeftArrow => self
        .advance()
        .and_then(|()| self.expr())
        .map(|rhs| Expr::Send(self.node(Send::new(atom, rhs)))),
      TokenKind::PlusEqual => self.advance().and_then(|()| self.expr()).map(|rhs| {
        Expr::AssignBinary(self.node(AssignBinary::new(atom, AssignBinaryOp::Add, rhs)))
      }),
      TokenKind::MinusEqual => self.advance().and_then(|()| self.expr()).map(|rhs| {
        Expr::AssignBinary(self.node(AssignBinary::new(atom, AssignBinaryOp::Sub, rhs)))
      }),
      TokenKind::StarEqual => self.advance().and_then(|()| self.expr()).map(|rhs| {
        Expr::AssignBinary(self.node(AssignBinary::new(atom, AssignBinaryOp::Mul, rhs)))
      }),
      TokenKind::SlashEqual => self.advance().and_then(|()| self.expr()).map(|rhs| {
        Expr::AssignBinary(self.node(AssignBinary::new(atom, AssignBinaryOp::Div, rhs)))
      }),
      _ => Ok(Expr::Atom(self.node(atom))),
    }
  }

  /// Consume a comma separated set of arguments for a call or list
  fn consume_arguments(
    &mut self,
    first_value: Option<Expr<'a>>,
    stop_token: TokenKind,
    max: usize,
  ) -> ParseResult<Vec<'a, Expr<'a>>> {
    let mut args = self.vec();

    if let Some(value) = first_value {
      args.push(value);
    }

    while !self.check(stop_token) {
      args.push(self.expr()?);

      if args.len() == max {
        return self.error(&format!("Cannot have more than {max} arguments"));
      }

      if !self.match_kind(TokenKind::Comma)? {
        break;
      }
    }

    Ok(args)
  }

  // Parse a function body
  fn fun_body(&mut self, block_return: BlockReturn) -> ParseResult<FunBody<'a>> {
    if self.match_kind(TokenKind::LeftBrace)? {
      let block = self.block(block_return)?;
      Ok(FunBody::Block(self.node(block)))
    } else {
      // implicitly return expression lambdas
      let expr = self.expr()?;
      Ok(FunBody::Expr(self.node(expr)))
    }
  }

  /// Parse a function declaration and body
  fn function(
    &mut self,
    name: Token<'a>,
    type_params: Vec<'a, TypeParam<'a>>,
    block_return: BlockReturn,
  ) -> ParseResult<Fun<'a>> {
    if !self.match_kind(TokenKind::LeftParen)? {
      return self.error_current(&format!("Expected '(' after {} name.", self.fun_kind));
    }

    let loop_depth = self.loop_depth;
    self.loop_depth = 0;

    // parse function parameters
    let call_params = self.call_params(TokenKind::RightParen)?;
    let call_sig = self.call_signature(call_params, type_params)?;

    if !self.match_kind(TokenKind::LeftBrace)? {
      return self.error_current(&format!("Expected '{{' after {} signature.", self.fun_kind));
    }

    let fun = self.block(block_return).map(|body| {
      Fun::new(
        Some(name),
        call_sig,
        self.table(),
        FunBody::Block(self.node(body)),
      )
    });

    self.loop_depth = loop_depth;
    fun
  }

  /// Parse a method declaration and body
  fn method(&mut self, name: Token<'a>, is_static: bool) -> ParseResult<(FunKind, Fun<'a>)> {
    let (fun_kind, block_return) = if is_static {
      (FunKind::StaticMethod, BlockReturn::Can)
    } else if INIT == name.str() {
      (FunKind::Initializer, BlockReturn::Cannot)
    } else {
      (FunKind::Method, BlockReturn::Can)
    };

    let previous = mem::replace(&mut self.fun_kind, fun_kind);
    let type_params = if self.match_kind(TokenKind::Less)? {
      self.type_params()?
    } else {
      self.vec()
    };

    let method = self
      .function(name, type_params, block_return)
      .map(|fun| (fun_kind, fun));
    self.fun_kind = previous;
    method
  }

  /// Parse type parameters
  fn type_params(&mut self) -> ParseResult<Vec<'a, TypeParam<'a>>> {
    let mut type_params: Vec<TypeParam> = self.vec();

    while !self.check(TokenKind::Greater) {
      self.consume_basic(
        TokenKind::Identifier,
        "Expected identifer for type parameter.",
      )?;
      let name = self.previous.clone();

      let constraint = if self.match_kind(TokenKind::Colon)? {
        Some(self.type_()?)
      } else {
        None
      };

      type_params.push(TypeParam::new(name, constraint));

      if !self.match_kind(TokenKind::Comma)? {
        break;
      }
    }

    self.consume_basic(TokenKind::Greater, "Expected '>' after type parameters.")?;
    Ok(type_params)
  }

  /// Parse a type
  fn type_(&mut self) -> ParseResult<Type<'a>> {
    self.parse_type_precedence(TypePrecedence::Union)
  }

  /// Use a pratt parser to generate a type
  fn parse_type_precedence(&mut self, precedence: TypePrecedence) -> ParseResult<Type<'a>> {
    self.advance()?;

    let prefix_fn = get_type_prefix(self.previous.kind()).op;

    // apply some prefix action
    let mut type_ = match prefix_fn {
      Some(prefix) => self.type_prefix(prefix),
      None => return self.error("Expected type expression."),
    }?;

    // while we still have binding power keep applying infix operations
    while precedence <= get_type_infix(self.current.kind()).precedence {
      self.advance()?;
      let infix_fn = get_type_infix(self.previous.kind()).op;

      type_ = match infix_fn {
        Some(infix) => self.type_infix(infix, type_),
        None => return self.error("Expected type expression."),
      }?;
    }

    Ok(type_)
  }

  /// Execute an prefix action
  fn type_prefix(&mut self, action: TypePrefix) -> ParseResult<Type<'a>> {
    match action {
      TypePrefix::Fun => {
        let params = self.call_params(TokenKind::RightParen)?;
        let call_sig = self.call_signature(params, self.vec())?;
        Ok(Type::Fun(self.node(call_sig)))
      },
      TypePrefix::Literal => self.type_literal(),
    }
  }

  /// Execute an infix action
  fn type_infix(&mut self, action: TypeInfix, lhs: Type<'a>) -> ParseResult<Type<'a>> {
    match action {
      TypeInfix::TypeArgs => self.type_ref(lhs),
      TypeInfix::List => self.list_type(lhs),
      TypeInfix::Intersection => self.intersection(lhs),
      TypeInfix::Union => self.union(lhs),
    }
  }

  /// Create a intersection type
  fn intersection(&mut self, lhs: Type<'a>) -> ParseResult<Type<'a>> {
    let rhs = self.parse_type_precedence(TypePrecedence::List)?;
    Ok(Type::Intersection(self.node(Intersection::new(lhs, rhs))))
  }

  /// Create a union type
  fn union(&mut self, lhs: Type<'a>) -> ParseResult<Type<'a>> {
    let rhs = self.parse_type_precedence(TypePrecedence::Intersection)?;
    Ok(Type::Union(self.node(Union::new(lhs, rhs))))
  }

  /// Create a list type
  fn list_type(&mut self, lhs: Type<'a>) -> ParseResult<Type<'a>> {
    self.consume_basic(TokenKind::RightBracket, "Expected ']' after list type.")?;
    Ok(Type::List(self.node(ListType::new(lhs))))
  }

  /// Create a type literal
  fn type_literal(&mut self) -> ParseResult<Type<'a>> {
    let token = self.previous.clone();
    match token.kind() {
      TokenKind::Nil => Ok(Type::Primitive(Primitive::Nil(token))),
      TokenKind::Identifier => match self.previous.str() {
        BOOL_TYPE => Ok(Type::Primitive(Primitive::Bool(token))),
        STRING_TYPE => Ok(Type::Primitive(Primitive::String(token))),
        NUMBER_TYPE => Ok(Type::Primitive(Primitive::Number(token))),
        ANY_TYPE => Ok(Type::Primitive(Primitive::Any(token))),

        // if we're not a primitive type then we're a type ref
        _ => Ok(Type::Ref(self.node(TypeRef::new(token, self.vec())))),
      },

      // This should not occur it means the table is messed up
      _ => panic!(
        "This should not be able to get to type_literal with kind {:?}",
        self.previous.kind()
      ),
    }
  }

  /// Parse a class type
  fn class_type(&mut self) -> ParseResult<ClassType<'a>> {
    let name = self.previous.clone();

    let type_args = if self.match_kind(TokenKind::Less)? {
      self.type_args()?
    } else {
      self.vec()
    };

    Ok(ClassType::new(TypeRef::new(name, type_args)))
  }

  /// Parse type ref
  fn type_ref(&mut self, lhs: Type<'a>) -> ParseResult<Type<'a>> {
    match lhs {
      Type::Ref(mut type_ref) => {
        type_ref.type_args = self.type_args()?;
        Ok(Type::Ref(type_ref))
      },
      _ => {
        // TODO: maybe
        self.error("Can only apply type argument to a non primitive type identifier.")
      },
    }
  }

  /// Parse a set of type args
  fn type_args(&mut self) -> ParseResult<Vec<'a, Type<'a>>> {
    let args = self.consume_type_args(u8::MAX as usize)?;
    self.consume_basic(TokenKind::Greater, "Expected '>' after arguments")?;

    Ok(args)
  }

  #[inline]
  fn consume_basic(&mut self, kind: TokenKind, message: &str) -> ParseResult<()> {
    self.consume(kind, message).map_err(|err| {
      let mut labels = err.labels.clone();
      labels.push(
        Label::secondary(self.file_id, self.previous.span())
          .with_message(format!("Consider adding a '{kind}' after this.")),
      );

      err.with_labels(labels)
    })
  }

  /// Consume a comma separated set of arguments for calls and lists
  fn consume_type_args(&mut self, max: usize) -> ParseResult<Vec<'a, Type<'a>>> {
    let mut args = self.vec();

    while !self.check(TokenKind::Greater) {
      args.push(self.type_()?);

      if args.len() == max {
        return self.error(&format!("Cannot have more than {max} type arguments."));
      }

      if !self.match_kind(TokenKind::Comma)? {
        break;
      }
    }

    Ok(args)
  }

  /// Does the provided token kind match if so advance the
  /// token index
  #[inline]
  fn match_kind(&mut self, kind: TokenKind) -> ParseResult<bool> {
    if !self.check(kind) {
      return Ok(false);
    }
    self.advance()?;
    Ok(true)
  }

  /// Does the provided token kind match the current kind
  #[inline]
  fn check(&self, kind: TokenKind) -> bool {
    self.current.kind() == kind
  }

  /// Advance the parser a token forward
  #[inline]
  fn advance(&mut self) -> ParseResult<()> {
    self.previous = mem::replace(&mut self.current, self.scanner.scan_token());

    if self.current.kind() != TokenKind::Error {
      return Ok(());
    }

    let token = self.current.clone();
    self.error_current(token.str())
  }

  /// Consume a token and advance the current token index
  #[inline]
  fn consume(&mut self, kind: TokenKind, message: &str) -> ParseResult<()> {
    if self.current.kind() == kind {
      return self.advance();
    }

    self.error_current(message)
  }

  /// Indicate an error occurred at he current index
  fn error_current<T>(&mut self, message: &str) -> ParseResult<T> {
    self.error_at(self.current.clone(), message)
  }

  /// Indicate an error occurred at the previous index
  fn error<T>(&mut self, message: &str) -> ParseResult<T> {
    self.error_at(self.previous.clone(), message)
  }

  /// Print an error to the console for a user to address
  fn error_at<T>(&mut self, token: Token<'a>, message: &str) -> ParseResult<T> {
    let error = Diagnostic::error()
      .with_message(message)
      .with_labels(vec![Label::primary(self.file_id, token.span())]);

    Err(error)
  }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Precedence {
  None,
  Assignment,
  Ternary,
  Or,
  And,
  Equality,
  Comparison,
  Term,
  Factor,
  Unary,
  Call,
  Primary,
}

impl Precedence {
  fn higher(&self) -> Precedence {
    match self {
      Precedence::None => Precedence::Assignment,
      Precedence::Assignment => Precedence::Ternary,
      Precedence::Ternary => Precedence::Or,
      Precedence::Or => Precedence::And,
      Precedence::And => Precedence::Equality,
      Precedence::Equality => Precedence::Comparison,
      Precedence::Comparison => Precedence::Term,
      Precedence::Term => Precedence::Factor,
      Precedence::Factor => Precedence::Unary,
      Precedence::Unary => Precedence::Call,
      Precedence::Call => Precedence::Primary,
      Precedence::Primary => panic!("Primary is highest precedence"),
    }
  }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum TypePrecedence {
  None,
  Union,
  Intersection,
  List,
  TypeArgs,
  Primary,
}

struct Rule<T, P> {
  op: Option<T>,
  precedence: P,
}

impl<T, P> Rule<T, P> {
  const fn new(op: Option<T>, precedence: P) -> Self {
    Self { op, precedence }
  }
}

#[derive(Clone, Copy)]
enum Prefix {
  Channel,
  Grouping,
  Interpolation,
  Lambda,
  List,
  Literal,
  Map,
  Number,
  Self_,
  String,
  Super,
  Unary,
  InstanceAccess,
  Variable,
}

#[derive(Clone, Copy)]
enum Infix {
  And,
  Binary,
  Ternary,
  Call,
  Dot,
  Index,
  Or,
}

#[derive(Clone, Copy)]
enum TypePrefix {
  Fun,
  Literal,
}

#[derive(Clone, Copy)]
enum TypeInfix {
  TypeArgs,
  List,
  Intersection,
  Union,
}

/// The rules for infix and prefix operators
const PREFIX_TABLE: [Rule<Prefix, Precedence>; TokenKind::VARIANT_COUNT] = [
  Rule::new(Some(Prefix::Grouping), Precedence::Call),
  // LEFT_PAREN
  Rule::new(None, Precedence::None),
  // RIGHT_PAREN
  Rule::new(Some(Prefix::Map), Precedence::Call),
  // LEFT_BRACE
  Rule::new(None, Precedence::None),
  // RIGHT_BRACE
  Rule::new(Some(Prefix::List), Precedence::Call),
  // LEFT_BRACKET
  Rule::new(None, Precedence::None),
  // RIGHT_BRACKET
  Rule::new(None, Precedence::None),
  // COMMA
  Rule::new(None, Precedence::None),
  // DOT
  Rule::new(Some(Prefix::Unary), Precedence::Term),
  // MINUS
  Rule::new(None, Precedence::None),
  // PLUS
  Rule::new(None, Precedence::None),
  // QUESTION_MARK
  Rule::new(None, Precedence::None),
  // COLON
  Rule::new(None, Precedence::None),
  // SEMICOLON
  Rule::new(Some(Prefix::Lambda), Precedence::Call),
  // PIPE
  Rule::new(None, Precedence::None),
  // SLASH
  Rule::new(None, Precedence::None),
  // STAR
  Rule::new(None, Precedence::None),
  // PLUS_EQUAL
  Rule::new(None, Precedence::None),
  // MINUS_EQUAL
  Rule::new(None, Precedence::None),
  // SLASH_EQUAL
  Rule::new(None, Precedence::None),
  // STAR_EQUAL
  Rule::new(None, Precedence::None),
  // RIGHT_ARROW
  Rule::new(Some(Prefix::Unary), Precedence::None),
  // LEFT_ARROW
  Rule::new(None, Precedence::None),
  // EXPORT
  Rule::new(None, Precedence::None),
  // IMPORT
  Rule::new(None, Precedence::None),
  // FROM
  Rule::new(None, Precedence::None),
  // AMP
  Rule::new(Some(Prefix::Unary), Precedence::None),
  // BANG
  Rule::new(None, Precedence::None),
  // BANG_EQUAL
  Rule::new(None, Precedence::None),
  // EQUAL
  Rule::new(None, Precedence::None),
  // EQUAL_EQUAL
  Rule::new(None, Precedence::None),
  // GREATER
  Rule::new(None, Precedence::None),
  // GREATER_EQUAL
  Rule::new(None, Precedence::None),
  // LESS
  Rule::new(None, Precedence::None),
  // LESS_EQUAL
  Rule::new(Some(Prefix::Variable), Precedence::None),
  // IDENTIFIER
  Rule::new(Some(Prefix::InstanceAccess), Precedence::None),
  // INSTANCE_ACCESS
  Rule::new(Some(Prefix::String), Precedence::None),
  // STRING
  Rule::new(Some(Prefix::Interpolation), Precedence::None),
  // STRING_BEGIN
  Rule::new(None, Precedence::None),
  // STRING_SEGMENT
  Rule::new(None, Precedence::None),
  // STRING_END
  Rule::new(Some(Prefix::Number), Precedence::None),
  // NUMBER
  Rule::new(None, Precedence::None),
  // AND
  Rule::new(None, Precedence::None),
  // CLASS
  Rule::new(None, Precedence::None),
  // ELSE
  Rule::new(Some(Prefix::Literal), Precedence::None),
  // FALSE
  Rule::new(None, Precedence::None),
  // FOR
  Rule::new(None, Precedence::None),
  // FUN
  Rule::new(None, Precedence::None),
  // IF
  Rule::new(None, Precedence::None),
  // IN
  Rule::new(Some(Prefix::Literal), Precedence::None),
  // NIL
  Rule::new(Some(Prefix::Lambda), Precedence::None),
  // OR
  Rule::new(None, Precedence::None),
  // RETURN
  Rule::new(None, Precedence::None),
  // BREAK
  Rule::new(None, Precedence::None),
  // CONTINUE
  Rule::new(Some(Prefix::Super), Precedence::None),
  // SUPER
  Rule::new(Some(Prefix::Self_), Precedence::None),
  // SELF
  Rule::new(None, Precedence::None),
  // STATIC
  Rule::new(Some(Prefix::Literal), Precedence::None),
  // TRUE
  Rule::new(None, Precedence::None),
  // VAR
  Rule::new(None, Precedence::None),
  // WHILE
  Rule::new(None, Precedence::None),
  // TRY
  Rule::new(None, Precedence::None),
  // CATCH
  Rule::new(None, Precedence::None),
  // RAISE
  Rule::new(None, Precedence::None),
  // TRAIT
  Rule::new(None, Precedence::None),
  // TYPE
  Rule::new(Some(Prefix::Channel), Precedence::None),
  // CHANNEL
  Rule::new(None, Precedence::None),
  // LAUNCH
  Rule::new(None, Precedence::None),
  // ERROR
  Rule::new(None, Precedence::None),
  // EOF
];

/// The rules for infix and prefix operators
const INFIX_TABLE: [Rule<Infix, Precedence>; TokenKind::VARIANT_COUNT] = [
  Rule::new(Some(Infix::Call), Precedence::Call),
  // LEFT_PAREN
  Rule::new(None, Precedence::None),
  // RIGHT_PAREN
  Rule::new(None, Precedence::None),
  // LEFT_BRACE
  Rule::new(None, Precedence::None),
  // RIGHT_BRACE
  Rule::new(Some(Infix::Index), Precedence::Call),
  // LEFT_BRACKET
  Rule::new(None, Precedence::None),
  // RIGHT_BRACKET
  Rule::new(None, Precedence::None),
  // COMMA
  Rule::new(Some(Infix::Dot), Precedence::Call),
  // DOT
  Rule::new(Some(Infix::Binary), Precedence::Term),
  // MINUS
  Rule::new(Some(Infix::Binary), Precedence::Term),
  // PLUS
  Rule::new(Some(Infix::Ternary), Precedence::Ternary),
  // QUESTION_MARK
  Rule::new(None, Precedence::None),
  // COLON
  Rule::new(None, Precedence::None),
  // SEMICOLON
  Rule::new(None, Precedence::None),
  // PIPE
  Rule::new(Some(Infix::Binary), Precedence::Factor),
  // SLASH
  Rule::new(Some(Infix::Binary), Precedence::Factor),
  // STAR
  Rule::new(None, Precedence::None),
  // PLUS_EQUAL
  Rule::new(None, Precedence::None),
  // MINUS_EQUAL
  Rule::new(None, Precedence::None),
  // SLASH_EQUAL
  Rule::new(None, Precedence::None),
  // STAR_EQUAL
  Rule::new(None, Precedence::None),
  // RIGHT_ARROW
  Rule::new(None, Precedence::None),
  // LEFT_ARROW
  Rule::new(None, Precedence::None),
  // EXPORT
  Rule::new(None, Precedence::None),
  // IMPORT
  Rule::new(None, Precedence::None),
  // FROM
  Rule::new(None, Precedence::None),
  // AMP
  Rule::new(None, Precedence::None),
  // BANG
  Rule::new(Some(Infix::Binary), Precedence::Equality),
  // BANG_EQUAL
  Rule::new(None, Precedence::None),
  // EQUAL
  Rule::new(Some(Infix::Binary), Precedence::Equality),
  // EQUAL_EQUAL
  Rule::new(Some(Infix::Binary), Precedence::Comparison),
  // GREATER
  Rule::new(Some(Infix::Binary), Precedence::Comparison),
  // GREATER_EQUAL
  Rule::new(Some(Infix::Binary), Precedence::Comparison),
  // LESS
  Rule::new(Some(Infix::Binary), Precedence::Comparison),
  // LESS_EQUAL
  Rule::new(None, Precedence::None),
  // IDENTIFIER
  Rule::new(None, Precedence::None),
  // INSTANCE_ACCESS
  Rule::new(None, Precedence::None),
  // STRING
  Rule::new(None, Precedence::None),
  // STRING_BEGIN
  Rule::new(None, Precedence::None),
  // STRING_SEGMENT
  Rule::new(None, Precedence::None),
  // STRING_END
  Rule::new(None, Precedence::None),
  // NUMBER
  Rule::new(Some(Infix::And), Precedence::And),
  // AND
  Rule::new(None, Precedence::None),
  // CLASS
  Rule::new(None, Precedence::None),
  // ELSE
  Rule::new(None, Precedence::None),
  // FALSE
  Rule::new(None, Precedence::None),
  // FOR
  Rule::new(None, Precedence::None),
  // FUN
  Rule::new(None, Precedence::None),
  // IF
  Rule::new(None, Precedence::None),
  // IN
  Rule::new(None, Precedence::None),
  // NIL
  Rule::new(Some(Infix::Or), Precedence::Or),
  // OR
  Rule::new(None, Precedence::None),
  // RETURN
  Rule::new(None, Precedence::None),
  // BREAK
  Rule::new(None, Precedence::None),
  // CONTINUE
  Rule::new(None, Precedence::None),
  // SUPER
  Rule::new(None, Precedence::None),
  // SELF
  Rule::new(None, Precedence::None),
  // STATIC
  Rule::new(None, Precedence::None),
  // TRUE
  Rule::new(None, Precedence::None),
  // VAR
  Rule::new(None, Precedence::None),
  // WHILE
  Rule::new(None, Precedence::None),
  // TRY
  Rule::new(None, Precedence::None),
  // CATCH
  Rule::new(None, Precedence::None),
  // RAISE
  Rule::new(None, Precedence::None),
  // TRAIT
  Rule::new(None, Precedence::None),
  // TYPE
  Rule::new(None, Precedence::None),
  // CHANNEL
  Rule::new(None, Precedence::None),
  // LAUNCH
  Rule::new(None, Precedence::None),
  // ERROR
  Rule::new(None, Precedence::None),
  // EOF
];

/// The rules for infix and prefix operators
const TYPE_PREFIX_TABLE: [Rule<TypePrefix, TypePrecedence>; TokenKind::VARIANT_COUNT] = [
  Rule::new(Some(TypePrefix::Fun), TypePrecedence::Primary),
  // LEFT_PAREN
  Rule::new(None, TypePrecedence::None),
  // RIGHT_PAREN
  Rule::new(None, TypePrecedence::None),
  // LEFT_BRACE
  Rule::new(None, TypePrecedence::None),
  // RIGHT_BRACE
  Rule::new(None, TypePrecedence::None),
  // LEFT_BRACKET
  Rule::new(None, TypePrecedence::None),
  // RIGHT_BRACKET
  Rule::new(None, TypePrecedence::None),
  // COMMA
  Rule::new(None, TypePrecedence::None),
  // DOT
  Rule::new(None, TypePrecedence::None),
  // MINUS
  Rule::new(None, TypePrecedence::None),
  // PLUS
  Rule::new(None, TypePrecedence::None),
  // COLON
  Rule::new(None, TypePrecedence::None),
  // COLON
  Rule::new(None, TypePrecedence::None),
  // SEMICOLON
  Rule::new(None, TypePrecedence::None),
  // PIPE
  Rule::new(None, TypePrecedence::None),
  // SLASH
  Rule::new(None, TypePrecedence::None),
  // STAR
  Rule::new(None, TypePrecedence::None),
  // PLUS_EQUAL
  Rule::new(None, TypePrecedence::None),
  // MINUS_EQUAL
  Rule::new(None, TypePrecedence::None),
  // SLASH_EQUAL
  Rule::new(None, TypePrecedence::None),
  // STAR_EQUAL
  Rule::new(None, TypePrecedence::None),
  // RIGHT_ARROW
  Rule::new(None, TypePrecedence::None),
  // LEFT_ARROW
  Rule::new(None, TypePrecedence::None),
  // EXPORT
  Rule::new(None, TypePrecedence::None),
  // IMPORT
  Rule::new(None, TypePrecedence::None),
  // FROM
  Rule::new(None, TypePrecedence::None),
  // AMP
  Rule::new(None, TypePrecedence::None),
  // BANG
  Rule::new(None, TypePrecedence::None),
  // BANG_EQUAL
  Rule::new(None, TypePrecedence::None),
  // EQUAL
  Rule::new(None, TypePrecedence::None),
  // EQUAL_EQUAL
  Rule::new(None, TypePrecedence::None),
  // GREATER
  Rule::new(None, TypePrecedence::None),
  // GREATER_EQUAL
  Rule::new(None, TypePrecedence::None),
  // LESS
  Rule::new(None, TypePrecedence::None),
  // LESS_EQUAL
  Rule::new(Some(TypePrefix::Literal), TypePrecedence::None),
  // IDENTIFIER
  Rule::new(None, TypePrecedence::None),
  // INSTANCE_ACCESS
  Rule::new(None, TypePrecedence::None),
  // STRING
  Rule::new(None, TypePrecedence::None),
  // STRING_BEGIN
  Rule::new(None, TypePrecedence::None),
  // STRING_SEGMENT
  Rule::new(None, TypePrecedence::None),
  // STRING_END
  Rule::new(None, TypePrecedence::None),
  // NUMBER
  Rule::new(None, TypePrecedence::None),
  // AND
  Rule::new(None, TypePrecedence::None),
  // CLASS
  Rule::new(None, TypePrecedence::None),
  // ELSE
  Rule::new(None, TypePrecedence::None),
  // FALSE
  Rule::new(None, TypePrecedence::None),
  // FOR
  Rule::new(None, TypePrecedence::None),
  // FUN
  Rule::new(None, TypePrecedence::None),
  // IF
  Rule::new(None, TypePrecedence::None),
  // IN
  Rule::new(Some(TypePrefix::Literal), TypePrecedence::Primary),
  // NIL
  Rule::new(None, TypePrecedence::None),
  // OR
  Rule::new(None, TypePrecedence::None),
  // RETURN
  Rule::new(None, TypePrecedence::None),
  // BREAK
  Rule::new(None, TypePrecedence::None),
  // CONTINUE
  Rule::new(None, TypePrecedence::None),
  // SUPER
  Rule::new(None, TypePrecedence::None),
  // SELF
  Rule::new(None, TypePrecedence::None),
  // STATIC
  Rule::new(None, TypePrecedence::None),
  // TRUE
  Rule::new(None, TypePrecedence::None),
  // VAR
  Rule::new(None, TypePrecedence::None),
  // WHILE
  Rule::new(None, TypePrecedence::None),
  // TRY
  Rule::new(None, TypePrecedence::None),
  // CATCH
  Rule::new(None, TypePrecedence::None),
  // RAISE
  Rule::new(None, TypePrecedence::None),
  // TRAIT
  Rule::new(None, TypePrecedence::None),
  // TYPE
  Rule::new(None, TypePrecedence::None),
  // CHANNEL
  Rule::new(None, TypePrecedence::None),
  // LAUNCH
  Rule::new(None, TypePrecedence::None),
  // ERROR
  Rule::new(None, TypePrecedence::None),
  // EOF
];

/// The rules for infix and prefix operators
const TYPE_INFIX_TABLE: [Rule<TypeInfix, TypePrecedence>; TokenKind::VARIANT_COUNT] = [
  Rule::new(None, TypePrecedence::None),
  // LEFT_PAREN
  Rule::new(None, TypePrecedence::None),
  // RIGHT_PAREN
  Rule::new(None, TypePrecedence::None),
  // LEFT_BRACE
  Rule::new(None, TypePrecedence::None),
  // RIGHT_BRACE
  Rule::new(Some(TypeInfix::List), TypePrecedence::List),
  // LEFT_BRACKET
  Rule::new(None, TypePrecedence::None),
  // RIGHT_BRACKET
  Rule::new(None, TypePrecedence::None),
  // COMMA
  Rule::new(None, TypePrecedence::None),
  // DOT
  Rule::new(None, TypePrecedence::None),
  // MINUS
  Rule::new(None, TypePrecedence::None),
  // COLON
  Rule::new(None, TypePrecedence::None),
  // PLUS
  Rule::new(None, TypePrecedence::None),
  // COLON
  Rule::new(None, TypePrecedence::None),
  // SEMICOLON
  Rule::new(Some(TypeInfix::Union), TypePrecedence::Intersection),
  // PIPE
  Rule::new(None, TypePrecedence::None),
  // SLASH
  Rule::new(None, TypePrecedence::None),
  // STAR
  Rule::new(None, TypePrecedence::None),
  // PLUS_EQUAL
  Rule::new(None, TypePrecedence::None),
  // MINUS_EQUAL
  Rule::new(None, TypePrecedence::None),
  // SLASH_EQUAL
  Rule::new(None, TypePrecedence::None),
  // STAR_EQUAL
  Rule::new(None, TypePrecedence::None),
  // RIGHT_ARROW
  Rule::new(None, TypePrecedence::None),
  // LEFT_ARROW
  Rule::new(None, TypePrecedence::None),
  // EXPORT
  Rule::new(None, TypePrecedence::None),
  // IMPORT
  Rule::new(None, TypePrecedence::None),
  // FROM
  Rule::new(Some(TypeInfix::Intersection), TypePrecedence::Intersection),
  // AMP
  Rule::new(None, TypePrecedence::None),
  // BANG
  Rule::new(None, TypePrecedence::None),
  // BANG_EQUAL
  Rule::new(None, TypePrecedence::None),
  // EQUAL
  Rule::new(None, TypePrecedence::None),
  // EQUAL_EQUAL
  Rule::new(None, TypePrecedence::None),
  // GREATER
  Rule::new(None, TypePrecedence::None),
  // GREATER_EQUAL
  Rule::new(Some(TypeInfix::TypeArgs), TypePrecedence::TypeArgs),
  // LESS
  Rule::new(None, TypePrecedence::None),
  // LESS_EQUAL
  Rule::new(None, TypePrecedence::None),
  // IDENTIFIER
  Rule::new(None, TypePrecedence::None),
  // INSTANCE_ACCESS
  Rule::new(None, TypePrecedence::None),
  // STRING
  Rule::new(None, TypePrecedence::None),
  // STRING_BEGIN
  Rule::new(None, TypePrecedence::None),
  // STRING_SEGMENT
  Rule::new(None, TypePrecedence::None),
  // STRING_END
  Rule::new(None, TypePrecedence::None),
  // NUMBER
  Rule::new(None, TypePrecedence::None),
  // AND
  Rule::new(None, TypePrecedence::None),
  // CLASS
  Rule::new(None, TypePrecedence::None),
  // ELSE
  Rule::new(None, TypePrecedence::None),
  // FALSE
  Rule::new(None, TypePrecedence::None),
  // FOR
  Rule::new(None, TypePrecedence::None),
  // FUN
  Rule::new(None, TypePrecedence::None),
  // IF
  Rule::new(None, TypePrecedence::None),
  // IN
  Rule::new(None, TypePrecedence::None),
  // NIL
  Rule::new(None, TypePrecedence::None),
  // OR
  Rule::new(None, TypePrecedence::None),
  // RETURN
  Rule::new(None, TypePrecedence::None),
  // BREAK
  Rule::new(None, TypePrecedence::None),
  // CONTINUE
  Rule::new(None, TypePrecedence::None),
  // SUPER
  Rule::new(None, TypePrecedence::None),
  // SELF
  Rule::new(None, TypePrecedence::None),
  // STATIC
  Rule::new(None, TypePrecedence::None),
  // TRUE
  Rule::new(None, TypePrecedence::None),
  // VAR
  Rule::new(None, TypePrecedence::None),
  // WHILE
  Rule::new(None, TypePrecedence::None),
  // TRY
  Rule::new(None, TypePrecedence::None),
  // CATCH
  Rule::new(None, TypePrecedence::None),
  // RAISE
  Rule::new(None, TypePrecedence::None),
  // TRAIT
  Rule::new(None, TypePrecedence::None),
  // TYPE
  Rule::new(None, TypePrecedence::None),
  // CHANNEL
  Rule::new(None, TypePrecedence::None),
  // LAUNCH
  Rule::new(None, TypePrecedence::None),
  // ERROR
  Rule::new(None, TypePrecedence::None),
  // EOF
];

/// Get a rule from the prefix table
const fn get_prefix(kind: TokenKind) -> &'static Rule<Prefix, Precedence> {
  &PREFIX_TABLE[kind as usize]
}

/// Get a rule from the rules table
const fn get_infix(kind: TokenKind) -> &'static Rule<Infix, Precedence> {
  &INFIX_TABLE[kind as usize]
}

/// Get a rule from the prefix table
const fn get_type_prefix(kind: TokenKind) -> &'static Rule<TypePrefix, TypePrecedence> {
  &TYPE_PREFIX_TABLE[kind as usize]
}

/// Get a rule from the rules table
const fn get_type_infix(kind: TokenKind) -> &'static Rule<TypeInfix, TypePrecedence> {
  &TYPE_INFIX_TABLE[kind as usize]
}

#[cfg(test)]
mod test {
  use laythe_core::{Allocator, NO_GC};

use crate::source::VM_FILE_TEST_ID;

  use super::super::ir::{AstPrint, Visitor};
  use super::*;

  fn test(src: &str) {
    let mut gc = Allocator::default();
    let source = gc.manage_str(src, &NO_GC);
    let source = Source::new(source);

    let (ast, _) = Parser::new(&source, VM_FILE_TEST_ID).parse();
    assert!(ast.is_ok(), "{}", src);
    let ast = ast.unwrap();

    let mut printer = AstPrint::default();
    printer.visit(&ast);

    let reproduced = gc.manage_str(printer.str(), &NO_GC);
    let reproduced = Source::new(reproduced);

    let (ast2, _) = Parser::new(&reproduced, VM_FILE_TEST_ID).parse();
    assert!(
      ast2.is_ok(),
      "expected:\n{}, \ngenerated: \n{}",
      &*source,
      printer.str()
    );
  }

  #[test]
  fn fib() {
    let example = "
    class Toggle {
      init(startState) {
        self.state = startState;
      }

      value() { self.state }

      activate() {
        self.state = !self.state;
        return self;
      }
    }

    class NthToggle : Toggle {
      init(startState, maxCounter) {
        super.init(startState);
        self.countMax = maxCounter;
        self.count = 0;
      }

      activate() {
        self.count = self.count + 1;
        if self.count >= self.countMax {
          super.activate();
          self.count = 0;
        }

        return self;
      }
    }

    let start = clock();
    let n = 500;
    let val = true;
    let toggle = Toggle(val);

    for i in range(n) {
      val = toggle.activate().value();
      val = toggle.activate().value();
      val = toggle.activate().value();
      val = toggle.activate().value();
      val = toggle.activate().value();
      val = toggle.activate().value();
      val = toggle.activate().value();
      val = toggle.activate().value();
      val = toggle.activate().value();
      val = toggle.activate().value();
    }

    toggle.value();

    val = true;
    let ntoggle = NthToggle(val, 3);

    for i in range(n) {
      val = ntoggle.activate().value();
      val = ntoggle.activate().value();
      val = ntoggle.activate().value();
      val = ntoggle.activate().value();
      val = ntoggle.activate().value();
      val = ntoggle.activate().value();
      val = ntoggle.activate().value();
      val = ntoggle.activate().value();
      val = ntoggle.activate().value();
      val = ntoggle.activate().value();
    }

    ntoggle.value();

    ";

    test(example);
  }

  #[test]
  fn import() {
    let example = r#"
      import std.time;
    "#;

    test(example);
  }

  #[test]
  fn import_self() {
    let example = r#"
      import self.stuff;
    "#;

    test(example);
  }

  #[test]
  fn export_variable() {
    let example = "
      export let x = 10;
    ";

    test(example);
  }

  #[test]
  fn export_variable_type() {
    for type_ in EXAMPLE_TYPES.iter() {
      let example = format!("export let x: {} = 10;", type_);
      test(&example);
    }
  }

  #[test]
  fn export_fun() {
    let example = "
      export fn example() {}
    ";

    test(example);
  }

  #[test]
  fn export_fun_typed() {
    let examples = [
      "export fn example(a: number | any) -> string {}",
      "export fn example(a) {}",
      "export fn example<T: U>(a: U) -> T {}",
    ];

    for example in &examples {
      test(example);
    }
  }

  #[test]
  fn export_class() {
    let example = "
      export class example {}
    ";

    test(example);
  }

  #[test]
  fn export_class_typed() {
    let examples = [
      "export class example {
        field1: string;
        field2: Something<any | number>;
      }",
      "export class Dude<T, V: Car> {
        field1: V;

        someMethod<K>(a: T, b: string & Bro<T>) -> K {

        }
      }",
      "export class Dawg<T> : Bro<T> {
        field1: V;

        someMethod<K>(a: T, b: string & Bro<T>) -> K {

        }
      }",
    ];

    for example in examples.iter() {
      test(example);
    }
  }

  #[test]
  fn class_with_inherit() {
    let example = "
      class A {}

      class B : A {}
    ";

    test(example);
  }

  #[test]
  fn class_empty() {
    let example = "
      class A {}
    ";

    test(example);
  }

  #[test]
  fn class_with_methods() {
    let example = "
      class A {
        init() {
          self.field = true;
        }

        getField() {
          return self.field;
        }

        getGetField() {
          return self.getField();
        }
      }
    ";

    test(example);
  }

  #[test]
  fn class_with_methods_implicit() {
    let example = "
      class A {
        init() { self.field = true; }
        getField() { self.field }
        getGetField() { self.getField() }
      }
    ";

    test(example);
  }

  #[test]
  fn class_with_instance_access() {
    let example = "
      class A {
        init() { @field = true; }
        getField() { @field }
        getGetField() { self.getField() }
      }
    ";

    test(example);
  }

  #[test]
  fn class_with_instance_access_fun_prop() {
    let example = "
      class A {
        init() { @field = |x| x; }
        getField() { @field(10) }
        getGetField() { self.getField() }
      }
    ";

    test(example);
  }

  #[test]
  fn class_with_static_methods() {
    let example = "
      class A {
        static sayHi() {
          return 'hi';
        }

        static sayBye() {
          return 'bye';
        }
      }
    ";

    test(example);
  }

  #[test]
  fn class_with_static_methods_typed() {
    let example = "
      class A {
        static sayHi() -> string {
          return 'hi';
        }

        static sayBye() -> string {
          return 'bye';
        }
      }
    ";

    test(example);
  }

  #[test]
  fn launch() {
    let example = "
      launch something(1, 2, 'cat');
    ";

    test(example);
  }

  #[test]
  fn raise() {
    let example = "
      raise Error('error');
    ";

    test(example);
  }

  #[test]
  fn trait_empty() {
    let example = "
      trait A {
      }
    ";

    test(example);
  }

  #[test]
  fn trait_fields() {
    let example = "
      trait A {
        a: bool;
        b: (some: bool) -> number;
      }
    ";

    test(example);
  }

  #[test]
  fn type_basic() {
    let example = "
      type A = bool;
    ";

    test(example);
  }

  #[test]
  fn empty_try_catch() {
    let example = "
      try {

      } catch e {

      }
    ";

    test(example);
  }

  #[test]
  fn filled_try_catch() {
    let example = r#"
      try {
        let empty = {};
        empty["missing"];
      } catch e: Error {
        print("no!");
      }
    "#;

    test(example);
  }

  #[test]
  fn nested_try_catch() {
    let example = r#"
      try {
        [][3];
        try {
          [][1];
        } catch e: Error {
          print("woops!");
        }
      } catch e: Error {
        print("no!");
      }
    "#;

    test(example);
  }

  #[test]
  fn open_capture() {
    let example = "
    fn example() {
      let x = 0;
      fn middle() {
        fn inner() {
          return x;
        }

        return inner();
      }

      return middle();
    }
    example();
    ";

    test(example);
  }

  #[test]
  fn close_capture() {
    let example = "
    fn example() {
      let a = 10;
      fn inner() {
        return a;
      }
      return inner;
    }
    let inner = example();
    inner();
    ";

    test(example);
  }

  #[test]
  fn empty_fun() {
    let example = "fn example() {} example();";

    test(example);
  }

  #[test]
  fn param_fun() {
    let example = "
    fn example(a) {
      return a;
    }
    let a = 1;
    example(a);
    ";

    test(example)
  }

  #[test]
  fn empty_fun_basic() {
    let example = "fn example() { let a = 10; return a; } example();";

    test(example);
  }

  #[test]
  fn fun_basic_implicit() {
    let example = "
    fn example(a) { a }
    let a = 1;
    example(a);
    ";

    test(example)
  }

  #[test]
  fn for_() {
    let example = "for x in [1, 2, 3] { print(x); }";

    test(example);
  }

  #[test]
  fn while_() {
    let example = "while true { print(10); }";

    test(example);
  }

  #[test]
  fn break_() {
    let example = "for x in [1, 2, 3] { break; }";

    test(example);
  }

  #[test]
  fn continue_() {
    let example = "while true { continue; }";

    test(example);
  }

  #[test]
  fn if_() {
    let example = "if 3 < 10 { print(\"hi\"); }";

    test(example);
  }

  #[test]
  fn if_else() {
    let example = "if 3 < 10 { print(\"hi\"); } else { print(\"bye\"); }";

    test(example);
  }

  #[test]
  fn return_empty() {
    let example = "fn example() { return; }";

    test(example);
  }

  #[test]
  fn return_value() {
    let example = "fn example() { return false; }";

    test(example);
  }

  #[test]
  fn list_index_set() {
    let example = "
      let a = [clock, clock, clock];
      a[1] = 5;
    ";

    test(example);
  }

  #[test]
  fn list_index_get() {
    let example = "
      let a = [\"john\", \"joe\", \"jim\"];
      print(a[1]);
    ";

    test(example);
  }

  #[test]
  fn channel() {
    let example = "
      chan();
    ";

    test(example);
  }

  #[test]
  fn channel_buffered() {
    let example = "
      print(chan(5));
    ";

    test(example);
  }

  #[test]
  fn lambda_expr_body() {
    let example = "
    let example = || 10;
    example();
    ";

    test(example);
  }

  #[test]
  fn lambda_block_body() {
    let example = "
    let example = || { return 10; };
    example();
    ";

    test(example);
  }

  #[test]
  fn map() {
    let example = "let a = { \"cat\": \"bat\", 10: nil };";

    test(example);
  }

  #[test]
  fn tuple_single() {
    let example = "let a = ([1, 2],);";

    test(example);
  }

  #[test]
  fn tuple() {
    let example = "let a = (1, \"cat\", nil, false);";

    test(example);
  }

  #[test]
  fn list() {
    let example = "let a = [1, 2, 3, \"cat\"];";

    test(example);
  }
  const EXAMPLE_TYPES: [&str; 7] = [
    "number | string",
    "MyThing<T, U>",
    "nil",
    "Cat & Dog",
    "(arg: Cat, arg2: number) -> any",
    "any",
    "Dude<(a: number | string) -> Cat>",
  ];

  const EXAMPLE_EXPR: [&str; 5] = [
    "10 < 3 && false",
    "'hellow world'.size()",
    "bill.was.here",
    "(|a| print(a))(10)",
    "b = nil",
  ];
  const EXAMPLE_ATOMS: [&str; 6] = [
    "cool!",
    "is_lame?",
    "item(true).stuff[3]",
    "settable",
    "man.dude.bro",
    "ten(\"false\")[10].bro",
  ];
  const EXAMPLE_PRIMARIES: [&str; 13] = [
    "true",
    "false",
    "nil",
    "10.3",
    "chan(5)",
    "'hi'",
    "('bye')",
    "self",
    "super.man",
    "|| print()",
    "((false), 'dog', false)",
    "[false, true, nil]",
    "{nil: 10, 4.3: false, \"cat\": 'hat'}",
  ];
  const EXAMPLE_TRAILERS: [&str; 3] = ["[2]", "(true, 10)", ".someProp"];
  const BINARY_OPS: [&str; 10] = [" !=", "==", ">", ">=", "<", "<=", "+", "-", "*", "/"];
  const ASSIGNMENTS: [&str; 6] = ["=", "<-", "+=", "-=", "/=", "*="];
  const UNARY_OPS: [&str; 3] = ["<-", "!", "-"];

  #[test]
  fn expr_stmt() {
    for expr in EXAMPLE_EXPR
      .iter()
      .chain(EXAMPLE_ATOMS.iter())
      .chain(EXAMPLE_PRIMARIES.iter())
    {
      let example = format!("{};", expr);
      test(&example);
    }
  }

  #[test]
  fn assign() {
    for expr in EXAMPLE_EXPR.iter() {
      for atom in EXAMPLE_ATOMS.iter() {
        for assign in ASSIGNMENTS.iter() {
          let example = format!("{}{}{};", atom, assign, expr);
          test(&example);
        }
      }
    }
  }

  #[test]
  fn ternary() {
    for cond in EXAMPLE_EXPR
      .iter()
      .chain(EXAMPLE_ATOMS.iter())
      .chain(EXAMPLE_PRIMARIES.iter())
    {
      for then in EXAMPLE_EXPR
        .iter()
        .chain(EXAMPLE_ATOMS.iter())
        .chain(EXAMPLE_PRIMARIES.iter())
      {
        for else_ in EXAMPLE_EXPR
          .iter()
          .chain(EXAMPLE_ATOMS.iter())
          .chain(EXAMPLE_PRIMARIES.iter())
        {
          let example = format!("{} ?{}:{};", cond, then, else_);
          test(&example);
        }
      }
    }
  }

  #[test]
  fn binary() {
    for p1 in EXAMPLE_ATOMS.iter().chain(EXAMPLE_PRIMARIES.iter()) {
      for p2 in EXAMPLE_ATOMS.iter().chain(EXAMPLE_PRIMARIES.iter()) {
        for op in BINARY_OPS.iter() {
          let example = format!("{}{}{};", p1, op, p2);
          test(&example);
        }
      }
    }
  }

  #[test]
  fn unary() {
    for p in EXAMPLE_PRIMARIES.iter() {
      for op in UNARY_OPS.iter() {
        let example = format!("{}{};", op, p);
        test(&example);
      }
    }
  }

  #[test]
  fn trailers() {
    for p in EXAMPLE_PRIMARIES.iter() {
      for t in EXAMPLE_TRAILERS.iter() {
        let example = format!("{}{};", p, t);
        test(&example);
      }
    }
  }

  #[test]
  fn primaries() {
    for p in EXAMPLE_PRIMARIES.iter() {
      let example = format!("{};", p);
      test(&example);
    }
  }
}
