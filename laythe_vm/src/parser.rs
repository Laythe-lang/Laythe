use crate::{
  ast::*,
  scanner::Scanner,
  token::{Lexeme, Token, TokenKind},
};
use laythe_core::{constants::INIT, object::FunKind};
use laythe_env::stdio::Stdio;
use std::mem;

type ParseResult<T> = Result<T, ()>;

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

  /// Has the parser encountered an error
  had_error: bool,

  /// Is the parser in panic mode
  panic_mode: bool,

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

  /// The environments standard io access
  stdio: Stdio,
}

impl<'a> Parser<'a> {
  /// Create a new instance of the parser from a source str
  pub fn new(stdio: Stdio, source: &'a str) -> Self {
    Self {
      scanner: Scanner::new(source),
      stdio,
      had_error: false,
      panic_mode: false,
      fun_kind: FunKind::Script,
      block_return: BlockReturn::Cannot,
      scope_depth: 0,
      loop_depth: 0,
      previous: Token {
        lexeme: Lexeme::Slice("error"),
        line: 0,
        kind: TokenKind::Error,
      },
      current: Token {
        lexeme: Lexeme::Slice("error"),
        line: 0,
        kind: TokenKind::Error,
      },
    }
  }

  /// Parse the provide source string into a Laythe AST
  /// Return the top level module struct if successful
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::parser::Parser;
  /// use laythe_env::stdio::Stdio;
  /// use laythe_native::stdio::StdioNative;
  ///
  /// // an expression
  /// let source = "3 / 2 + 10;";
  ///
  /// let stdio = Stdio::new(Box::new(StdioNative::default()));
  /// let parser = Parser::new(stdio, source);
  /// assert_eq!(parser.parse().is_ok(), true);
  /// ```
  pub fn parse(mut self) -> ParseResult<Module<'a>> {
    self.advance()?;

    // early exit if ""
    if let TokenKind::Eof = self.current.kind {
      return Ok(Module::default());
    }

    let mut decls = Vec::new();
    while !self.match_kind(TokenKind::Eof)? {
      decls.push(self.decl()?)
    }

    self.consume(TokenKind::Eof, "Expected end of expression.")?;

    if !self.had_error {
      Ok(Module::new(decls))
    } else {
      Err(())
    }
  }

  /// Parse a Laythe declaration, if an error occurred at a lower level attempt
  /// synchronize to provide more error messages
  fn decl(&mut self) -> ParseResult<Decl<'a>> {
    let decl = match self.current.kind {
      TokenKind::Class => self
        .advance()
        .and_then(|()| self.class())
        .map(|class| Decl::Symbol(Box::new(class))),
      TokenKind::Fun => self
        .advance()
        .and_then(|()| self.fun())
        .map(|fun| Decl::Symbol(Box::new(fun))),
      TokenKind::Let => self
        .advance()
        .and_then(|()| self.let_())
        .map(|let_| Decl::Symbol(Box::new(let_))),
      TokenKind::Trait => self
        .advance()
        .and_then(|()| self.trait_())
        .map(|trait_| Decl::Symbol(Box::new(trait_))),
      TokenKind::Type => self
        .advance()
        .and_then(|()| self.type_decl())
        .map(|type_decl| Decl::Symbol(Box::new(type_decl))),
      TokenKind::Export => self.advance().and_then(|()| self.export_declaration()),
      _ => self.stmt().map(|stmt| Decl::Stmt(Box::new(stmt))),
    };

    decl.or_else(|()| self.synchronize())
  }

  /// Synchronize the parser to a sentinel token
  fn synchronize(&mut self) -> ParseResult<Decl<'a>> {
    self.panic_mode = false;
    let mut tokens: Vec<Token> = vec![];

    while self.current.kind != TokenKind::Eof || self.previous.kind == TokenKind::Semicolon {
      tokens.push(self.previous.clone());

      match self.current.kind {
        TokenKind::Class
        | TokenKind::Fun
        | TokenKind::Let
        | TokenKind::For
        | TokenKind::If
        | TokenKind::While
        | TokenKind::Return => {
          break;
        }
        _ => (),
      }

      self.advance()?;
    }

    Ok(Decl::Error(tokens.into_boxed_slice()))
  }

  /// Parse a statement
  fn stmt(&mut self) -> ParseResult<Stmt<'a>> {
    match self.current.kind {
      TokenKind::Import => self.advance().and_then(|()| self.import()),
      TokenKind::Try => self.advance().and_then(|()| self.try_block()),
      TokenKind::If => self.advance().and_then(|()| self.if_()),
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
    self.consume(TokenKind::Identifier, "Expected class name.")?;
    let name = self.previous.clone();

    let type_params = if self.match_kind(TokenKind::Less)? {
      self.type_params()?
    } else {
      vec![]
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
    self.consume(TokenKind::LeftBrace, "Expected '{' before class body.")?;
    let mut type_members: Vec<TypeMember> = vec![];
    let mut methods: Vec<Fun> = vec![];
    let mut static_methods: Vec<Fun> = vec![];
    let mut init: Option<Fun> = None;
    let start = self.previous.line;

    while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
      // We need to do a lookahead for ':' to determine
      // if we're looking a member or a method
      match self.current.kind {
        // identifer may be method or member
        TokenKind::Identifier => {
          self.advance()?;
          let name = self.previous.clone();

          match self.current.kind {
            TokenKind::Colon => {
              self.advance()?;
              let type_ = self.type_()?;
              self.consume(
                TokenKind::Semicolon,
                "Expected ';' after class member declaration.",
              )?;
              type_members.push(TypeMember::new(name, type_));
            }
            _ => {
              let (fun_kind, method) = self.method(name, false)?;
              match fun_kind {
                FunKind::Method => methods.push(method),
                FunKind::Initializer => init = Some(method),
                _ => unreachable!(),
              }
            }
          }
        }

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
        }
        _ => return self.error_current("Expected method or member declaration inside of class."),
      }
    }

    self.consume(TokenKind::RightBrace, "Expected '}' after class body.")?;
    let end = self.previous.line;

    Ok(Symbol::Class(Class::new(
      Some(name),
      Range { start, end },
      type_params,
      super_class,
      type_members,
      init,
      methods,
      static_methods,
    )))
  }

  /// Parse a function declaration
  fn fun(&mut self) -> ParseResult<Symbol<'a>> {
    let previous = mem::replace(&mut self.fun_kind, FunKind::Fun);

    self.consume(TokenKind::Identifier, "Expected function name.")?;
    let name = self.previous.clone();

    let type_params = if self.match_kind(TokenKind::Less)? {
      self.type_params()
    } else {
      Ok(vec![])
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

    self
      .consume(
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
      Ok(vec![])
    }?;

    self.consume(TokenKind::LeftBrace, "Expected '{' after trait name.")?;

    let mut members: Vec<TypeMember> = vec![];
    let mut methods: Vec<TypeMethod> = vec![];

    while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
      // We need to do a lookahead for ':' to determine
      // if we're looking a member or a method
      self.consume(
        TokenKind::Identifier,
        "Expected method or member declaration",
      )?;
      let name = self.previous.clone();

      match self.current.kind {
        TokenKind::Colon => {
          self.advance()?;
          let type_ = self.type_()?;
          self.consume(
            TokenKind::Semicolon,
            "Expected ';' after class member declaration.",
          )?;
          members.push(TypeMember::new(name, type_));
        }
        TokenKind::Less | TokenKind::LeftParen => {
          self.advance()?;
          let type_params = if self.match_kind(TokenKind::Less)? {
            self.type_params()?
          } else {
            vec![]
          };
          let call_sig = self.call_signature(TokenKind::RightParen, type_params)?;
          self.consume(
            TokenKind::Semicolon,
            "Expected ';' after class member declaration.",
          )?;
          methods.push(TypeMethod::new(name, call_sig));
        }
        _ => self.error_at(
          self.current.clone(),
          "Expected member or method declaration inside trait.",
        )?,
      }
    }

    self.consume(TokenKind::RightBrace, "Expected '}' after trait body.")?;

    let range = Range {
      start: name.line,
      end: self.previous.line,
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
      Ok(vec![])
    }?;

    self.consume(TokenKind::Equal, "Expected '=' after type name.")?;

    let type_ = self.type_()?;
    self.consume(TokenKind::Semicolon, "Expected ';' after type declaration.")?;

    Ok(Symbol::TypeDecl(TypeDecl::new(name, params, type_)))
  }

  /// Parse a symbol export declaration
  fn export_declaration(&mut self) -> ParseResult<Decl<'a>> {
    let symbol = match self.current.kind {
      TokenKind::Class => self.advance().and_then(|()| self.class()),
      TokenKind::Fun => self.advance().and_then(|()| self.fun()),
      TokenKind::Let => self.advance().and_then(|()| self.let_()),
      TokenKind::Trait => self.advance().and_then(|()| self.trait_()),
      TokenKind::Type => self.advance().and_then(|()| self.type_decl()),
      _ => self.error_current("Can only export variable, function or class declarations."),
    }?;

    Ok(Decl::Export(Box::new(symbol)))
  }

  /// Parse an import statement
  fn import(&mut self) -> ParseResult<Stmt<'a>> {
    if self.scope_depth > 0 {
      return self.error_current("Can only import from the module scope.");
    }

    self.consume(TokenKind::Identifier, "Expected name following import.")?;
    let imported = self.previous.clone();

    let path = self
      .consume(TokenKind::From, "Expected 'from' following import name.")
      .and_then(|()| self.consume(TokenKind::String, "Expected path string after 'from'."))
      .map(|()| self.previous.clone())?;

    self
      .consume(TokenKind::Semicolon, "Expected ';' after value.")
      .map(|()| Stmt::Import(Box::new(Import::new(imported, path))))
  }

  /// Parse a try catch block
  fn try_block(&mut self) -> ParseResult<Stmt<'a>> {
    let block = self
      .consume(TokenKind::LeftBrace, "Expected '{' after try.")
      .and_then(|()| self.block(BlockReturn::Cannot))?;

    self
      .consume(TokenKind::Catch, "Expected 'catch' after try block.")
      .and_then(|()| self.consume(TokenKind::LeftBrace, "Expected '{' after catch."))
      .and_then(|()| self.block(BlockReturn::Cannot))
      .map(|catch| Stmt::Try(Box::new(Try::new(block, catch))))
  }

  /// Parse a if statement
  fn if_(&mut self) -> ParseResult<Stmt<'a>> {
    // parse the condition
    let cond = self.expr()?;
    self.consume(TokenKind::LeftBrace, "Expected '{' after condition.")?;

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

    Ok(Stmt::If(Box::new(If::new(cond, body, else_body))))
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
    self.loop_(|self_| {
      self_.consume(TokenKind::Identifier, "Expected identifer after 'for'.")?;
      let item = self_.previous.clone();

      self_.consume(TokenKind::In, "Expected 'in' following for loop variable.")?;
      let iter = self_.expr()?;

      self_
        .consume(TokenKind::LeftBrace, "Expected '{' after iterable.")
        .and_then(|()| self_.block(BlockReturn::Cannot))
        .map(|body| Stmt::For(Box::new(For::new(item, iter, body))))
    })
  }

  /// Parse while statement
  fn while_(&mut self) -> ParseResult<Stmt<'a>> {
    self.loop_(|self_| {
      let cond = self_.expr()?;
      self_.consume(TokenKind::LeftBrace, "Expected '{' after while condition.")?;

      let body = self_.block(BlockReturn::Cannot)?;
      Ok(Stmt::While(Box::new(While::new(cond, body))))
    })
  }

  /// Parse a return statement
  fn return_(&mut self) -> ParseResult<Stmt<'a>> {
    if let FunKind::Script = self.fun_kind {
      return self.error("Cannot return from outside of a function or method.");
    }

    let return_ = self.previous.clone();
    if self.match_kind(TokenKind::Semicolon)? {
      Ok(Stmt::Return(Box::new(Return::new(return_, None))))
    } else {
      let expr = self.expr()?;
      let result = self
        .consume(TokenKind::Semicolon, "Expected ';' after return value.")
        .map(|()| Stmt::Return(Box::new(Return::new(return_, Some(expr)))));

      if let FunKind::Initializer = self.fun_kind {
        self.error("Cannot return from outside of a function or method.")
      } else {
        result
      }
    }
  }

  fn continue_(&mut self) -> ParseResult<Stmt<'a>> {
    if self.loop_depth == 0 {
      return self.error("Cannot continue from outside of a for or while loop.");
    }

    let continue_ = self.previous.clone();
    self.consume(TokenKind::Semicolon, "Expected ';' after continue.")?;
    Ok(Stmt::Continue(Box::new(continue_)))
  }

  fn break_(&mut self) -> ParseResult<Stmt<'a>> {
    if self.loop_depth == 0 {
      return self.error("Cannot break from outside of a for or while loop.");
    }

    let break_ = self.previous.clone();
    self.consume(TokenKind::Semicolon, "Expected ';' after break.")?;
    Ok(Stmt::Break(Box::new(break_)))
  }

  /// Parse an expression statement
  fn expr_stmt(&mut self) -> ParseResult<Stmt<'a>> {
    let expr = self.expr()?;
    if self.match_kind(TokenKind::Semicolon)? {
      Ok(Stmt::Expr(Box::new(expr)))
    } else {
      if let BlockReturn::Cannot = self.block_return {
        return self.error("Expected ';' after expression");
      }

      Ok(Stmt::ImplicitReturn(Box::new(expr)))
    }
  }

  /// Parse an expression using a Pratt parser
  fn parse_precedence(&mut self, precedence: Precedence) -> ParseResult<Expr<'a>> {
    self.advance()?;

    let can_assign = precedence <= Precedence::Assignment;
    let prefix_fn = get_prefix(self.previous.kind).op;

    // apply some prefix action
    let mut expr = match prefix_fn {
      Some(prefix) => self.prefix(prefix, can_assign),
      None => return self.error("Expected expression."),
    }?;

    // while we still have binding power keep applying infix operations
    while precedence <= get_infix(self.current.kind).precedence {
      self.advance()?;
      let infix_fn = get_infix(self.previous.kind).op;

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
      Prefix::AssignBlock => self.assign_block(),
      Prefix::List => self.list(),
      Prefix::Map => self.map(),
      Prefix::Lambda => self.lambda(),
      Prefix::Grouping => self.grouping(),
      Prefix::Literal => Ok(self.literal()),
      Prefix::Number => Ok(self.number()),
      Prefix::String => Ok(self.string()),
      Prefix::Interpolation => self.interpolation(),
      Prefix::Super => self.super_(),
      Prefix::Self_ => Ok(self.self_()),
      Prefix::Variable => self.variable(can_assign),
      Prefix::Unary => self.unary(),
    }
  }

  /// Execute an infix action
  fn infix(&mut self, action: Infix, lhs: Expr<'a>, can_assign: bool) -> ParseResult<Expr<'a>> {
    match action {
      Infix::And => self.and(lhs),
      Infix::Binary => self.binary(lhs),
      Infix::Call => self.call(lhs),
      Infix::Index => self.index(lhs, can_assign),
      Infix::Dot => self.dot(lhs, can_assign),
      Infix::Or => self.or(lhs),
    }
  }

  /// Parse an assignment block
  fn assign_block(&mut self) -> ParseResult<Expr<'a>> {
    self.consume(
      TokenKind::LeftBrace,
      "Expected '{' after assignment block ':'",
    )?;
    let block = self.block(BlockReturn::Cannot)?;
    Ok(self.atom(Primary::AssignBlock(block)))
  }

  /// Parse a block statement
  fn block(&mut self, block_return: BlockReturn) -> ParseResult<Block<'a>> {
    let start = self.previous.line;

    self.scope_depth += 1;
    let block_return = mem::replace(&mut self.block_return, block_return);

    let mut decls: Vec<Decl> = vec![];
    while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
      decls.push(self.decl().map_err(|err| {
        self.scope_depth -= 1;
        err
      })?);
    }

    if let BlockReturn::Can = self.block_return {
      if let Some((_, rest)) = decls.split_last() {
        for decl in rest {
          if let Decl::Stmt(stmt) = decl {
            if let Stmt::ImplicitReturn(return_) = &**stmt {
              return self.error_at::<Block<'a>>(
                Token {
                  kind: TokenKind::Error,
                  line: return_.end(),
                  lexeme: Lexeme::Slice(&"sup"),
                },
                "Implicit return can only appear at the end of a block.",
              );
            }
          }
        }
      }
    }

    self.scope_depth -= 1;
    self.block_return = block_return;

    let end = self.current.line;

    self
      .consume(TokenKind::RightBrace, "Expected '}' after block.")
      .map(|()| Block::new(Range { start, end }, decls))
  }

  /// Parse a binary expression
  fn binary(&mut self, lhs: Expr<'a>) -> ParseResult<Expr<'a>> {
    let operator_kind = self.previous.kind;
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

    Ok(Expr::Binary(Box::new(Binary::new(op, lhs, rhs))))
  }

  /// Parse an and expression
  fn and(&mut self, lhs: Expr<'a>) -> ParseResult<Expr<'a>> {
    let rhs = self.parse_precedence(Precedence::And)?;
    Ok(Expr::Binary(Box::new(Binary::new(BinaryOp::And, lhs, rhs))))
  }

  /// Parse an or expression
  fn or(&mut self, lhs: Expr<'a>) -> ParseResult<Expr<'a>> {
    let rhs = self.parse_precedence(Precedence::Or)?;
    Ok(Expr::Binary(Box::new(Binary::new(BinaryOp::Or, lhs, rhs))))
  }

  /// Parse a unary expression
  fn unary(&mut self) -> ParseResult<Expr<'a>> {
    let operator_kind = self.previous.kind;
    let expr = self.parse_precedence(Precedence::Unary)?;

    let op = match operator_kind {
      TokenKind::Minus => UnaryOp::Negate,
      TokenKind::Bang => UnaryOp::Not,
      _ => unimplemented!("Unexpected unary operator"),
    };

    Ok(Expr::Unary(Box::new(Unary::new(op, expr))))
  }

  /// Parse a call invocation
  fn call(&mut self, mut lhs: Expr<'a>) -> ParseResult<Expr<'a>> {
    let start = self.previous.line;
    let args = self.consume_arguments(TokenKind::RightParen, std::u8::MAX as usize)?;
    self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;

    let range = Range {
      start,
      end: self.previous.line,
    };

    if let Expr::Atom(atom) = &mut lhs {
      atom
        .trailers
        .push(Trailer::Call(Box::new(Call::new(range, args))))
    } else {
      return self.error("Expected expression. TODO can you get here?");
    }

    Ok(lhs)
  }

  /// Parse an index on an atom
  fn index(&mut self, mut expr: Expr<'a>, can_assign: bool) -> ParseResult<Expr<'a>> {
    let indexer = self.expr()?;
    self.consume(TokenKind::RightBracket, "Expected ']' after index")?;

    if let Expr::Atom(atom) = &mut expr {
      atom
        .trailers
        .push(Trailer::Index(Box::new(Index::new(indexer))))
    } else {
      return self.error("Expected expression. TODO can you get here?");
    }

    if can_assign {
      expr = self.assign(expr)?;
    }

    Ok(expr)
  }

  /// Parse a property access
  fn dot(&mut self, mut expr: Expr<'a>, can_assign: bool) -> ParseResult<Expr<'a>> {
    self.consume(TokenKind::Identifier, "Expected property name after '.'.")?;

    if let Expr::Atom(atom) = &mut expr {
      atom.trailers.push(Trailer::Access(Box::new(Access::new(
        self.previous.clone(),
      ))))
    } else {
      return self.error("Expected expression. TODO can you get here?");
    }

    if can_assign {
      expr = self.assign(expr)?;
    }

    Ok(expr)
  }

  /// Parse a lambda expression
  fn lambda(&mut self) -> ParseResult<Expr<'a>> {
    // parse function parameters
    let call_sig = self.call_signature(TokenKind::Pipe, vec![])?;

    let previous = mem::replace(&mut self.fun_kind, FunKind::Fun);
    let lambda = self
      .fun_body(BlockReturn::Can)
      .map(|body| self.atom(Primary::Lambda(Box::new(Fun::new(None, call_sig, body)))));

    self.fun_kind = previous;
    lambda
  }

  /// Parse a grouping expression
  fn grouping(&mut self) -> ParseResult<Expr<'a>> {
    let expr = self.expr()?;
    self.consume(TokenKind::RightParen, "Expected ')' after expression")?;

    Ok(self.atom(Primary::Grouping(Box::new(expr))))
  }

  /// Compile a variable statement
  fn variable(&mut self, can_assign: bool) -> ParseResult<Expr<'a>> {
    let mut expr = self.atom(Primary::Ident(self.previous.clone()));

    if can_assign {
      expr = self.assign(expr)?;
    }

    Ok(expr)
  }

  /// Parse a class's self identifier
  fn self_(&mut self) -> Expr<'a> {
    self.atom(Primary::Self_(self.previous.clone()))
  }

  /// Parse a class' super identifer
  fn super_(&mut self) -> ParseResult<Expr<'a>> {
    let super_ = self.previous.clone();
    self.consume(TokenKind::Dot, "Expected '.' after super.")?;
    self.consume(TokenKind::Identifier, "Expected identifier after '.'.")?;

    let access = self.previous.clone();
    Ok(self.atom(Primary::Super(Super::new(super_, access))))
  }

  /// Parse a list literal
  fn list(&mut self) -> ParseResult<Expr<'a>> {
    let start = self.previous.line;
    let items = self.consume_arguments(TokenKind::RightBracket, std::u16::MAX as usize)?;
    self.consume(TokenKind::RightBracket, "Expected ']' after arguments")?;

    let range = Range {
      start,
      end: self.previous.line,
    };
    Ok(self.atom(Primary::List(List::new(range, items))))
  }

  /// Parse a map literal
  fn map(&mut self) -> ParseResult<Expr<'a>> {
    let start = self.previous.line;
    let mut entries: Vec<(Expr, Expr)> = vec![];

    while !self.check(TokenKind::RightBrace) {
      let key = self.expr()?;
      self.consume(TokenKind::Colon, "Expected ':' after map key")?;
      let value = self.expr()?;

      if entries.len() == std::u16::MAX as usize {
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
        self.atom(Primary::Map(Map::new(
          Range {
            start,
            end: self.previous.line,
          },
          entries,
        )))
      })
  }

  /// Parse a number literal
  fn number(&self) -> Expr<'a> {
    self.atom(Primary::Number(self.previous.clone()))
  }

  /// Parse a string literal
  fn string(&self) -> Expr<'a> {
    self.atom(Primary::String(self.previous.clone()))
  }

  /// Parse a string literal
  fn interpolation(&mut self) -> ParseResult<Expr<'a>> {
    let start = self.previous.clone();

    let mut segments: Vec<StringSegments> = vec![];
    loop {
      if segments.len() == std::u16::MAX as usize {
        return self.error(&format!(
          "Cannot have more than {} segments in a string interpolation",
          segments.len()
        ));
      }

      match self.current.kind {
        TokenKind::StringSegment => {
          self.advance()?;
          segments.push(StringSegments::Token(self.previous.clone()))
        }
        TokenKind::StringEnd => {
          break;
        }
        _ => segments.push(StringSegments::Expr(Box::new(self.expr()?))),
      }
    }
    self.consume(TokenKind::StringEnd, "Unterminated interpolated string.")?;
    let end = self.previous.clone();

    Ok(
      self.atom(Primary::Interpolation(Box::new(Interpolation::new(
        start, segments, end,
      )))),
    )
  }

  /// Parse a literal
  fn literal(&self) -> Expr<'a> {
    let previous = self.previous.clone();
    match self.previous.kind {
      TokenKind::True => self.atom(Primary::True(previous)),
      TokenKind::False => self.atom(Primary::False(previous)),
      TokenKind::Nil => self.atom(Primary::Nil(previous)),
      _ => unreachable!(format!("Unexpected token kind {:?}", previous.kind)),
    }
  }

  /// Create an atom from a primary
  fn atom(&self, primary: Primary<'a>) -> Expr<'a> {
    Expr::Atom(Box::new(Atom::new(primary)))
  }

  /// Parse the current functions call signature
  fn call_signature(
    &mut self,
    stop_kind: TokenKind,
    type_params: Vec<TypeParam<'a>>,
  ) -> ParseResult<CallSignature<'a>> {
    let start = type_params
      .first()
      .map_or_else(|| self.previous.line, |first| first.start());
    let mut params = vec![];
    let mut arity: u16 = 0;

    if !self.check(stop_kind) {
      loop {
        arity += 1;
        if arity == std::u8::MAX as u16 {
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

    self.consume(stop_kind, "Expected ')' or '|' after parameter list.")?;
    let return_type = if self.match_kind(TokenKind::Arrow)? {
      Some(self.type_()?)
    } else {
      None
    };

    let range = Range {
      start,
      end: self.previous.line,
    };
    Ok(CallSignature::new(range, type_params, params, return_type))
  }

  fn assign(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
    match self.current.kind {
      TokenKind::Equal => self
        .advance()
        .and_then(|()| self.expr())
        .map(|rhs| Expr::Assign(Box::new(Assign::new(expr, rhs)))),
      TokenKind::PlusEqual => self
        .advance()
        .and_then(|()| self.expr())
        .map(|rhs| Expr::AssignBinary(Box::new(AssignBinary::new(expr, AssignBinaryOp::Add, rhs)))),
      TokenKind::MinusEqual => self
        .advance()
        .and_then(|()| self.expr())
        .map(|rhs| Expr::AssignBinary(Box::new(AssignBinary::new(expr, AssignBinaryOp::Sub, rhs)))),
      TokenKind::StarEqual => self
        .advance()
        .and_then(|()| self.expr())
        .map(|rhs| Expr::AssignBinary(Box::new(AssignBinary::new(expr, AssignBinaryOp::Mul, rhs)))),
      TokenKind::SlashEqual => self
        .advance()
        .and_then(|()| self.expr())
        .map(|rhs| Expr::AssignBinary(Box::new(AssignBinary::new(expr, AssignBinaryOp::Div, rhs)))),
      _ => Ok(expr),
    }
  }

  /// Consume a comma separated set of arguments for a call or list
  fn consume_arguments(&mut self, stop_token: TokenKind, max: usize) -> ParseResult<Vec<Expr<'a>>> {
    let mut args = vec![];

    while !self.check(stop_token) {
      args.push(self.expr()?);

      if args.len() == max {
        return self.error(&format!("Cannot have more than {} arguments", max));
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
      Ok(FunBody::Block(Box::new(self.block(block_return)?)))
    } else {
      // implicitly return expression lambdas
      Ok(FunBody::Expr(Box::new(self.expr()?)))
    }
  }

  /// Parse a function declaration and body
  fn function(
    &mut self,
    name: Token<'a>,
    type_params: Vec<TypeParam<'a>>,
    block_return: BlockReturn,
  ) -> ParseResult<Fun<'a>> {
    if !self.match_kind(TokenKind::LeftParen)? {
      return self.error_current(&format!("Expected '(' after {} name.", self.fun_kind));
    }

    // parse function parameters
    let call_sig = self.call_signature(TokenKind::RightParen, type_params)?;

    if !self.match_kind(TokenKind::LeftBrace)? {
      return self.error_current(&format!("Expected '{{' after {} signature.", self.fun_kind));
    }
    self
      .block(block_return)
      .map(|body| Fun::new(Some(name), call_sig, FunBody::Block(Box::new(body))))
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
      vec![]
    };

    let method = self
      .function(name, type_params, block_return)
      .map(|fun| (fun_kind, fun));
    self.fun_kind = previous;
    method
  }

  /// Parse type parameters
  fn type_params(&mut self) -> ParseResult<Vec<TypeParam<'a>>> {
    let mut type_params: Vec<TypeParam> = vec![];

    while !self.check(TokenKind::Greater) {
      self.consume(
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

    self.consume(TokenKind::Greater, "Expected '>' after type parameters.")?;
    Ok(type_params)
  }

  /// Parse a type
  fn type_(&mut self) -> ParseResult<Type<'a>> {
    self.parse_type_precedence(TypePrecedence::Union)
  }

  /// Use a pratt parser to generate a type
  fn parse_type_precedence(&mut self, precedence: TypePrecedence) -> ParseResult<Type<'a>> {
    self.advance()?;

    let prefix_fn = get_type_prefix(self.previous.kind).op;

    // apply some prefix action
    let mut type_ = match prefix_fn {
      Some(prefix) => self.type_prefix(prefix),
      None => return self.error("Expected type expression."),
    }?;

    // while we still have binding power keep applying infix operations
    while precedence <= get_type_infix(self.current.kind).precedence {
      self.advance()?;
      let infix_fn = get_type_infix(self.previous.kind).op;

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
      TypePrefix::Fun => Ok(Type::Fun(Box::new(
        self.call_signature(TokenKind::RightParen, vec![])?,
      ))),
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
    Ok(Type::Intersection(Box::new(Intersection::new(lhs, rhs))))
  }

  /// Create a union type
  fn union(&mut self, lhs: Type<'a>) -> ParseResult<Type<'a>> {
    let rhs = self.parse_type_precedence(TypePrecedence::Intersection)?;
    Ok(Type::Union(Box::new(Union::new(lhs, rhs))))
  }

  /// Create a list type
  fn list_type(&mut self, lhs: Type<'a>) -> ParseResult<Type<'a>> {
    self.consume(TokenKind::RightBracket, "Expected ']' after list type.")?;
    Ok(Type::List(Box::new(ListType::new(lhs))))
  }

  /// Create a type literal
  fn type_literal(&mut self) -> ParseResult<Type<'a>> {
    let token = self.previous.clone();
    match token.kind {
      TokenKind::Nil => Ok(Type::Primitive(Primitive::Nil(token))),
      TokenKind::Identifier => match self.previous.str() {
        BOOL_TYPE => Ok(Type::Primitive(Primitive::Bool(token))),
        STRING_TYPE => Ok(Type::Primitive(Primitive::String(token))),
        NUMBER_TYPE => Ok(Type::Primitive(Primitive::Number(token))),
        ANY_TYPE => Ok(Type::Primitive(Primitive::Any(token))),

        // if we're not a primitive type then we're a type ref
        _ => Ok(Type::Ref(Box::new(TypeRef::new(token, vec![])))),
      },

      // This should not occur it means the table is messed up
      _ => panic!(
        "This should not be able to get to type_literal with kind {:?}",
        self.previous.kind
      ),
    }
  }

  /// Parse a class type
  fn class_type(&mut self) -> ParseResult<ClassType<'a>> {
    let name = self.previous.clone();

    let type_args = if self.match_kind(TokenKind::Less)? {
      self.type_args()?
    } else {
      vec![]
    };

    Ok(ClassType::new(TypeRef::new(name, type_args)))
  }

  /// Parse type ref
  fn type_ref(&mut self, lhs: Type<'a>) -> ParseResult<Type<'a>> {
    match lhs {
      Type::Ref(mut type_ref) => {
        type_ref.type_args = self.type_args()?;
        Ok(Type::Ref(type_ref))
      }
      _ => {
        // TODO: maybe
        self.error("Can only apply type argument to a non primitive type identifier.")
      }
    }
  }

  fn type_args(&mut self) -> ParseResult<Vec<Type<'a>>> {
    let args = self.consume_type_args(std::u8::MAX as usize)?;
    self.consume(TokenKind::Greater, "Expected '>' after arguments")?;

    Ok(args)
  }

  /// Consume a comma separated set of arguments for calls and lists
  fn consume_type_args(&mut self, max: usize) -> ParseResult<Vec<Type<'a>>> {
    let mut args = vec![];

    while !self.check(TokenKind::Greater) {
      args.push(self.type_()?);

      if args.len() == max {
        return self.error(&format!("Cannot have more than {} type arguments.", max));
      }

      if !self.match_kind(TokenKind::Comma)? {
        break;
      }
    }

    Ok(args)
  }

  /// Does the provided token kind match if so advance the
  /// token index
  fn match_kind(&mut self, kind: TokenKind) -> ParseResult<bool> {
    if !self.check(kind) {
      return Ok(false);
    }
    self.advance()?;
    Ok(true)
  }

  /// Does the provided token kind match the current kind
  fn check(&self, kind: TokenKind) -> bool {
    self.current.kind == kind
  }

  /// Advance the parser a token forward
  fn advance(&mut self) -> ParseResult<()> {
    self.previous = mem::replace(&mut self.current, self.scanner.scan_token());

    if self.current.kind != TokenKind::Error {
      return Ok(());
    }

    let token = self.current.clone();
    self.error_current(token.str())
  }

  /// Consume a token and advance the current token index
  fn consume(&mut self, kind: TokenKind, message: &str) -> ParseResult<()> {
    if self.current.kind == kind {
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
    if self.panic_mode {
      return Err(());
    }

    self.panic_mode = true;
    let stderr = self.stdio.stderr();

    write!(stderr, "[line {}] Error", token.line).expect("Unable to write to stderr.");

    match token.kind {
      TokenKind::Eof => {
        write!(stderr, " at end").expect("Unable to write to stderr.");
      }
      TokenKind::Error => (),
      _ => {
        write!(stderr, " at {}", token.str()).expect("Unable to write to stderr.");
      }
    }

    writeln!(stderr, ": {}", message).expect("Unable to write to stderr.");
    self.had_error = true;
    Err(())
  }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Precedence {
  None,
  Assignment,
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
      Precedence::Assignment => Precedence::Or,
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
  AssignBlock,
  Grouping,
  Lambda,
  List,
  Literal,
  Map,
  Number,
  Self_,
  String,
  Interpolation,
  Super,
  Unary,
  Variable,
}

#[derive(Clone, Copy)]
enum Infix {
  And,
  Binary,
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

const TOKEN_VARIANTS: usize = 63;

/// The rules for infix and prefix operators
const PREFIX_TABLE: [Rule<Prefix, Precedence>; TOKEN_VARIANTS] = [
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
  Rule::new(Some(Prefix::AssignBlock), Precedence::None),
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
  // ARROW
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
  Rule::new(None, Precedence::None),
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
  // TRAIT
  Rule::new(None, Precedence::None),
  // TYPE
  Rule::new(None, Precedence::None),
  // ERROR
  Rule::new(None, Precedence::None),
  // EOF
];

/// The rules for infix and prefix operators
const INFIX_TABLE: [Rule<Infix, Precedence>; TOKEN_VARIANTS] = [
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
  // ARROW
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
  // TRAIT
  Rule::new(None, Precedence::None),
  // TYPE
  Rule::new(None, Precedence::None),
  // ERROR
  Rule::new(None, Precedence::None),
  // EOF
];

/// The rules for infix and prefix operators
const TYPE_PREFIX_TABLE: [Rule<TypePrefix, TypePrecedence>; TOKEN_VARIANTS] = [
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
  // ARROW
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
  // TRAIT
  Rule::new(None, TypePrecedence::None),
  // TYPE
  Rule::new(None, TypePrecedence::None),
  // ERROR
  Rule::new(None, TypePrecedence::None),
  // EOF
];

/// The rules for infix and prefix operators
const TYPE_INFIX_TABLE: [Rule<TypeInfix, TypePrecedence>; TOKEN_VARIANTS] = [
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
  // ARROW
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
  // TRAIT
  Rule::new(None, TypePrecedence::None),
  // TYPE
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
  use super::*;
  use crate::ast_printer::AstPrint;
  use laythe_native::stdio::StdioNative;

  fn test(src: &str) {
    let stdio = Stdio::new(Box::new(StdioNative::default()));
    let ast = Parser::new(stdio, src).parse();
    assert!(ast.is_ok());
    let ast = ast.unwrap();

    let mut printer = AstPrint::default();
    printer.visit(&ast);

    let stdio = Stdio::new(Box::new(StdioNative::default()));
    let ast2 = Parser::new(stdio, printer.str()).parse();
    assert!(
      ast2.is_ok(),
      format!("expected:\n{}, \ngenerated: \n{}", src, printer.str())
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
      import time from "std/time";
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

      } catch {

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
      } catch {
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
        } catch {
          print("woops!");
        }
      } catch {
        print("no!");
      }
    "#;

    test(example);
  }

  #[test]
  fn open_upvalue() {
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
  fn close_upvalue() {
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
  fn block_empty() {
    let example = ":{};";

    test(example);
  }

  #[test]
  fn block_filled() {
    let example = ":{ print(10); };";

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
    "10 < 3 and false",
    "'hellow world'.size()",
    "bill.was.here",
    "(|a| print(a))(10)",
    "b = nil",
  ];
  const EXAMPLE_ATOMS: [&str; 4] = [
    "item(true).stuff[3]",
    "settable",
    "man.dude.bro",
    "ten(\"false\")[10].bro",
  ];
  const EXAMPLE_PRIMARIES: [&str; 12] = [
    "true",
    "false",
    "nil",
    "10.3",
    "'hi'",
    "('bye')",
    "self",
    "super.man",
    "|| print()",
    ":{ let x = 10; }",
    "[false, true, nil]",
    "{nil: 10, 4.3: false, \"cat\": 'hat'}",
  ];
  const EXAMPLE_TRAILERS: [&str; 3] = ["[2]", "(true, 10)", ".someProp"];
  const BINARY_OPS: [&str; 10] = ["!=", "==", ">", ">=", "<", "<=", "+", "-", "*", "/"];
  const ASSIGNMENTS: [&str; 5] = ["=", "+=", "-=", "/=", "*="];
  const UNARY_OPS: [&str; 2] = ["!", "-"];

  #[test]
  fn expr_stmt() {
    for expr in EXAMPLE_EXPR.iter() {
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
  fn binary() {
    for p1 in EXAMPLE_PRIMARIES.iter() {
      for p2 in EXAMPLE_PRIMARIES.iter() {
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
