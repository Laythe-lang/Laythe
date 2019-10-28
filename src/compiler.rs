use crate::scanner::{Scanner, TokenKind, Token};
use crate::chunk::{Chunk, OpCode};
use crate::debug::{disassemble_chunk};

pub struct Compiler<'a> {
  parser: Parser<'a>,
  chunk: &'a mut Chunk,
  rule_table: [ParseRule; 40]
}

impl<'a> Compiler<'a> {
  pub fn new(source: &'a str, chunk: &'a mut Chunk) -> Self {
    Self { 
      chunk,
      parser: Parser::new(source),
      rule_table: [
        ParseRule::new(Some(CompilerAction::Grouping), None, Precedence::None),       // TOKEN_LEFT_PAREN      
        ParseRule::new(None, None, Precedence::None),       // TOKEN_RIGHT_PAREN     
        ParseRule::new(None, None, Precedence::None),       // TOKEN_LEFT_BRACE
        ParseRule::new(None, None, Precedence::None),       // TOKEN_RIGHT_BRACE     
        ParseRule::new(None, None, Precedence::None),       // TOKEN_COMMA           
        ParseRule::new(None, None, Precedence::None),       // TOKEN_DOT             
        ParseRule::new(Some(CompilerAction::Unary), Some(CompilerAction::Binary), Precedence::Term),       // TOKEN_MINUS           
        ParseRule::new(None, Some(CompilerAction::Binary), Precedence::Term),       // TOKEN_PLUS            
        ParseRule::new(None, None, Precedence::None),       // TOKEN_SEMICOLON       
        ParseRule::new(None, Some(CompilerAction::Binary), Precedence::Factor),     // TOKEN_SLASH           
        ParseRule::new(None, Some(CompilerAction::Binary), Precedence::Factor),     // TOKEN_STAR            
        ParseRule::new(None, None, Precedence::None),       // TOKEN_BANG            
        ParseRule::new(None, None, Precedence::None),       // TOKEN_BANG_EQUAL      
        ParseRule::new(None, None, Precedence::None),       // TOKEN_EQUAL           
        ParseRule::new(None, None, Precedence::None),       // TOKEN_EQUAL_EQUAL     
        ParseRule::new(None, None, Precedence::None),       // TOKEN_GREATER         
        ParseRule::new(None, None, Precedence::None),       // TOKEN_GREATER_EQUAL   
        ParseRule::new(None, None, Precedence::None),       // TOKEN_LESS            
        ParseRule::new(None, None, Precedence::None),       // TOKEN_LESS_EQUAL      
        ParseRule::new(None, None, Precedence::None),       // TOKEN_IDENTIFIER      
        ParseRule::new(None, None, Precedence::None),       // TOKEN_STRING          
        ParseRule::new(Some(CompilerAction::Number), None, Precedence::None),       // TOKEN_NUMBER          
        ParseRule::new(None, None, Precedence::None),       // TOKEN_AND             
        ParseRule::new(None, None, Precedence::None),       // TOKEN_CLASS           
        ParseRule::new(None, None, Precedence::None),       // TOKEN_ELSE            
        ParseRule::new(None, None, Precedence::None),       // TOKEN_FALSE           
        ParseRule::new(None, None, Precedence::None),       // TOKEN_FOR             
        ParseRule::new(None, None, Precedence::None),       // TOKEN_FUN             
        ParseRule::new(None, None, Precedence::None),       // TOKEN_IF              
        ParseRule::new(None, None, Precedence::None),       // TOKEN_NIL             
        ParseRule::new(None, None, Precedence::None),       // TOKEN_OR              
        ParseRule::new(None, None, Precedence::None),       // TOKEN_PRINT           
        ParseRule::new(None, None, Precedence::None),       // TOKEN_RETURN          
        ParseRule::new(None, None, Precedence::None),       // TOKEN_SUPER           
        ParseRule::new(None, None, Precedence::None),       // TOKEN_THIS            
        ParseRule::new(None, None, Precedence::None),       // TOKEN_TRUE            
        ParseRule::new(None, None, Precedence::None),       // TOKEN_VAR             
        ParseRule::new(None, None, Precedence::None),       // TOKEN_WHILE           
        ParseRule::new(None, None, Precedence::None),       // TOKEN_ERROR           
        ParseRule::new(None, None, Precedence::None)        // TOKEN_EOF 
      ]
    }
  }

  pub fn compile(&mut self) -> bool {
    self.parser.advance();
    self.expression();

    self.parser.consume(TokenKind::Eof, "Expected end of expression.");
    self.end_compiler();
    !self.parser.had_error
  }

  fn expression(&mut self) {
    self.parse_precendence(Precedence::Assignment)
  }

  fn end_compiler(&mut self) {
    self.emit_return();

    #[cfg(debug_assertions)]
    self.print_chunk()
  }

  fn print_chunk(&self) {
    if self.parser.had_error {
      disassemble_chunk(self.chunk, "code")
    }
  }

  pub fn binary(&mut self) {
    // Remember the operator
    let operator_kind = self.parser.previous.kind.clone();
  
    let precedence = self.get_rule(operator_kind.clone()).precedence.higher();
    self.parse_precendence(precedence);

    match operator_kind {
      TokenKind::Plus => self.emit_byte(OpCode::Add),
      TokenKind::Minus => self.emit_byte(OpCode::Subtract),
      TokenKind::Star => self.emit_byte(OpCode::Multiply),
      TokenKind::Slash => self.emit_byte(OpCode::Divide),
      _ => panic!("Invalid operator")
    }
  }

  pub fn grouping(&mut self) {
    self.expression();

    self.parser.consume(TokenKind::RightParen, "Expected ')' after expression")
  }

  pub fn number(&mut self) {
    let value = self.parser.previous.lexeme.parse::<f64>().expect("Unable to parse float");
    self.emit_constant(value.clone());
  }

  pub fn unary(&mut self) {
    let operator_kind = self.parser.previous.kind.clone();

    // Compile the operand
    self.parse_precendence(Precedence::Unary);

    // Emit the operator instruction
    match operator_kind {
      TokenKind::Minus => self.emit_byte(OpCode::Negate),
      _ => panic!(),
    }
  }

  fn parse_precendence(&mut self, precedence: Precedence) {
    self.parser.advance();

    let prefix_fn = self.get_rule(self.parser.previous.kind.clone()).prefix.clone();
    self.execute_action(prefix_fn.expect("Expected expression."));

    while precedence <= self.get_rule(self.parser.current.kind.clone()).precedence {
      self.parser.advance();
      let infix_fn = self.get_rule(self.parser.previous.kind.clone()).infix.clone();

      self.execute_action(infix_fn.expect("Failure"));
    }
  }

  fn execute_action(&mut self, action: CompilerAction) {
    match action {
      CompilerAction::Binary => self.binary(), 
      CompilerAction::Unary => self.unary(), 
      CompilerAction::Grouping => self.grouping(), 
      CompilerAction::Number => self.number(), 
    }
  }

  fn get_rule(&self, kind: TokenKind) -> &ParseRule {
    return &self.rule_table[kind as usize];
  }

  fn emit_return(&mut self) {
    self.emit_byte(OpCode::Return)
  }

  fn make_constant(&mut self, value: f64) -> u8 {
    let index = self.chunk.add_constant(value);
    if index > std::u8::MAX as usize {
      self.parser.error("Too many constants in one chunk.");
      return 0;
    }

    index as u8
  }

  fn emit_constant(&mut self, value: f64) {
    let index = self.make_constant(value);
    self.emit_byte(OpCode::Constant(index));
  }

  fn emit_byte(&mut self, op_code: OpCode) {
    let line = self.parser.current.line.clone();
    self.chunk.write_instruction(op_code, line);
  }
}

struct Parser<'a> {
  current: Token<'a>,
  previous: Token<'a>,
  had_error: bool,
  panic_mode: bool,
  scanner: Scanner<'a>
}

impl<'a> Parser<'a> {
  pub fn new(source: &'a str) -> Self {
    Self { 
      scanner: Scanner::new(source),
      had_error: false,
      panic_mode: false,
      previous: Token { lexeme: "error", line: 0, kind: TokenKind::Error },
      current: Token { lexeme: "error", line: 0, kind: TokenKind::Error },
    }
  }

  pub fn advance(&mut self) -> () {
    self.previous = self.current.clone();

    loop {
      self.current = self.scanner.scan_token();
      // println!("previous {:?} current {:?}", self.previous, self.current);

      if self.current.kind != TokenKind::Error {
        break;
      }

      self.error_at_current("temp")
    }
  }

  fn consume(&mut self, kind: TokenKind, message: &str) -> () {
    if self.current.kind == kind {
      self.advance();
      return;
    }

    self.error_at_current(message)
  }

  fn error_at_current(&mut self, message: &str) -> () {
    let token = self.current.clone();
    self.error_at(token, message);
  }

  pub fn error(&mut self, message: &str) -> () {
    let token = self.previous.clone();
    self.error_at(token, message);
  }

  fn error_at(&mut self, token: Token, message: &str) -> () {
    if self.panic_mode {
      return;
    }

    self.panic_mode = true;
    eprint!("[line {}] Error", token.line);

    match token.kind {
      TokenKind::Eof => eprint!(" at end"),
      TokenKind::Error => (),
      _ => eprint!(" at {}", token.lexeme)
    }

    eprintln!(": {}", message);
  }
}

// fn print_tokens(scanner: &mut Scanner) {
//   let mut line: i32 = -1;

//   loop {
//     let token = scanner.scan_token();

//     if token.line != line {
//       print!("{:0>4} ", token.line);
//       line = token.line;
//     } else {
//       print!("   | ");
//     }
//     println!("{:?} {}", token.kind, token.lexeme.replace(" ", "<space>").replace("\n", "<newline>"));

//     if token.kind == TokenKind::Eof {
//       break;
//     }
//   }
// }

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
  Primary
}

impl Precedence {
  pub fn higher(&self) -> Precedence {
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

struct ParseRule {
  prefix: Option<CompilerAction>,
  infix: Option<CompilerAction>,
  precedence: Precedence
}

impl ParseRule {
  fn new(
    prefix: Option<CompilerAction>,
    infix: Option<CompilerAction>,
    precedence: Precedence) -> Self {
    Self {
      prefix,
      infix,
      precedence
    }
  }
}

#[derive(Clone)]
enum CompilerAction {
  Grouping,
  Binary,
  Unary,
  Number,
}