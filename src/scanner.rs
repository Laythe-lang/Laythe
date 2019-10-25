// use std::io::{Write, stdout};

/// A token in the space lox language
#[derive(Debug)]
pub struct Token<'a> {
  /// The token kind
  pub kind: TokenKind,

  /// The character array of the source
  pub lexeme: &'a str,

  /// line number this token appears
  pub line: i32,
}

/// Token kinds in the space lox language
#[derive(Debug, PartialEq)]
pub enum TokenKind {
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Comma,
  Dot,
  Minus,
  Plus,
  Semicolon,
  Slash,
  Star,

  Bang,
  BangEqual,
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,

  Identifier,
  String,
  Number,

  And,
  Class,
  Else,
  False,
  For,
  Fun,
  If,
  Nil,
  Or,
  Print,
  Return,
  Super,
  This,
  True,
  Var,
  While,

  Error,
  Eof,
}

/// 
pub struct Scanner<'a> {
  source: &'a str,
  line: i32,

  start: usize,
  current: usize,
  char_start: usize,
}

const STRING_ERROR: &'static str = "Unterminated string";
const UNKNOWN_CHARACTER: &'static str = "Unexpected character";

impl<'a> Scanner<'a> {
  pub fn new(source: &str) -> Scanner {
    Scanner {
      source,

      start: 0,
      current: next_boundary(source, 0),
      char_start: 0,

      line: 0,
    }
  }

  pub fn scan_token(&mut self) -> Token {
    self.skip_white_space();

    self.start = last_boundary(self.source, self.current);
    self.char_start = self.start;

    if self.is_at_end() {
      self.current = self.start;
      return self.make_token(TokenKind::Eof);
    }

    let c = self.advance();
    match c {
      "(" => self.make_token(TokenKind::LeftParen),
      ")" => self.make_token(TokenKind::RightParen),
      "{" => self.make_token(TokenKind::LeftBrace),
      "}" => self.make_token(TokenKind::RightBrace),
      ";" => self.make_token(TokenKind::Semicolon),
      "," => self.make_token(TokenKind::Comma),
      "." => self.make_token(TokenKind::Dot),
      "-" => self.make_token(TokenKind::Minus),
      "+" => self.make_token(TokenKind::Plus),
      "/" => self.make_token(TokenKind::Slash),
      "*" => self.make_token(TokenKind::Slash),
      "=" => {
        if self.match_token("=") {
          self.make_token(TokenKind::EqualEqual)
        } else {
          self.make_token(TokenKind::Equal)
        }
      }
      "<" => {
        if self.match_token("=") {
          self.make_token(TokenKind::LessEqual)
        } else {
          self.make_token(TokenKind::Less)
        }
      }
      ">" => {
        if self.match_token("=") {
          self.make_token(TokenKind::GreaterEqual)
        } else {
          self.make_token(TokenKind::Greater)
        }
      }
      "!" => {
        if self.match_token("=") {
          self.make_token(TokenKind::BangEqual)
        } else {
          self.make_token(TokenKind::Bang)
        }
      }
      "\"" => self.string(),
      _ => {
        if is_digit(c) {
          return self.number();
        }

        if is_alpha(c) {
          return self.identifier();
        }

        self.error_token(UNKNOWN_CHARACTER)
      }
    }
  }

  fn identifier(&mut self) -> Token {
    while !self.is_at_end() && (is_alpha(self.peek()) || is_digit(self.peek())) {
      self.advance();
    }

    self.make_token(self.identifier_type())
  }

  fn identifier_type(&self) -> TokenKind {
    match self.nth_char_from(self.start, 0) {
      Some(c1) => match c1 {
        "a" => self.check_keyword(1, "nd", TokenKind::And),
        "c" => self.check_keyword(1, "lass", TokenKind::Class),
        "e" => self.check_keyword(1, "lse", TokenKind::Else),
        "f" => match self.nth_char_from(self.start, 1) {
          Some(c2) => match c2 {
            "a" => self.check_keyword(2, "lse", TokenKind::False),
            "o" => self.check_keyword(2, "r", TokenKind::For),
            "u" => self.check_keyword(2, "n", TokenKind::Fun),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier
        },
        "i" => self.check_keyword(1, "f", TokenKind::If),
        "n" => self.check_keyword(1, "il", TokenKind::Nil),
        "o" => self.check_keyword(1, "r", TokenKind::Or),
        "p" => self.check_keyword(1, "rint", TokenKind::Print),
        "r" => self.check_keyword(1, "eturn", TokenKind::Return),
        "s" => self.check_keyword(1, "uper", TokenKind::Super),
        "t" => match self.nth_char_from(self.start, 1) {
          Some(c2) => match c2 {
            "h" => self.check_keyword(2, "is", TokenKind::This),
            "r" => self.check_keyword(2, "ue", TokenKind::True),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier
        }
        "v" => self.check_keyword(1, "ar", TokenKind::Var),
        "w" => self.check_keyword(1, "hile", TokenKind::While),
        _ => TokenKind::Identifier,
      }
      None => panic!("")
    }
  }

  fn check_keyword(&self, start: usize, rest: &str, kind: TokenKind) -> TokenKind {
    let start_index = self.nth_next_boundary(self.start, start);
    if rest == &self.source[start_index..self.current] {
      return kind;
    }

    TokenKind::Identifier
  }

  fn number(&mut self) -> Token {
    while is_digit(self.peek()) {
      self.advance();
    }

    if self.peek() == "." {
      if let Some(next) = self.peek_next() {
        if is_digit(next) {
          self.advance();
          while is_digit(self.peek()) {
            self.advance();
          }
        }
      }
    }

    self.make_token(TokenKind::Number)
  }

  fn string(&mut self) -> Token {
    while self.peek() != "\"" && !self.is_at_end() {
      if self.peek() == "\n" {
        self.line += 1;
      }
      self.advance();
    }

    if self.is_at_end() {
      return self.error_token(&STRING_ERROR);
    }

    self.advance();
    self.make_token(TokenKind::String)
  }

  fn skip_white_space(&mut self) {
    while !self.is_at_end() {
      let c = self.peek();
      match c {
        " " | "\r" | "\t" => {
          self.advance();
        }
        "\n" => {
          self.line += 1;
          self.advance();
        }
        "/" => match self.peek_next() {
          Some(next) => {
            if next == "/" {
              while self.peek() != "\n" && self.is_at_end() {
                self.advance();
              }
            } else {
              return;
            }
          }
          None => {
            return;
          }
        },
        _ => {
          return;
        }
      }
    }
  }

  fn make_token(&self, kind: TokenKind) -> Token {
    Token {
      kind,
      lexeme: self.current_slice(),
      line: self.line,
    }
  }

  fn error_token(&self, message: &'static str) -> Token {
    Token {
      kind: TokenKind::Error,
      lexeme: message,
      line: self.line,
    }
  }

  fn peek_next(&self) -> Option<&str> {
    let start = self.current;
    let end = next_boundary(self.source, self.current);

    if self.char_index_at_end(end) {
      return None;
    }

    Some(&self.source[start..end])
  }

  fn peek(&self) -> &str {
    &self.source[self.char_start..self.current]
  }

  fn nth_char_from(&self, start: usize, n: u8) -> Option<&str> {
    let mut current_index = next_boundary(self.source, start);
    let mut start_index = start;

    for _ in 0..n {
      start_index = current_index;
      current_index = next_boundary(self.source, current_index);
    }

    match self.char_index_at_end(current_index) {
      true => None,
      false => Some(&self.source[start_index..current_index])
    }
  }

  fn current_slice(&self) -> &str {
    &self.source[self.start..self.current]
  }

  fn advance(&mut self) -> &str {
    let current = &self.source[self.char_start..self.current];
    self.advance_indices();
    current
  }

  fn advance_indices(&mut self) {
    self.char_start = self.current;
    self.current = next_boundary(self.source, self.current);
  }

  fn nth_next_boundary(&self, start: usize, n: usize) -> usize {
    let mut current = start;
    for _ in 0..n {
      current = next_boundary(self.source, current);
    }

    current
  }


  fn match_token(&mut self, expected: &str) -> bool {
    if self.is_at_end() {
      return false;
    }

    if self.peek() != expected {
      return false;
    }

    self.advance_indices();
    true
  }

  fn is_at_end(&self) -> bool {
    self.current >= self.source.len()
  }

  fn char_index_at_end(&self, index: usize) -> bool {
    index >= self.source.len()
  }
}

fn last_boundary(source: &str, start: usize) -> usize {
  let mut current = start - 1;
  while !source.is_char_boundary(current) && current < source.len() {
    current -= 1;
  }

  current
}


fn next_boundary(source: &str, start: usize) -> usize {
  let mut current = start + 1;
  while !source.is_char_boundary(current) && current < source.len() {
    current += 1;
  }

  current
}

fn is_digit(c: &str) -> bool {
  c >= "0" && c <= "9"
}

fn is_alpha(c: &str) -> bool {
  (c >= "a" && c <= "z") || (c >= "A" && c < "Z") || c == "_"
}
