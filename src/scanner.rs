/// A token in the space lox language
#[derive(Debug)]
pub struct Token<'a> {
  /// The token kind
  pub kind: TokenKind,

  /// The character array of the source
  pub source: &'a [char],

  /// the start index of the token
  pub start: usize,

  /// one past the end position
  pub end: usize,

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
pub struct Scanner {
  source: Vec<char>,
  start: usize,
  current: usize,
  line: i32,
}

const STRING_ERROR: [char; 19] = [
  'U', 'n', 't', 'e', 'r', 'm', 'i', 'n', 'a', 't', 'e', 'd', ' ', 's', 't', 'r', 'i', 'n', 'g',
];

impl Scanner {
  pub fn new(source: &str) -> Scanner {
    Scanner {
      source: source.chars().collect(),
      start: 0,
      current: 0,
      line: 0,
    }
  }

  pub fn scan_token(&mut self) -> Token {
    self.skip_white_space();

    if self.is_at_end() {
      return self.make_token(TokenKind::Eof);
    }

    let c = self.advance();
    match c {
      '(' => self.make_token(TokenKind::LeftParen),
      ')' => self.make_token(TokenKind::RightParen),
      '{' => self.make_token(TokenKind::LeftBrace),
      '}' => self.make_token(TokenKind::RightBrace),
      ';' => self.make_token(TokenKind::Semicolon),
      ',' => self.make_token(TokenKind::Comma),
      '.' => self.make_token(TokenKind::Dot),
      '-' => self.make_token(TokenKind::Minus),
      '+' => self.make_token(TokenKind::Plus),
      '/' => self.make_token(TokenKind::Slash),
      '*' => self.make_token(TokenKind::Slash),
      '=' => {
        if self.match_token('=') {
          self.make_token(TokenKind::EqualEqual)
        } else {
          self.make_token(TokenKind::Equal)
        }
      }
      '<' => {
        if self.match_token('=') {
          self.make_token(TokenKind::LessEqual)
        } else {
          self.make_token(TokenKind::Less)
        }
      }
      '>' => {
        if self.match_token('=') {
          self.make_token(TokenKind::GreaterEqual)
        } else {
          self.make_token(TokenKind::Greater)
        }
      }
      '!' => {
        if self.match_token('=') {
          self.make_token(TokenKind::BangEqual)
        } else {
          self.make_token(TokenKind::Bang)
        }
      }
      '"' => self.string(),
      _ => {
        if is_digit(c) {
          return self.number();
        }

        if is_alpha(c) {
          return self.identifier();
        }

        self.error_token([; 0])
      }
    }
  }

  fn identifier(&mut self) -> Token {
    while !self.is_at_end() && (is_alpha(self.peek()) || is_digit(self.peek())) {
      self.advance();
    }

    return self.make_token(self.identifier_type());
  }

  fn identifier_type(&self) -> TokenKind {
    match self.source[self.start] {
      'a' => self.check_keyword(1, vec!['n', 'd'], TokenKind::And),
      'c' => self.check_keyword(1, vec!['l', 'a', 's', 's'], TokenKind::Class),
      'e' => self.check_keyword(1, vec!['l', 's', 'e'], TokenKind::Else),
      'f' => {
        if self.current - self.start > 1 {
          return match self.source[self.start + 1] {
            'a' => self.check_keyword(2, vec!['l', 's', 'e'], TokenKind::False),
            'o' => self.check_keyword(2, vec!['r'], TokenKind::For),
            'u' => self.check_keyword(2, vec!['n'], TokenKind::Fun),
            _ => TokenKind::Identifier,
          };
        }
        return TokenKind::Identifier;
      }
      'i' => self.check_keyword(1, vec!['f'], TokenKind::If),
      'n' => self.check_keyword(1, vec!['i', 'l'], TokenKind::Nil),
      'o' => self.check_keyword(1, vec!['r'], TokenKind::Or),
      'p' => self.check_keyword(1, vec!['p', 'r', 'i', 'n', 't'], TokenKind::Print),
      'r' => self.check_keyword(1, vec!['e', 't', 'u', 'r', 'n'], TokenKind::Return),
      's' => self.check_keyword(1, vec!['u', 'p', 'e', 'r'], TokenKind::Super),
      't' => {
        if self.current - self.start > 1 {
          return match self.source[self.start + 1] {
            'h' => self.check_keyword(2, vec!['i', 's'], TokenKind::This),
            'r' => self.check_keyword(2, vec!['u', 'e'], TokenKind::True),
            _ => TokenKind::Identifier,
          };
        }
        return TokenKind::Identifier;
      }
      'v' => self.check_keyword(1, vec!['a', 'r'], TokenKind::Var),
      'w' => self.check_keyword(1, vec!['h', 'i', 'l', 'e'], TokenKind::While),
      _ => TokenKind::Identifier,
    }
  }

  fn check_keyword(&self, start: usize, rest: Vec<char>, kind: TokenKind) -> TokenKind {
    println!("start {} current {}", self.start, self.current);
    println!("left {}, right {}", self.current - (self.start + start), rest.len());
    if self.current - (self.start + start) == rest.len() {
      for (left, right) in self.source[self.start..self.current - 1]
        .iter()
        .zip(rest.iter())
      {
        println!("left {}, right {}", left, right);
        if left != right {
          return TokenKind::Identifier;
        }
      }
      return kind;
    }

    TokenKind::Identifier
  }

  fn number(&mut self) -> Token {
    while is_digit(self.peek()) {
      self.advance();
    }

    if self.peek() == '.' {
      if let Some(next) = self.peek_next() {
        if is_digit(next) {
          self.advance();
          while is_digit(self.peek()) {
            self.advance();
          }
        }
      }
    }

    return self.make_token(TokenKind::Number);
  }

  fn string(&mut self) -> Token {
    println!("string");
    while self.peek() != '"' && !self.is_at_end() {
      if self.peek() == '\n' {
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
        ' ' | '\r' | '\t' => {
          self.advance();
        }
        '\n' => {
          self.line += 1;
          self.advance();
        }
        '/' => match self.peek_next() {
          Some(next) => {
            if next == '/' {
              while self.peek() != '\n' && self.is_at_end() {
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
      source: &self.source,
      line: self.line.clone(),
      start: self.start.clone(),
      end: self.current.clone(),
    }
  }

  fn error_token(&self, message: &'static [char]) -> Token {
    Token {
      kind: TokenKind::Error,
      source: message,
      line: self.line,
      start: self.start.clone(),
      end: self.current.clone(),
    }
  }

  fn peek_next(&self) -> Option<char> {
    if self.is_at_end() {
      return None;
    }

    Some(self.source[self.current + 1])
  }

  fn peek(&self) -> char {
    self.source[self.current]
  }

  fn advance(&mut self) -> char {
    self.current += 1;
    self.source[self.current - 1]
  }

  fn match_token(&mut self, expected: char) -> bool {
    if self.is_at_end() {
      return false;
    }

    if self.source[self.current] != expected {
      return false;
    }

    self.current += 1;
    true
  }

  fn is_at_end(&self) -> bool {
    self.current == self.source.len()
  }
}

fn is_digit(c: char) -> bool {
  c >= '0' && c <= '9'
}

fn is_alpha(c: char) -> bool {
  c >= 'a' && c <= 'z' || c >= 'A' && c < 'Z' || c == '_'
}
