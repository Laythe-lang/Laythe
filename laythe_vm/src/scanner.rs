use crate::token::{Token, TokenKind};
use laythe_core::utils::{next_boundary, previous_boundary};
use smol_str::SmolStr;

/// A scanner for the lox language. This struct is
/// responsible for taking a source string and tokenizing it
pub struct Scanner<'a> {
  /// The input source string
  source: &'a str,

  /// The current line number
  line: u32,

  /// The start of the current token
  start: usize,

  /// The current index into the source str
  current: usize,

  /// The start of the current character
  char_start: usize,
}

impl<'a> Scanner<'a> {
  /// Create a new scanner that can be used to tokenize
  /// a lox source string
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::scanner::Scanner;
  /// use laythe_vm::token::TokenKind;
  ///
  /// let source = String::from("
  /// let x = \"something\";
  /// if x != \"something\" {
  ///   print(x);
  /// }
  /// ");
  ///
  /// let mut scanner = Scanner::new(&source);
  /// let token = scanner.scan_token();
  /// assert_eq!(token.line, 2);
  /// assert_eq!(token.kind, TokenKind::Let);
  /// assert_eq!(token.lexeme, "let");
  /// ```
  pub fn new(source: &'a str) -> Scanner<'a> {
    let current = next_boundary(&source, 0);

    Scanner {
      source,

      start: 0,
      current,
      char_start: 0,

      line: 1,
    }
  }

  /// Scan the next token from the space lox source
  /// string provide.
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::scanner::{Scanner};
  /// use laythe_vm::token::TokenKind;
  ///
  /// let source = String::from("
  /// let x = \"something\";
  /// if x != \"something\" {
  ///   print(x);
  /// }
  /// ");
  ///
  /// let mut scanner = Scanner::new(&source);
  /// let mut token = scanner.scan_token();
  /// assert_eq!(token.line, 2);
  /// assert_eq!(token.kind, TokenKind::Let);
  /// assert_eq!(token.lexeme, "let");
  ///
  /// token = scanner.scan_token();
  /// assert_eq!(token.line, 2);
  /// assert_eq!(token.kind, TokenKind::Identifier);
  /// assert_eq!(token.lexeme, "x");
  ///
  /// token = scanner.scan_token();
  /// assert_eq!(token.line, 2);
  /// assert_eq!(token.kind, TokenKind::Equal);
  /// assert_eq!(token.lexeme, "=");
  /// ```
  pub fn scan_token(&mut self) -> Token {
    // advance whitespace
    self.skip_white_space();

    // find the previous unicode boundary
    self.start = previous_boundary(&self.source, self.current);
    self.char_start = self.start;

    // if at end return oef token
    if self.is_at_end() {
      return make_token(TokenKind::Eof, "", self.line);
    }

    // move scanner index and get current unicode character
    let char_start = self.char_start;
    let current = self.current;

    self.advance_indices();
    let char_slice = &self.source[char_start..current];
    match char_slice {
      "(" => self.make_token_source(TokenKind::LeftParen),
      ")" => self.make_token_source(TokenKind::RightParen),
      "{" => self.make_token_source(TokenKind::LeftBrace),
      "}" => self.make_token_source(TokenKind::RightBrace),
      "[" => self.make_token_source(TokenKind::LeftBracket),
      "]" => self.make_token_source(TokenKind::RightBracket),
      ":" => self.make_token_source(TokenKind::Colon),
      ";" => self.make_token_source(TokenKind::Semicolon),
      "," => self.make_token_source(TokenKind::Comma),
      "." => self.make_token_source(TokenKind::Dot),
      "-" => {
        if self.match_char(">") {
          self.make_token_source(TokenKind::Arrow)
        } else {
          self.make_token_source(TokenKind::Minus)
        }
      }
      "&" => self.make_token_source(TokenKind::Amp),
      "+" => self.make_token_source(TokenKind::Plus),
      "|" => self.make_token_source(TokenKind::Pipe),
      "/" => self.make_token_source(TokenKind::Slash),
      "*" => self.make_token_source(TokenKind::Star),
      "=" => {
        if self.match_char("=") {
          self.make_token_source(TokenKind::EqualEqual)
        } else {
          self.make_token_source(TokenKind::Equal)
        }
      }
      "<" => {
        if self.match_char("=") {
          self.make_token_source(TokenKind::LessEqual)
        } else {
          self.make_token_source(TokenKind::Less)
        }
      }
      ">" => {
        if self.match_char("=") {
          self.make_token_source(TokenKind::GreaterEqual)
        } else {
          self.make_token_source(TokenKind::Greater)
        }
      }
      "!" => {
        if self.match_char("=") {
          self.make_token_source(TokenKind::BangEqual)
        } else {
          self.make_token_source(TokenKind::Bang)
        }
      }
      "\"" => self.string("\""),
      "'" => self.string("'"),
      _ => {
        if is_digit(char_slice) {
          return self.number();
        }

        if is_alpha(char_slice) {
          return self.identifier();
        }

        self.error_token("Unexpected character")
      }
    }
  }

  /// Generate an identifier token
  fn identifier(&mut self) -> Token {
    // advance until we hit whitespace or a special char
    loop {
      if self.is_at_end() {
        break;
      }

      let peek = self.peek();
      if !(is_alpha(peek) || is_digit(peek)) {
        break;
      }

      self.advance_indices();
    }

    // identifier if we are actually a keyword
    self.make_token_source(self.identifier_type())
  }

  /// Generate a number token
  fn number(&mut self) -> Token {
    // advance consecutive digits
    while !self.is_at_end() && is_digit(self.peek()) {
      self.advance_indices();
    }

    // check if floating point format
    if !self.is_at_end() && self.peek() == "." {
      if let Some(next) = self.peek_next() {
        if is_digit(next) {
          self.advance_indices();
          while !self.is_at_end() && is_digit(self.peek()) {
            self.advance_indices();
          }
        }
      }
    }

    if self.match_char("e") || self.match_char("E") {
      match self.peek() {
        "+" | "-" => self.advance_indices(),
        _ => (),
      }

      if !is_digit(self.peek()) {
        self.error_token("Unterminated scientific notation.");
      }

      while !self.is_at_end() && is_digit(self.peek()) {
        self.advance_indices();
      }
    }

    self.make_token_source(TokenKind::Number)
  }

  /// Generate a string token
  fn string(&mut self, quote_char: &str) -> Token {
    let mut buffer = String::with_capacity(8);

    while !self.is_at_end() && self.peek() != quote_char {
      match self.peek() {
        "\n" => {
          self.line += 1;
          buffer.push('\n');
        }
        "\\" => {
          self.advance_indices();

          match self.peek() {
            "0" => buffer.push('\0'),
            "n" => buffer.push('\n'),
            "t" => buffer.push('\t'),
            "r" => buffer.push('\r'),
            "\\" => buffer.push('\\'),
            "'" => buffer.push('\''),
            "\"" => buffer.push('"'),
            "u" => {
              self.advance_indices();
              let start = self.current;

              // assert we have opening bracket
              if self.peek() != "{" {
                return self.error_token("Expected '{' unicode escape '\\u'");
              }
              self.advance_indices();

              let mut len = 0u8;

              // continue until we hit ending } or we're over the valid length
              while !self.is_at_end() && self.peek() != quote_char && self.peek() != "}" {
                self.advance_indices();
                if len > 6 {
                  return self
                    .error_token("Unicode escape have a hexidecimal longer than length 6");
                }

                len += 1;
              }

              if self.peek() == quote_char {
                return self.error_token("Expected '}' after unicode escape.");
              }

              if self.is_at_end() {
                return self.error_token("Unterminated string");
              }
              let unicode = &self.source[start..self.current - 1];

              match u32::from_str_radix(unicode, 16) {
                Ok(code_point) => match std::char::from_u32(code_point) {
                  Some(c) => buffer.push(c),
                  None => {
                    return self.error_token(&format!("Invalid unicode escape {}.", unicode));
                  }
                },
                Err(_) => {
                  return self.error_token(&format!(
                    "Invalid hexadecimal for unicode escape {}.",
                    unicode
                  ));
                }
              }
            }
            _ => {
              return self.error_token(&format!(
                "Invalid escape character '{}'.",
                self.current_slice()
              ));
            }
          }
        }
        c => buffer.push_str(c),
      }
      self.advance_indices();
    }

    if self.is_at_end() {
      return self.error_token("Unterminated string");
    }

    self.advance_indices();
    make_token(TokenKind::String, &buffer, self.line)
  }

  /// Advance through whitespace effectively throwing it away
  fn skip_white_space(&mut self) {
    while !self.is_at_end() {
      let c = self.peek();

      match c {
        " " | "\r" | "\t" => {
          self.advance_indices();
        }
        "\n" => {
          self.line += 1;
          self.advance_indices();
        }
        "/" => match self.peek_next() {
          Some(next) => {
            if next == "/" {
              while !self.is_at_end() && self.peek() != "\n" {
                self.advance_indices();
              }
            } else {
              return;
            }
          }
          None => return,
        },
        _ => return,
      }
    }
  }

  /// Identify if the current slice is a keyword.
  /// This uses a short of hard coded trie
  fn identifier_type(&self) -> TokenKind {
    match self.nth_char_from(self.start, 0) {
      Some(c1) => match c1 {
        "a" => self.check_keyword(1, "nd", TokenKind::And),
        "b" => self.check_keyword(1, "reak", TokenKind::Break),
        "c" => match self.nth_char_from(self.start, 1) {
          Some(c2) => match c2 {
            "a" => self.check_keyword(2, "tch", TokenKind::Catch),
            "o" => self.check_keyword(2, "ntinue", TokenKind::Continue),
            "l" => self.check_keyword(2, "ass", TokenKind::Class),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        "e" => match self.nth_char_from(self.start, 1) {
          Some(c2) => match c2 {
            "l" => self.check_keyword(2, "se", TokenKind::Else),
            "x" => self.check_keyword(2, "port", TokenKind::Export),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        "f" => match self.nth_char_from(self.start, 1) {
          Some(c2) => match c2 {
            "a" => self.check_keyword(2, "lse", TokenKind::False),
            "o" => self.check_keyword(2, "r", TokenKind::For),
            "r" => self.check_keyword(2, "om", TokenKind::From),
            "n" => TokenKind::Fun,
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        "i" => match self.nth_char_from(self.start, 1) {
          Some(c2) => match c2 {
            "f" => self.check_keyword(2, "", TokenKind::If),
            "m" => self.check_keyword(2, "port", TokenKind::Import),
            "n" => self.check_keyword(2, "", TokenKind::In),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        "l" => self.check_keyword(1, "et", TokenKind::Let),
        "n" => self.check_keyword(1, "il", TokenKind::Nil),
        "o" => self.check_keyword(1, "r", TokenKind::Or),
        "r" => self.check_keyword(1, "eturn", TokenKind::Return),
        "s" => match self.nth_char_from(self.start, 1) {
          Some(c2) => match c2 {
            "e" => self.check_keyword(2, "lf", TokenKind::Self_),
            "u" => self.check_keyword(2, "per", TokenKind::Super),
            "t" => self.check_keyword(2, "atic", TokenKind::Static),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        "t" => match self.nth_char_from(self.start, 1) {
          Some(c2) => match c2 {
            "r" => match self.nth_char_from(self.start, 2) {
              Some(c3) => match c3 {
                "a" => self.check_keyword(3, "it", TokenKind::Trait),
                "u" => self.check_keyword(3, "e", TokenKind::True),
                "y" => TokenKind::Try,
                _ => TokenKind::Identifier,
              },
              None => TokenKind::Identifier,
            },
            "y" => self.check_keyword(2, "pe", TokenKind::Type),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        "w" => self.check_keyword(1, "hile", TokenKind::While),
        _ => TokenKind::Identifier,
      },
      None => panic!(""),
    }
  }

  /// Check if the remainder of the current slice matches the rest
  /// of the keyword
  fn check_keyword(&self, start: usize, rest: &str, kind: TokenKind) -> TokenKind {
    let start_index = self.nth_next_boundary(self.start, start);
    let len = self.current - start_index - 1;

    if len == rest.len() && rest == &self.source[start_index..self.current - 1] {
      return kind;
    }

    TokenKind::Identifier
  }

  /// Make a token from the current state of the scanner
  fn make_token_source(&'a self, kind: TokenKind) -> Token {
    make_token(kind, self.current_slice(), self.line)
  }

  /// Make a new error token
  fn error_token(&'a self, message: &str) -> Token {
    make_token(TokenKind::Error, message, self.line)
  }

  /// Peek the next token
  fn peek_next(&self) -> Option<&str> {
    let start = self.current;
    let end = next_boundary(&self.source, self.current);

    if self.char_index_at_end(end) {
      return None;
    }

    Some(&self.source[start..end])
  }

  /// Peek the current token
  fn peek(&self) -> &str {
    unsafe { self.source.get_unchecked(self.char_start..self.current) }
  }

  /// Find the nth char from the current index
  fn nth_char_from(&self, start: usize, n: u8) -> Option<&str> {
    let mut current_index = next_boundary(&self.source, start);
    let mut start_index = start;

    for _ in 0..n {
      start_index = current_index;
      current_index = next_boundary(&self.source, current_index);
    }

    if self.char_index_at_end(current_index) {
      None
    } else {
      Some(&self.source[start_index..current_index])
    }
  }

  /// Get the current str slice
  fn current_slice(&'a self) -> &'a str {
    &self.source[self.start..self.current - 1]
  }

  /// Advance the housekeeping indices
  fn advance_indices(&mut self) {
    self.char_start = self.current;
    self.current = next_boundary(&self.source, self.current);
  }

  /// Find the nth next char boundary
  fn nth_next_boundary(&self, start: usize, n: usize) -> usize {
    let mut current = start;
    for _ in 0..n {
      current = next_boundary(&self.source, current);
    }

    current
  }

  /// match the current token against an expected
  fn match_char(&mut self, expected: &str) -> bool {
    if self.is_at_end() {
      return false;
    }

    if self.peek() != expected {
      return false;
    }

    self.advance_indices();
    true
  }

  /// Is the scanner at eof
  fn is_at_end(&self) -> bool {
    self.current > self.source.len()
  }

  /// Is the index at eof
  fn char_index_at_end(&self, index: usize) -> bool {
    index > self.source.len()
  }
}

/// Make a new token
fn make_token(kind: TokenKind, raw: &str, line: u32) -> Token {
  let lexeme = SmolStr::new(raw);
  Token { kind, lexeme, line }
}

/// Is the str slice a digit. Assumes single char
fn is_digit(c: &str) -> bool {
  ("0"..="9").contains(&c)
  // c >= "0" && c <= "9"
}

/// Is the str slice a alphabetic. Assumes single char
fn is_alpha(c: &str) -> bool {
  ("a"..="z").contains(&c) || ("A"..="Z").contains(&c) || c == "_"
  // (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c == "_"
}

#[cfg(test)]
mod test {
  use super::*;
  use std::collections::HashMap;

  enum TokenGen {
    Symbol(Box<dyn Fn() -> String>),
    Comparator(Box<dyn Fn() -> String>),
    ALpha(Box<dyn Fn() -> String>),
  }

  fn token_gen() -> HashMap<TokenKind, TokenGen> {
    let mut map = HashMap::new();

    map.insert(
      TokenKind::LeftParen,
      TokenGen::Symbol(Box::new(|| "(".to_string())),
    );
    map.insert(
      TokenKind::RightParen,
      TokenGen::Symbol(Box::new(|| ")".to_string())),
    );
    map.insert(
      TokenKind::LeftBrace,
      TokenGen::Symbol(Box::new(|| "{".to_string())),
    );
    map.insert(
      TokenKind::RightBrace,
      TokenGen::Symbol(Box::new(|| "}".to_string())),
    );
    map.insert(
      TokenKind::Comma,
      TokenGen::Symbol(Box::new(|| ",".to_string())),
    );
    map.insert(
      TokenKind::Dot,
      TokenGen::Symbol(Box::new(|| ".".to_string())),
    );
    map.insert(
      TokenKind::Minus,
      TokenGen::Symbol(Box::new(|| "-".to_string())),
    );
    map.insert(
      TokenKind::Amp,
      TokenGen::Symbol(Box::new(|| "&".to_string())),
    );
    map.insert(
      TokenKind::Arrow,
      TokenGen::Symbol(Box::new(|| "->".to_string())),
    );
    map.insert(
      TokenKind::Plus,
      TokenGen::Symbol(Box::new(|| "+".to_string())),
    );
    map.insert(
      TokenKind::Semicolon,
      TokenGen::Symbol(Box::new(|| ";".to_string())),
    );
    map.insert(
      TokenKind::Slash,
      TokenGen::Symbol(Box::new(|| "/".to_string())),
    );
    map.insert(
      TokenKind::Star,
      TokenGen::Symbol(Box::new(|| "*".to_string())),
    );
    map.insert(
      TokenKind::Bang,
      TokenGen::Comparator(Box::new(|| "!".to_string())),
    );
    map.insert(
      TokenKind::BangEqual,
      TokenGen::Comparator(Box::new(|| "!=".to_string())),
    );
    map.insert(
      TokenKind::Equal,
      TokenGen::Comparator(Box::new(|| "=".to_string())),
    );
    map.insert(
      TokenKind::EqualEqual,
      TokenGen::Comparator(Box::new(|| "==".to_string())),
    );
    map.insert(
      TokenKind::Greater,
      TokenGen::Comparator(Box::new(|| ">".to_string())),
    );
    map.insert(
      TokenKind::GreaterEqual,
      TokenGen::Comparator(Box::new(|| ">=".to_string())),
    );
    map.insert(
      TokenKind::Less,
      TokenGen::Comparator(Box::new(|| "<".to_string())),
    );
    map.insert(
      TokenKind::LessEqual,
      TokenGen::Comparator(Box::new(|| "<=".to_string())),
    );
    map.insert(
      TokenKind::Identifier,
      TokenGen::ALpha(Box::new(|| "example".to_string())),
    );
    map.insert(
      TokenKind::String,
      TokenGen::Symbol(Box::new(|| "\"example\"".to_string())),
    );
    map.insert(
      TokenKind::Number,
      TokenGen::ALpha(Box::new(|| "12345".to_string())),
    );
    map.insert(
      TokenKind::And,
      TokenGen::ALpha(Box::new(|| "and".to_string())),
    );
    map.insert(
      TokenKind::Break,
      TokenGen::ALpha(Box::new(|| "break".to_string())),
    );
    map.insert(
      TokenKind::Catch,
      TokenGen::ALpha(Box::new(|| "catch".to_string())),
    );
    map.insert(
      TokenKind::Class,
      TokenGen::ALpha(Box::new(|| "class".to_string())),
    );
    map.insert(
      TokenKind::Continue,
      TokenGen::ALpha(Box::new(|| "continue".to_string())),
    );
    map.insert(
      TokenKind::Else,
      TokenGen::ALpha(Box::new(|| "else".to_string())),
    );
    map.insert(
      TokenKind::Export,
      TokenGen::ALpha(Box::new(|| "export".to_string())),
    );
    map.insert(
      TokenKind::False,
      TokenGen::ALpha(Box::new(|| "false".to_string())),
    );
    map.insert(
      TokenKind::For,
      TokenGen::ALpha(Box::new(|| "for".to_string())),
    );
    map.insert(
      TokenKind::From,
      TokenGen::ALpha(Box::new(|| "from".to_string())),
    );
    map.insert(
      TokenKind::Fun,
      TokenGen::ALpha(Box::new(|| "fn".to_string())),
    );
    map.insert(
      TokenKind::If,
      TokenGen::ALpha(Box::new(|| "if".to_string())),
    );
    map.insert(
      TokenKind::Import,
      TokenGen::ALpha(Box::new(|| "import".to_string())),
    );
    map.insert(
      TokenKind::In,
      TokenGen::ALpha(Box::new(|| "in".to_string())),
    );
    map.insert(
      TokenKind::Nil,
      TokenGen::ALpha(Box::new(|| "nil".to_string())),
    );
    map.insert(
      TokenKind::Or,
      TokenGen::ALpha(Box::new(|| "or".to_string())),
    );
    map.insert(
      TokenKind::Return,
      TokenGen::ALpha(Box::new(|| "return".to_string())),
    );
    map.insert(
      TokenKind::Super,
      TokenGen::ALpha(Box::new(|| "super".to_string())),
    );
    map.insert(
      TokenKind::Self_,
      TokenGen::ALpha(Box::new(|| "self".to_string())),
    );
    map.insert(
      TokenKind::Static,
      TokenGen::ALpha(Box::new(|| "static".to_string())),
    );
    map.insert(
      TokenKind::True,
      TokenGen::ALpha(Box::new(|| "true".to_string())),
    );
    map.insert(
      TokenKind::Try,
      TokenGen::ALpha(Box::new(|| "try".to_string())),
    );
    map.insert(
      TokenKind::Let,
      TokenGen::ALpha(Box::new(|| "let".to_string())),
    );
    map.insert(
      TokenKind::While,
      TokenGen::ALpha(Box::new(|| "while".to_string())),
    );
    map.insert(
      TokenKind::Trait,
      TokenGen::ALpha(Box::new(|| "trait".to_string())),
    );
    map.insert(
      TokenKind::Type,
      TokenGen::ALpha(Box::new(|| "type".to_string())),
    );
    map.insert(
      TokenKind::Error,
      TokenGen::ALpha(Box::new(|| "$$".to_string())),
    );
    map.insert(TokenKind::Eof, TokenGen::ALpha(Box::new(|| "".to_string())));

    map
  }

  #[test]
  fn empty_string() {
    let empty = "".to_string();
    let mut scanner = Scanner::new(&empty);

    let token_eof = scanner.scan_token().clone();
    assert_eq!(token_eof.kind, TokenKind::Eof);
    assert_eq!(token_eof.lexeme, "");
  }

  #[test]
  fn string_escape_sequence() {
    let tests = vec![
      ("\"\\n\"", "\n"),
      ("\"\\t\"", "\t"),
      ("\"\\r\"", "\r"),
      ("\"\\0\"", "\0"),
      ("\"\\\"\"", "\""),
      ("\"\\'\"", "'"),
      ("\"\\\\\"", "\\"),
      ("\"\\\\\"", "\\"),
      ("\"\u{1F4AF}\"", "💯"),
    ];

    for (input, expected) in tests {
      let mut scanner = Scanner::new(&input);
      let scanned_token = scanner.scan_token().clone();
      assert_eq!(scanned_token.kind, TokenKind::String);
      assert_eq!(scanned_token.lexeme, expected);
    }
  }

  #[test]
  fn single_token() {
    for (token_kind, gen) in token_gen() {
      let example = match gen {
        TokenGen::Symbol(x) => x(),
        TokenGen::ALpha(x) => x(),
        TokenGen::Comparator(x) => x(),
      };

      let mut scanner = Scanner::new(&example);
      let scanned_token = scanner.scan_token().clone();
      assert_eq!(scanned_token.kind, token_kind.clone());
    }
  }

  #[test]
  fn multiple_tokens() {
    let basic = "10 + 3".to_string();
    let mut scanner = Scanner::new(&basic);

    let token_ten = scanner.scan_token().clone();
    assert_eq!(token_ten.kind, TokenKind::Number);
    assert_eq!(token_ten.lexeme, "10");

    let token_plus = scanner.scan_token().clone();
    assert_eq!(token_plus.kind, TokenKind::Plus);
    assert_eq!(token_plus.lexeme, "+");

    let token_three = scanner.scan_token().clone();
    assert_eq!(token_three.kind, TokenKind::Number);
    assert_eq!(token_three.lexeme, "3");

    let token_eof = scanner.scan_token().clone();
    assert_eq!(token_eof.kind, TokenKind::Eof);
    assert_eq!(token_eof.lexeme, "");
  }
}
