use std::{iter::Peekable, str::CharIndices, usize};

use super::ir::token::{Lexeme, Token, TokenKind};
use crate::source::LineOffsets;
use laythe_core::utils::next_boundary;

/// Tracking information for one layer of string interpolation
struct Interpolation {
  // the bracket depth for the current string interpolation
  brackets: u32,

  // the quoting character for this string interpolation
  quote_char: char,
}

/// A scanner for the lox language. This struct is
/// responsible for taking a source string and tokenizing it
pub struct Scanner<'a> {
  /// The input source string
  source: &'a str,

  /// A peekable char indicies iterator
  char_indices: Peekable<CharIndices<'a>>,

  /// The current string interpolation depth
  interpolations: Vec<Interpolation>,

  /// The current line number
  line_offsets: Vec<usize>,

  /// The start of the current token
  start: usize,

  /// The start of the current token
  current: usize,

  /// The current char
  current_char: char,
}

impl<'a> Scanner<'a> {
  /// Create a new scanner from the provided source string.
  /// The scanner produces a sequence of tokens
  /// by calling the `scan_token` method
  pub fn new(source: &'a str) -> Scanner<'a> {
    assert!(
      source.len() < std::u32::MAX as usize,
      "Can only read files less than {} bytes",
      std::u32::MAX
    );

    let char_indices = source.char_indices().peekable();

    let mut line_offsets = Vec::with_capacity(source_line_heuristic_guess(source.len()));
    line_offsets.push(0);

    let mut scanner = Scanner {
      source,
      char_indices,

      interpolations: Vec::new(),

      start: 0,
      current: 0,
      current_char: 'a',

      line_offsets,
    };

    scanner.initial_skip_comment();
    scanner
  }

  /// Scan the next token from the source
  /// string provided.
  pub fn scan_token(&mut self) -> Token<'a> {
    // advance whitespace
    self.skip_white_space();

    let maybe_char = self.next();
    self.start = self.current;

    // if at end return oef token
    if maybe_char.is_none() {
      return make_token(TokenKind::Eof, "", self.start, self.current_offset());
    }

    let c = maybe_char.unwrap();
    match c {
      '(' => self.make_token_source(TokenKind::LeftParen),
      ')' => self.make_token_source(TokenKind::RightParen),
      '{' => {
        if !self.interpolations.is_empty() {
          let end = self.interpolations.len() - 1;
          self.interpolations[end].brackets += 1;
        }
        self.make_token_source(TokenKind::LeftBrace)
      },
      '}' => {
        if !self.interpolations.is_empty() {
          let end = self.interpolations.len() - 1;
          self.interpolations[end].brackets -= 1;

          if self.interpolations[end].brackets == 0 {
            let interpolation = self.interpolations.pop().expect("Expected interpolation.");
            return self.string(TokenKind::StringSegment, interpolation.quote_char);
          }
        }

        self.make_token_source(TokenKind::RightBrace)
      },
      '[' => self.make_token_source(TokenKind::LeftBracket),
      ']' => self.make_token_source(TokenKind::RightBracket),
      ':' => self.make_token_source(TokenKind::Colon),
      '?' => self.make_token_source(TokenKind::QuestionMark),
      ';' => self.make_token_source(TokenKind::Semicolon),
      ',' => self.make_token_source(TokenKind::Comma),
      '.' => self.make_token_source(TokenKind::Dot),
      '-' => {
        if self.match_char('>') {
          self.make_token_source(TokenKind::RightArrow)
        } else if self.match_char('=') {
          self.make_token_source(TokenKind::MinusEqual)
        } else {
          self.make_token_source(TokenKind::Minus)
        }
      },
      '&' => self.make_token_source(TokenKind::Amp),
      '+' => {
        if self.match_char('=') {
          self.make_token_source(TokenKind::PlusEqual)
        } else {
          self.make_token_source(TokenKind::Plus)
        }
      },
      '|' => self.make_token_source(TokenKind::Pipe),
      '/' => {
        if self.match_char('=') {
          self.make_token_source(TokenKind::SlashEqual)
        } else {
          self.make_token_source(TokenKind::Slash)
        }
      },
      '*' => {
        if self.match_char('=') {
          self.make_token_source(TokenKind::StarEqual)
        } else {
          self.make_token_source(TokenKind::Star)
        }
      },
      '=' => {
        if self.match_char('=') {
          self.make_token_source(TokenKind::EqualEqual)
        } else {
          self.make_token_source(TokenKind::Equal)
        }
      },
      '<' => {
        if self.match_char('=') {
          self.make_token_source(TokenKind::LessEqual)
        } else if self.match_char('-') {
          self.make_token_source(TokenKind::LeftArrow)
        } else {
          self.make_token_source(TokenKind::Less)
        }
      },
      '>' => {
        if self.match_char('=') {
          self.make_token_source(TokenKind::GreaterEqual)
        } else {
          self.make_token_source(TokenKind::Greater)
        }
      },
      '!' => {
        if self.match_char('=') {
          self.make_token_source(TokenKind::BangEqual)
        } else {
          self.make_token_source(TokenKind::Bang)
        }
      },
      '"' => self.string(TokenKind::String, '"'),
      '\'' => self.string(TokenKind::String, '\''),
      '@' => self.instance_access(),
      _ => {
        if is_digit(c) {
          return self.number();
        }

        if is_alpha(c) {
          return self.identifier();
        }

        self.error_token("Unexpected character.")
      },
    }
  }

  /// Retrieve this files line offsets after it has
  /// been scanned
  pub fn line_offsets(mut self) -> LineOffsets {
    while let Some(c) = self.peek() {
      self.next();

      if c == '\n' {
        self.new_line();
      }
    }

    self.line_offsets.shrink_to_fit();
    LineOffsets::new(self.line_offsets, self.source.len())
  }

  /// Generate an instance access token
  fn instance_access(&mut self) -> Token<'a> {
    // advance until we hit whitespace or a special char
    while self.next_if(|c| is_alpha(*c) || is_digit(*c)).is_some() {}

    // identifier if we are actually a keyword
    self.make_token_source(TokenKind::InstanceAccess)
  }

  /// Generate an identifier token
  fn identifier(&mut self) -> Token<'a> {
    // advance until we hit whitespace or a special char
    while self.next_if(|c| is_alpha(*c) || is_digit(*c)).is_some() {}

    // identifier if we are actually a keyword
    self.make_token_source(self.identifier_type())
  }

  /// Generate a number token
  fn number(&mut self) -> Token<'a> {
    // advance consecutive digits
    while self.next_if(|c| is_digit(*c)).is_some() {}

    // check if floating point format
    if let Some('.') = self.peek() {
      let mut chars = self.source[self.current_offset()..].chars();
      chars.next();

      if let Some(peek_next) = chars.next() {
        if is_digit(peek_next) {
          self.next();

          while self.next_if(|c| is_digit(*c)).is_some() {}
        }
      }
    }

    if self.match_char('e') || self.match_char('E') {
      if self.match_char('+') || self.match_char('-') {}

      if self.next_if(|c| is_digit(*c)).is_none() {
        return self.error_token("Unterminated scientific notation.");
      }

      while self.next_if(|c| is_digit(*c)).is_some() {}
    }

    self.make_token_source(TokenKind::Number)
  }

  /// Generate a string token
  fn string(&mut self, kind: TokenKind, quote_char: char) -> Token<'a> {
    let mut buffer = String::with_capacity(8);
    let mut kind = kind;

    loop {
      let curr_char = self.peek();
      self.next();

      match curr_char {
        Some('\n') => {
          self.new_line();
          buffer.push('\n');
        },
        Some('\\') => {
          match self.next() {
            Some('0') => buffer.push('\0'),
            Some('n') => buffer.push('\n'),
            Some('t') => buffer.push('\t'),
            Some('r') => buffer.push('\r'),
            Some('\\') => buffer.push('\\'),
            Some('\'') => buffer.push('\''),
            Some('"') => buffer.push('"'),
            Some('u') => {
              // assert we have opening bracket
              match self.next() {
                Some('{') => {},
                _ => return self.error_token("Expected '{' unicode escape '\\u'"),
              }

              let start = self.current_offset();
              let mut len = 0u8;

              // continue until we hit ending } or we're over the valid length
              loop {
                match self.next() {
                  Some(c) => {
                    if c == '}' {
                      break;
                    }
                    if c == quote_char {
                      return self.error_token("Expected '}' after unicode escape sequence.");
                    }
                  },
                  None => return self.error_token("Unterminated string."),
                }

                if len > 6 {
                  return self.error_token(
                    "Unicode escape sequence has a hexidecimal longer than length 6.",
                  );
                }

                len += 1;
              }

              let unicode = &self.source[start..self.current];

              match u32::from_str_radix(unicode, 16) {
                Ok(code_point) => match std::char::from_u32(code_point) {
                  Some(c) => buffer.push(c),
                  None => {
                    return self.error_token_owned(format!("Invalid unicode escape {}.", unicode));
                  },
                },
                Err(_) => {
                  return self.error_token_owned(format!(
                    "Invalid hexadecimal unicode escape sequence {}.",
                    unicode
                  ));
                },
              }
            },
            Some(_) => {
              return self.error_token_owned(format!(
                "Invalid escape character '{}'.",
                self.current_slice()
              ))
            },
            None => {
              return self.error_token_owned(format!(
                "Invalid escape character '{}'.",
                self.current_slice()
              ))
            },
          }
        },
        Some('$') => {
          if self.next_if(|c| *c == '{').is_some() {
            if let TokenKind::String = kind {
              kind = TokenKind::StringStart
            }
            self.interpolations.push(Interpolation {
              quote_char,
              brackets: 1,
            });
            break;
          } else {
            buffer.push('$');
          }
        },
        Some(c) => {
          if c == quote_char {
            if let TokenKind::StringSegment = kind {
              kind = TokenKind::StringEnd;
            }
            break;
          } else {
            buffer.push(c)
          }
        },
        None => return self.error_token("Unterminated string."),
      }
    }

    make_token_owned(kind, buffer, self.start, self.current_offset())
  }

  /// Advance through whitespace effectively throwing it away
  fn skip_white_space(&mut self) {
    while let Some(c) = self.peek() {
      match c {
        ' ' | '\r' | '\t' => {
          self.next();
        },
        '\n' => {
          self.next();
          self.new_line();
        },
        '/' => {
          let mut chars = self.source[self.current_offset()..].chars();
          chars.next();

          match chars.next() {
            Some(c) => {
              if c == '/' {
                while self.next_if(|c| *c != '\n').is_some() {}
              } else {
                return;
              }
            },
            None => return,
          }
        },
        _ => return,
      }
    }
  }

  // skip a comment if it appears first in the source file
  fn initial_skip_comment(&mut self) {
    while let Some(c) = self.peek() {
      match c {
        '/' => {
          let mut chars = self.source.chars();
          chars.next();

          match chars.next() {
            Some(c) => {
              if c == '/' {
                while self.next_if(|c| *c != '\n').is_some() {}
              } else {
                return;
              }
            },
            None => return,
          }
        },
        _ => return,
      }
    }
  }

  /// Identify if the current slice is a keyword.
  /// This uses a short of hard coded trie
  fn identifier_type(&self) -> TokenKind {
    let mut chars = self.current_slice().chars();

    match chars.next() {
      Some(c1) => match c1 {
        'a' => match chars.next() {
          Some(c2) => match c2 {
            'n' => self.check_keyword(2, "d", TokenKind::And),
            's' => self.check_keyword_len(2, TokenKind::As),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        'b' => self.check_keyword(1, "reak", TokenKind::Break),
        'c' => match chars.next() {
          Some(c2) => match c2 {
            'a' => self.check_keyword(2, "tch", TokenKind::Catch),
            'h' => self.check_keyword(2, "an", TokenKind::Channel),
            'o' => self.check_keyword(2, "ntinue", TokenKind::Continue),
            'l' => self.check_keyword(2, "ass", TokenKind::Class),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        'e' => match chars.next() {
          Some(c2) => match c2 {
            'l' => self.check_keyword(2, "se", TokenKind::Else),
            'x' => self.check_keyword(2, "port", TokenKind::Export),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        'f' => match chars.next() {
          Some(c2) => match c2 {
            'a' => self.check_keyword(2, "lse", TokenKind::False),
            'o' => self.check_keyword(2, "r", TokenKind::For),
            'n' => self.check_keyword_len(2, TokenKind::Fun),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        'i' => match chars.next() {
          Some(c2) => match c2 {
            'f' => self.check_keyword_len(2, TokenKind::If),
            'm' => self.check_keyword(2, "port", TokenKind::Import),
            'n' => self.check_keyword_len(2, TokenKind::In),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        'l' => match chars.next() {
          Some(c2) => match c2 {
            'a' => self.check_keyword(2, "unch", TokenKind::Launch),
            'e' => self.check_keyword(2, "t", TokenKind::Let),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        'n' => self.check_keyword(1, "il", TokenKind::Nil),
        'o' => self.check_keyword(1, "r", TokenKind::Or),
        'r' => self.check_keyword(1, "eturn", TokenKind::Return),
        's' => match chars.next() {
          Some(c2) => match c2 {
            'e' => self.check_keyword(2, "lf", TokenKind::Self_),
            'u' => self.check_keyword(2, "per", TokenKind::Super),
            't' => self.check_keyword(2, "atic", TokenKind::Static),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        't' => match chars.next() {
          Some(c2) => match c2 {
            'r' => match chars.next() {
              Some(c3) => match c3 {
                'a' => self.check_keyword(3, "it", TokenKind::Trait),
                'u' => self.check_keyword(3, "e", TokenKind::True),
                'y' => self.check_keyword_len(3, TokenKind::Try),
                _ => TokenKind::Identifier,
              },
              None => TokenKind::Identifier,
            },
            'y' => self.check_keyword(2, "pe", TokenKind::Type),
            _ => TokenKind::Identifier,
          },
          None => TokenKind::Identifier,
        },
        'w' => self.check_keyword(1, "hile", TokenKind::While),
        _ => TokenKind::Identifier,
      },
      None => panic!(""),
    }
  }

  /// Check if the remainder of the current slice matches the rest
  /// of the keyword
  fn check_keyword(&self, start: usize, rest: &str, kind: TokenKind) -> TokenKind {
    // we can do a straight addition here because all keywords are ascii characters
    let start_index = self.start + start;
    debug_assert!(self.nth_next_boundary(self.start, start) == start_index);

    let len = self.current_offset() - start_index;

    if len == rest.len() && rest == &self.source[start_index..self.current_offset()] {
      return kind;
    }

    TokenKind::Identifier
  }

  /// Check if token is the correct length for this provided token kind
  fn check_keyword_len(&self, len: usize, kind: TokenKind) -> TokenKind {
    let token_len = self.current_offset() - self.start;

    if len == token_len {
      return kind;
    }

    TokenKind::Identifier
  }

  /// Make a token from the current state of the scanner
  fn make_token_source(&self, kind: TokenKind) -> Token<'a> {
    make_token(
      kind,
      self.current_slice(),
      self.start,
      self.current_offset(),
    )
  }

  /// Make a new error token
  fn error_token(&self, message: &'a str) -> Token<'a> {
    make_token(TokenKind::Error, message, self.start, self.current_offset())
  }

  /// Make a owned error error token
  fn error_token_owned(&self, message: String) -> Token<'static> {
    make_token_owned(
      TokenKind::Error,
      message,
      self.start,
      next_boundary(self.source, self.current),
    )
  }

  /// Advance the character iterator if condition is met
  fn next_if(&mut self, f: impl FnOnce(&char) -> bool) -> Option<char> {
    self.char_indices.next_if(|(_, c)| f(c)).map(|(index, c)| {
      self.current = index;
      self.current_char = c;
      c
    })
  }

  /// Get the next character
  fn next(&mut self) -> Option<char> {
    self.char_indices.next().map(|(index, c)| {
      self.current = index;
      self.current_char = c;
      c
    })
  }

  /// Peek the current character
  fn peek(&mut self) -> Option<char> {
    self.char_indices.peek().map(|(_, c)| *c)
  }

  /// Insert a new line
  fn new_line(&mut self) {
    self.line_offsets.push(self.current_offset());
  }

  /// Get the current str slice
  fn current_slice(&self) -> &'a str {
    &self.source[self.start..self.current_offset()]
  }

  /// Get the current offset
  fn current_offset(&self) -> usize {
    self.current + self.current_char.len_utf8()
  }

  /// Find the nth next char boundary
  fn nth_next_boundary(&self, start: usize, n: usize) -> usize {
    let mut current = start;
    for _ in 0..n {
      current = next_boundary(self.source, current);
    }

    current
  }

  /// match the current token against an expected
  fn match_char(&mut self, expected: char) -> bool {
    self.next_if(|c| *c == expected).is_some()
  }
}

/// Make a new token
fn make_token(kind: TokenKind, lexeme: &str, start: usize, end: usize) -> Token {
  Token::new(kind, Lexeme::Slice(lexeme), start as u32, end as u32)
}

/// Make a new token
fn make_token_owned<'a>(kind: TokenKind, lexeme: String, start: usize, end: usize) -> Token<'a> {
  Token::new(kind, Lexeme::Owned(lexeme), start as u32, end as u32)
}

/// Is the str slice a digit. Assumes single char
fn is_digit(c: char) -> bool {
  ('0'..='9').contains(&c)
}

/// Is the str slice a digit. Assumes single char
fn is_alpha(c: char) -> bool {
  ('A'..='Z').contains(&c) || ('a'..='z').contains(&c) || c == '_'
}

/// A loose estimate for how many characters are in a typical line
const fn source_line_heuristic_guess(len: usize) -> usize {
  len / 20
}

#[cfg(test)]
mod test {
  use super::super::ir::ast::Spanned;
  use super::*;
  use crate::source::LineError;
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
      TokenKind::MinusEqual,
      TokenGen::Symbol(Box::new(|| "-=".to_string())),
    );
    map.insert(
      TokenKind::QuestionMark,
      TokenGen::Symbol(Box::new(|| "?".to_string())),
    );
    map.insert(
      TokenKind::Amp,
      TokenGen::Symbol(Box::new(|| "&".to_string())),
    );
    map.insert(
      TokenKind::RightArrow,
      TokenGen::Symbol(Box::new(|| "->".to_string())),
    );
    map.insert(
      TokenKind::Plus,
      TokenGen::Symbol(Box::new(|| "+".to_string())),
    );
    map.insert(
      TokenKind::PlusEqual,
      TokenGen::Symbol(Box::new(|| "+=".to_string())),
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
      TokenKind::SlashEqual,
      TokenGen::Symbol(Box::new(|| "/=".to_string())),
    );
    map.insert(
      TokenKind::Star,
      TokenGen::Symbol(Box::new(|| "*".to_string())),
    );
    map.insert(
      TokenKind::StarEqual,
      TokenGen::Symbol(Box::new(|| "*=".to_string())),
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
      TokenKind::InstanceAccess,
      TokenGen::ALpha(Box::new(|| "@field".to_string())),
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
      TokenKind::Channel,
      TokenGen::ALpha(Box::new(|| "chan".to_string())),
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
      TokenKind::As,
      TokenGen::ALpha(Box::new(|| "as".to_string())),
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
      TokenKind::Launch,
      TokenGen::ALpha(Box::new(|| "launch".to_string())),
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
  fn new() {
    let source = "
    let x = \"something\";
    if x != \"something\" {
      print(x);
    }
    ";

    let mut scanner = Scanner::new(source);
    let token = scanner.scan_token();

    assert_eq!(token.start(), 5);
    assert_eq!(token.end(), 8);
    assert_eq!(token.kind(), TokenKind::Let);
    assert_eq!(token.str(), "let");
  }

  #[test]
  fn scan_token() {
    let source = "
    let x = \"something\";
    if x != \"something\" {
      print(x);
    }
    ";

    let mut scanner = Scanner::new(&source);
    let mut token = scanner.scan_token();
    assert_eq!(token.start(), 5);
    assert_eq!(token.end(), 8);
    assert_eq!(token.kind(), TokenKind::Let);
    assert_eq!(token.str(), "let");

    token = scanner.scan_token();
    assert_eq!(token.start(), 9);
    assert_eq!(token.end(), 10);
    assert_eq!(token.kind(), TokenKind::Identifier);
    assert_eq!(token.str(), "x");

    token = scanner.scan_token();
    assert_eq!(token.start(), 11);
    assert_eq!(token.end(), 12);
    assert_eq!(token.kind(), TokenKind::Equal);
    assert_eq!(token.str(), "=");
  }

  #[test]
  fn line_offsets() {
    let source = "
    let x = \"something\";
    if x != \"something\" {
      print(x);
    }";

    let mut scanner = Scanner::new(&source);

    scanner.scan_token();
    let offsets = scanner.line_offsets();

    assert_eq!(offsets.lines(), 5);
    assert_eq!(offsets.line_range(0), Ok(0..1));
    assert_eq!(offsets.line_range(1), Ok(1..26));
    assert_eq!(offsets.line_range(2), Ok(26..52));
    assert_eq!(offsets.line_range(3), Ok(52..68));
    assert_eq!(offsets.line_range(4), Ok(68..73));
    assert_eq!(offsets.line_range(5), Err(LineError::LineOutOfBounds));
  }

  #[test]
  fn empty_string() {
    let empty = "".to_string();
    let mut scanner = Scanner::new(&empty);

    let token_eof = scanner.scan_token().clone();
    assert_eq!(token_eof.kind(), TokenKind::Eof);
    assert_eq!(token_eof.str(), "");
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
      let scanned_token = scanner.scan_token();
      assert_eq!(scanned_token.kind(), TokenKind::String);
      assert_eq!(scanned_token.str(), expected);
    }
  }

  #[test]
  fn string_interpolation() {
    let source = "
    let x = 'My name is ${name} and am ${'${20}'}';
    ";

    let mut scanner = Scanner::new(&source);
    let asserts = [
      (TokenKind::Let, "let"),
      (TokenKind::Identifier, "x"),
      (TokenKind::Equal, "="),
      (TokenKind::StringStart, "My name is "),
      (TokenKind::Identifier, "name"),
      (TokenKind::StringSegment, " and am "),
      (TokenKind::StringStart, ""),
      (TokenKind::Number, "20"),
      (TokenKind::StringEnd, ""),
      (TokenKind::StringEnd, ""),
    ];

    for (kind, lexeme) in asserts.iter() {
      let scanned_token = scanner.scan_token();
      assert_eq!(scanned_token.kind(), *kind);
      assert_eq!(scanned_token.str(), *lexeme);
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
      assert_eq!(scanned_token.kind(), token_kind.clone());
    }
  }

  #[test]
  fn multiple_tokens() {
    let basic = "10 + 3".to_string();
    let mut scanner = Scanner::new(&basic);

    let token_ten = scanner.scan_token().clone();
    assert_eq!(token_ten.kind(), TokenKind::Number);
    assert_eq!(token_ten.str(), "10");

    let token_plus = scanner.scan_token().clone();
    assert_eq!(token_plus.kind(), TokenKind::Plus);
    assert_eq!(token_plus.str(), "+");

    let token_three = scanner.scan_token().clone();
    assert_eq!(token_three.kind(), TokenKind::Number);
    assert_eq!(token_three.str(), "3");

    let token_eof = scanner.scan_token().clone();
    assert_eq!(token_eof.kind(), TokenKind::Eof);
    assert_eq!(token_eof.str(), "");
  }
}
