use smol_str::SmolStr;

/// A token in the space lox language
#[derive(Debug, Clone)]
pub struct Token {
  /// The token kind
  pub kind: TokenKind,

  /// The character array of the source
  pub lexeme: SmolStr,

  /// line number this token appears
  pub line: u32,
}

/// Token kinds in the space lox language
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenKind {
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  LeftBracket,
  RightBracket,
  Comma,
  Dot,
  Minus,
  Plus,
  Colon,
  Semicolon,
  Pipe,
  Slash,
  Star,

  // modules
  Export,
  Import,
  From,

  // logical operators
  Bang,
  BangEqual,
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,

  // sequences
  Identifier,
  String,
  Number,

  // keywords
  And,
  Class,
  Else,
  False,
  For,
  Fn,
  If,
  In,
  Nil,
  Or,
  Return,
  Super,
  Self_,
  Static,
  True,
  Let,
  While,
  Try,
  Catch,

  // meta
  Error,
  Eof,
}
