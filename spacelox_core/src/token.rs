/// A token in the space lox language
#[derive(Debug, Clone)]
pub struct Token {
  /// The token kind
  pub kind: TokenKind,

  /// The character array of the source
  pub lexeme: String,

  /// line number this token appears
  pub line: u32,
}

/// Token kinds in the space lox language
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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
