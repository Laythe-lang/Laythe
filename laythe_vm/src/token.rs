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
#[repr(u8)]
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
  PlusEqual,
  MinusEqual,
  SlashEqual,
  StarEqual,
  Arrow,

  // modules
  Export,
  Import,
  From,

  // logical operators
  Amp,
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
  StringStart,
  StringSegment,
  StringEnd,
  Number,

  // keywords
  And,
  Class,
  Else,
  False,
  For,
  Fun,
  If,
  In,
  Nil,
  Or,
  Return,
  Break,
  Continue,
  Super,
  Self_,
  Static,
  True,
  Let,
  While,
  Try,
  Catch,
  Trait,
  Type,

  // meta
  Error,
  Eof,
}
