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
  LeftParen = 0,
  RightParen = 1,
  LeftBrace = 2,
  RightBrace = 3,
  LeftBracket = 4,
  RightBracket = 5,
  Comma = 6,
  Dot = 7,
  Minus = 8,
  Plus = 9,
  Colon = 10,
  Semicolon = 11,
  Pipe = 12,
  Slash = 13,
  Star = 14,
  Arrow = 15,

  // modules
  Export = 16,
  Import = 17,
  From = 18,

  // logical operators
  Amp = 19,
  Bang = 20,
  BangEqual = 21,
  Equal = 22,
  EqualEqual = 23,
  Greater = 24,
  GreaterEqual = 25,
  Less = 26,
  LessEqual = 27,

  // sequences
  Identifier = 28,
  String = 29,
  Number = 30,

  // keywords
  And = 31,
  Class = 32,
  Else = 33,
  False = 34,
  For = 35,
  Fun = 36,
  If = 37,
  In = 38,
  Nil = 39,
  Or = 40,
  Return = 41,
  Super = 42,
  Self_ = 43,
  Static = 44,
  True = 45,
  Let = 46,
  While = 47,
  Try = 48,
  Catch = 49,
  Trait = 50,
  Type = 51,

  // meta
  Error = 52,
  Eof = 53,
}
