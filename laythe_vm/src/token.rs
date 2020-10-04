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
  Break = 42,
  Continue = 43,
  Super = 44,
  Self_ = 45,
  Static = 46,
  True = 47,
  Let = 48,
  While = 49,
  Try = 50,
  Catch = 51,
  Trait = 52,
  Type = 53,

  // meta
  Error = 54,
  Eof = 55,
}
