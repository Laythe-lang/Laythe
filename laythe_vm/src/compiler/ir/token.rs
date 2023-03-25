use super::ast::Spanned;
use std::fmt;
use variant_count::VariantCount;

#[derive(Debug, Clone)]
pub enum Lexeme<'a> {
  Slice(&'a str),
  Owned(String),
}

/// A token in the Laythe language
#[derive(Debug, Clone)]
pub struct Token<'a> {
  /// The token kind
  kind: TokenKind,

  /// The character array of the source
  lexeme: Lexeme<'a>,

  /// the start offset of this token
  start: u32,

  /// The end offset of this token
  end: u32,
}

impl<'a> Token<'a> {
  pub const fn new(kind: TokenKind, lexeme: Lexeme<'a>, start: u32, end: u32) -> Self {
    Self {
      kind,
      lexeme,
      start,
      end,
    }
  }

  #[inline]
  pub fn str(&'a self) -> &'a str {
    match &self.lexeme {
      Lexeme::Slice(slice) => slice,
      Lexeme::Owned(string) => string,
    }
  }

  #[inline]
  pub fn kind(&self) -> TokenKind {
    self.kind
  }
}

impl<'a> Spanned for Token<'a> {
  fn start(&self) -> u32 {
    self.start
  }

  fn end(&self) -> u32 {
    self.end
  }
}

/// Token kinds in the Laythe language
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, VariantCount)]
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
  QuestionMark,
  Colon,
  Semicolon,
  Pipe,
  Slash,
  Star,
  PlusEqual,
  MinusEqual,
  SlashEqual,
  StarEqual,
  RightArrow,
  LeftArrow,

  // modules
  Export,
  Import,
  As,

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
  InstanceAccess,
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
  Raise,
  Trait,
  Type,
  Channel,
  Launch,

  // meta
  Error,
  Eof,
}

impl fmt::Display for TokenKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      TokenKind::LeftParen => "(",
      TokenKind::RightParen => ")",
      TokenKind::LeftBrace => "{",
      TokenKind::RightBrace => "}",
      TokenKind::LeftBracket => "[",
      TokenKind::RightBracket => "]",
      TokenKind::Comma => ",",
      TokenKind::Dot => ".",
      TokenKind::Minus => "-",
      TokenKind::Plus => "+",
      TokenKind::QuestionMark => "?",
      TokenKind::Colon => ":",
      TokenKind::Semicolon => ";",
      TokenKind::Pipe => "|",
      TokenKind::Slash => "/",
      TokenKind::Star => "*",
      TokenKind::PlusEqual => "+=",
      TokenKind::MinusEqual => "-=",
      TokenKind::SlashEqual => "/=",
      TokenKind::StarEqual => "*=",
      TokenKind::RightArrow => "->",
      TokenKind::LeftArrow => "<-",
      TokenKind::Export => "export",
      TokenKind::Import => "import",
      TokenKind::As => "as",
      TokenKind::Amp => "&",
      TokenKind::Bang => "!",
      TokenKind::BangEqual => "!=",
      TokenKind::Equal => "=",
      TokenKind::EqualEqual => "==",
      TokenKind::Greater => ">",
      TokenKind::GreaterEqual => ">=",
      TokenKind::Less => "<",
      TokenKind::LessEqual => "<=",
      TokenKind::Identifier => "identifier",
      TokenKind::InstanceAccess => "instance access",
      TokenKind::String => "string",
      TokenKind::StringStart => "string start",
      TokenKind::StringSegment => "string segment",
      TokenKind::StringEnd => "string end",
      TokenKind::Number => "number",
      TokenKind::And => "and",
      TokenKind::Class => "class",
      TokenKind::Else => "else",
      TokenKind::False => "false",
      TokenKind::For => "for",
      TokenKind::Fun => "fn",
      TokenKind::If => "if",
      TokenKind::In => "in",
      TokenKind::Nil => "nil",
      TokenKind::Or => "or",
      TokenKind::Return => "return",
      TokenKind::Break => "break",
      TokenKind::Continue => "continue",
      TokenKind::Super => "super",
      TokenKind::Self_ => "self",
      TokenKind::Static => "static",
      TokenKind::True => "true",
      TokenKind::Let => "let",
      TokenKind::While => "while",
      TokenKind::Try => "try",
      TokenKind::Catch => "catch",
      TokenKind::Raise => "raise",
      TokenKind::Trait => "trait",
      TokenKind::Type => "type",
      TokenKind::Channel => "channel",
      TokenKind::Launch => "launch",
      TokenKind::Error => "error",
      TokenKind::Eof => "eof",
    })
  }
}
