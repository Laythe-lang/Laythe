use crate::scanner::{Scanner, TokenKind};

pub fn compile(source: &str) {
  let mut scanner = Scanner::new(source);
  let mut line: i32 = -1;

  loop {
    let token = scanner.scan_token();

    if token.line != line {
      print!("{:0>4} ", token.line);
      line = token.line;
    } else {
      print!("   | ");
    }
    println!("{:?} {}", token.kind, token.start);

    if token.kind == TokenKind::Eof {
      break;
    }
  }
}