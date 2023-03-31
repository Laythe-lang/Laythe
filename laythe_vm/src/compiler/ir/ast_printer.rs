use super::{ast::*, token::Token};

#[derive(Default)]
pub struct AstPrint {
  depth: u16,
  buffer: String,
}

impl AstPrint {
  fn reset(&mut self) {
    self.depth = 0;
    self.buffer.clear();
  }

  fn pad(&mut self) {
    for _ in 0..self.depth {
      self.buffer.push_str("  ");
    }
  }

  pub fn str(&self) -> &str {
    &self.buffer
  }
}

impl<'a> Visitor<'a> for AstPrint {
  type Result = ();

  fn visit(&mut self, module: &Module) -> Self::Result {
    self.reset();

    for decl in &module.decls {
      self.visit_decl(decl);
      self.buffer.push('\n')
    }
  }

  fn visit_decl(&mut self, decl: &Decl) -> Self::Result {
    match decl {
      Decl::Symbol(symbol) => self.visit_symbol(symbol),
      Decl::Export(export) => self.visit_export(export),
      Decl::Stmt(stmt) => self.visit_stmt(stmt),
      Decl::Error(error) => self.visit_error(error),
    }
  }

  fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Result {
    match stmt {
      Stmt::Expr(expr) => {
        self.pad();
        self.visit_expr(expr);
        self.buffer.push(';');
      },
      Stmt::ImplicitReturn(expr) => {
        self.pad();
        self.visit_expr(expr);
      },
      Stmt::Import(import) => self.visit_import(import),
      Stmt::For(for_) => self.visit_for(for_),
      Stmt::If(if_) => self.visit_if(if_),
      Stmt::Launch(launch) => self.visit_launch(launch),
      Stmt::Return(return_) => self.visit_return(return_),
      Stmt::Continue(continue_) => self.visit_continue(continue_),
      Stmt::Break(break_) => self.visit_break(break_),
      Stmt::While(while_) => self.visit_while(while_),
      Stmt::Try(try_) => self.visit_try(try_),
      Stmt::Raise(raise) => self.visit_raise(raise),
    }
  }

  fn visit_expr(&mut self, expr: &Expr) -> Self::Result {
    match expr {
      Expr::Assign(assign) => self.visit_assign(assign),
      Expr::Send(drain) => self.visit_drain(drain),
      Expr::AssignBinary(assign_binary) => self.visit_assign_binary(assign_binary),
      Expr::Ternary(ternary) => self.visit_ternary(ternary),
      Expr::Binary(binary) => self.visit_binary(binary),
      Expr::Unary(unary) => self.visit_unary(unary),
      Expr::Atom(atom) => self.visit_atom(atom),
    }
  }

  fn visit_primary(&mut self, primary: &Primary) -> Self::Result {
    match primary {
      Primary::Channel(token) => self.visit_channel(token),
      Primary::True(token) => self.visit_true(token),
      Primary::False(token) => self.visit_false(token),
      Primary::Nil(token) => self.visit_nil(token),
      Primary::Number(token) => self.visit_number(token),
      Primary::Grouping(expr) => {
        self.buffer.push('(');
        self.visit_expr(expr);
        self.buffer.push(')');
      },
      Primary::String(token) => self.visit_string(token),
      Primary::Interpolation(string_interp) => self.visit_interpolation(string_interp),
      Primary::Ident(token) => self.visit_ident(token),
      Primary::InstanceAccess(instance_access) => self.visit_instance_access(instance_access),
      Primary::Self_(token) => self.visit_self(token),
      Primary::Super(token) => self.visit_super(token),
      Primary::Lambda(fun) => self.visit_lambda(fun),
      Primary::List(items) => self.visit_list(items),
      Primary::Tuple(items) => self.visit_tuple(items),
      Primary::Map(kvps) => self.visit_map(kvps),
    }
  }

  fn visit_symbol(&mut self, symbol: &Symbol) -> Self::Result {
    match symbol {
      Symbol::Class(class) => self.visit_class(class),
      Symbol::Fun(fun) => self.visit_fun(fun),
      Symbol::Let(let_) => self.visit_let(let_),
      Symbol::Trait(trait_) => self.visit_trait(trait_),
      Symbol::TypeDecl(type_decl) => self.visit_type_decl(type_decl),
    }
  }
  fn visit_export(&mut self, export: &Symbol) -> Self::Result {
    self.buffer.push_str("export ");
    match &export {
      Symbol::Class(class) => self.visit_class(class),
      Symbol::Fun(fun) => self.visit_fun(fun),
      Symbol::Let(let_) => self.visit_let(let_),
      Symbol::Trait(trait_) => self.visit_trait(trait_),
      Symbol::TypeDecl(type_decl) => self.visit_type_decl(type_decl),
    }
  }

  fn visit_error(&mut self, error: &[Token<'a>]) -> Self::Result {
    for token in error.iter() {
      self.buffer.push_str(&token.str());
      self.buffer.push(' ')
    }
  }

  fn visit_class(&mut self, class: &Class) -> Self::Result {
    self.pad();

    self.buffer.push_str("class ");
    self.buffer.push_str(&class.name.str());

    self.buffer.push_str(" {\n");
    self.depth += 1;

    for member in &class.type_members {
      self.visit_type_member(member);
      self.buffer.push('\n');
    }

    if let Some(init) = &class.init {
      self.visit_method(&init);
      self.buffer.push('\n');
    }

    for method in &class.methods {
      self.visit_method(&method);
      self.buffer.push('\n');
    }

    for static_method in &class.static_methods {
      self.visit_static_method(&static_method);
      self.buffer.push('\n');
    }

    self.depth -= 1;
    self.pad();
    self.buffer.push_str("}\n")
  }

  fn visit_method(&mut self, method: &Fun) -> Self::Result {
    self.pad();

    match &method.name {
      Some(name) => {
        self.buffer.push_str(&name.str());
      },
      None => unreachable!(),
    }

    self.buffer.push('(');
    let len = method.call_sig.params.len();
    for (idx, param) in method.call_sig.params.iter().enumerate() {
      self.buffer.push_str(&param.name.str());

      if let Some(type_) = &param.type_ {
        self.buffer.push_str(": ");
        self.visit_type(type_);
      }

      if idx < len - 1 {
        self.buffer.push_str(", ");
      }
    }

    self.buffer.push_str(") ");
    match &method.body {
      FunBody::Block(block) => self.visit_block(&block),
      FunBody::Expr(expr) => self.visit_expr(&expr),
    };
  }
  fn visit_static_method(&mut self, static_method: &Fun) -> Self::Result {
    self.pad();

    match &static_method.name {
      Some(name) => {
        self.buffer.push_str("static ");
        self.buffer.push_str(&name.str());
      },
      None => unreachable!(),
    }

    self.buffer.push('(');
    let len = static_method.call_sig.params.len();
    for (idx, param) in static_method.call_sig.params.iter().enumerate() {
      self.buffer.push_str(&param.name.str());

      if let Some(type_) = &param.type_ {
        self.visit_type(type_)
      }

      if idx < len - 1 {
        self.buffer.push_str(", ");
      }
    }

    self.buffer.push_str(") ");
    match &static_method.body {
      FunBody::Block(block) => self.visit_block(&block),
      FunBody::Expr(expr) => self.visit_expr(&expr),
    };
  }

  fn visit_fun(&mut self, fun: &Fun) -> Self::Result {
    self.pad();

    match &fun.name {
      Some(name) => {
        self.buffer.push_str("fn ");
        self.buffer.push_str(&name.str());
      },
      None => self.buffer.push_str("fn"),
    }

    self.visit_call_sig(&fun.call_sig);
    self.buffer.push(' ');

    match &fun.body {
      FunBody::Block(block) => self.visit_block(&block),
      FunBody::Expr(expr) => self.visit_expr(&expr),
    };
  }
  fn visit_let(&mut self, let_: &Let) -> Self::Result {
    self.pad();
    self.buffer.push_str("let ");
    self.buffer.push_str(&let_.name.str());

    if let Some(type_) = &let_.type_ {
      self.buffer.push_str(": ");
      self.visit_type(type_);
    }

    match &let_.value {
      Some(v) => {
        self.buffer.push_str(" = ");
        self.visit_expr(&v);
        self.buffer.push(';');
      },
      None => self.buffer.push(';'),
    }
  }

  fn visit_import(&mut self, import: &Import) -> Self::Result {
    self.pad();
    self.buffer.push_str("import ");
    match import.path.split_last() {
      Some((last, start)) => {
        for segment in start {
          self.buffer.push_str(segment.str());
          self.buffer.push('.');
        }
        self.buffer.push_str(last.str());
      },
      None => self.buffer.push_str(import.path[0].str()),
    }

    fn visit_rename(printer: &mut AstPrint, import_symbol: &ImportSymbol) {
      printer.buffer.push_str(import_symbol.symbol.str());

      if let Some(rename) = &import_symbol.rename {
        printer.buffer.push_str(" as ");
        printer.buffer.push_str(rename.str())
      }
    }

    match &import.stem {
      ImportStem::None => {},
      ImportStem::Rename(rename) => {
        self.buffer.push_str(" as ");
        self.buffer.push_str(rename.str());
      },
      ImportStem::Symbols(symbols) => {
        self.buffer.push('{');
        match symbols.split_last() {
          Some((last, start)) => {
            for symbol in start {
              visit_rename(self, symbol);
              self.buffer.push(',');
            }

            visit_rename(self, last);
          },
          None => visit_rename(self, &symbols[0]),
        }

        self.buffer.push('}');
      },
    }

    self.buffer.push_str(";");
  }

  fn visit_for(&mut self, for_: &For) -> Self::Result {
    self.pad();
    self.buffer.push_str("for ");
    self.buffer.push_str(&for_.item.str());
    self.buffer.push_str(" in ");
    self.visit_expr(&for_.iter);
    self.visit_block(&for_.body);
  }

  fn visit_while(&mut self, while_: &While) -> Self::Result {
    self.pad();
    self.buffer.push_str("while ");
    self.visit_expr(&while_.cond);
    self.visit_block(&while_.body);
  }
  fn visit_if(&mut self, if_: &If) -> Self::Result {
    self.pad();
    self.buffer.push_str("if ");

    self.visit_expr(&if_.cond);
    self.visit_block(&if_.body);

    if let Some(else_) = &if_.else_ {
      self.buffer.push_str(" else ");
      match else_ {
        Else::If(if_) => self.visit_if(if_),
        Else::Block(block) => self.visit_block(block),
      }
    }
  }

  fn visit_launch(&mut self, launch: &Launch) -> Self::Result {
    self.pad();
    self.buffer.push_str("launch ");
    self.visit_expr(&launch.closure);
    self.buffer.push(';')
  }

  fn visit_return(&mut self, return_: &Return) -> Self::Result {
    self.pad();
    match &return_.value {
      Some(v) => {
        self.buffer.push_str("return ");
        self.visit_expr(&v);
        self.buffer.push(';');
      },
      None => self.buffer.push_str("return;"),
    }
  }
  fn visit_continue(&mut self, _: &Token) -> Self::Result {
    self.pad();
    self.buffer.push_str("continue;")
  }

  fn visit_break(&mut self, _: &Token) -> Self::Result {
    self.pad();
    self.buffer.push_str("break;")
  }
  fn visit_try(&mut self, try_: &Try) -> Self::Result {
    self.pad();
    self.buffer.push_str("try ");

    self.visit_block(&try_.block);

    self.buffer.push_str(" catch ");
    self.visit_block(&try_.catch);
  }

  fn visit_raise(&mut self, raise: &Raise) -> Self::Result {
    self.pad();
    self.buffer.push_str("raise ");

    self.visit_expr(&raise.error);

    self.buffer.push(';');
  }

  fn visit_block(&mut self, block: &Block) -> Self::Result {
    self.buffer.push_str("{\n");
    self.depth += 1;

    for decl in &block.decls {
      self.visit_decl(&decl);
      self.buffer.push('\n');
    }

    self.depth -= 1;
    self.pad();
    self.buffer.push_str("}");
  }
  fn visit_assign(&mut self, assign: &Assign) -> Self::Result {
    self.visit_expr(&assign.lhs);
    self.buffer.push_str(" = ");
    self.visit_expr(&assign.rhs);
  }

  fn visit_drain(&mut self, assign: &Send) -> Self::Result {
    self.visit_expr(&assign.lhs);
    self.buffer.push_str(" <- ");
    self.visit_expr(&assign.rhs);
  }

  fn visit_assign_binary(&mut self, assign_binary: &AssignBinary) -> Self::Result {
    self.visit_expr(&assign_binary.lhs);
    self.buffer.push(' ');
    match &assign_binary.op {
      AssignBinaryOp::Add => self.buffer.push_str("+="),
      AssignBinaryOp::Sub => self.buffer.push_str("-="),
      AssignBinaryOp::Mul => self.buffer.push_str("*="),
      AssignBinaryOp::Div => self.buffer.push_str("/="),
    }
    self.buffer.push(' ');
    self.visit_expr(&assign_binary.rhs);
  }
  fn visit_ternary(&mut self, ternary: &Ternary) -> Self::Result {
    self.visit_expr(&ternary.cond);
    self.buffer.push_str(" ? ");
    self.visit_expr(&ternary.then);
    self.buffer.push_str(" : ");
    self.visit_expr(&ternary.else_);
  }
  fn visit_binary(&mut self, binary: &Binary) -> Self::Result {
    self.visit_expr(&binary.lhs);

    self.buffer.push(' ');
    match &binary.op {
      BinaryOp::Add => self.buffer.push('+'),
      BinaryOp::Sub => self.buffer.push('-'),
      BinaryOp::Mul => self.buffer.push('*'),
      BinaryOp::Div => self.buffer.push('/'),
      BinaryOp::Lt => self.buffer.push('<'),
      BinaryOp::LtEq => self.buffer.push_str("<="),
      BinaryOp::Gt => self.buffer.push('>'),
      BinaryOp::GtEq => self.buffer.push_str(">="),
      BinaryOp::Eq => self.buffer.push_str("=="),
      BinaryOp::Ne => self.buffer.push_str("!="),
      BinaryOp::And => self.buffer.push_str("&&"),
      BinaryOp::Or => self.buffer.push_str("||"),
    }
    self.buffer.push(' ');
    self.visit_expr(&binary.rhs);
  }
  fn visit_unary(&mut self, unary: &Unary) -> Self::Result {
    match &unary.op {
      UnaryOp::Not => self.buffer.push('!'),
      UnaryOp::Receive => self.buffer.push_str("<- "),
      UnaryOp::Negate => self.buffer.push('-'),
    }

    self.visit_expr(&unary.expr)
  }
  fn visit_call(&mut self, call: &Call) -> Self::Result {
    self.buffer.push('(');
    let len = call.args.len();
    for (idx, arg) in call.args.iter().enumerate() {
      self.visit_expr(&arg);

      if idx < len - 1 {
        self.buffer.push_str(", ");
      }
    }

    self.buffer.push_str(")");
  }

  fn visit_index(&mut self, index: &Index) -> Self::Result {
    self.buffer.push('[');
    self.visit_expr(&index.index);
    self.buffer.push(']');
  }
  fn visit_access(&mut self, access: &Access) -> Self::Result {
    self.buffer.push('.');
    self.buffer.push_str(&access.prop.str());
  }

  fn visit_call_sig(&mut self, call_sig: &CallSignature) -> Self::Result {
    self.visit_type_params(&call_sig.type_params);
    self.buffer.push('(');
    let len = call_sig.params.len();

    for (idx, param) in call_sig.params.iter().enumerate() {
      self.buffer.push_str(param.name.str());
      if let Some(type_) = &param.type_ {
        self.buffer.push_str(": ");
        self.visit_type(type_);
      }

      if idx < len - 1 {
        self.buffer.push_str(", ");
      }
    }

    self.buffer.push(')');
    if let Some(type_) = &call_sig.return_type {
      self.buffer.push_str(" -> ");
      self.visit_type(type_)
    }
  }
  fn visit_atom(&mut self, atom: &Atom) -> Self::Result {
    self.visit_primary(&atom.primary);

    for trailer in atom.trailers.iter() {
      match trailer {
        Trailer::Call(call) => self.visit_call(&call),
        Trailer::Index(index) => self.visit_index(&index),
        Trailer::Access(access) => self.visit_access(&access),
      }
    }
  }
  fn visit_true(&mut self, _: &Token) -> Self::Result {
    self.buffer.push_str("true");
  }
  fn visit_false(&mut self, _: &Token) -> Self::Result {
    self.buffer.push_str("false");
  }
  fn visit_nil(&mut self, _: &Token) -> Self::Result {
    self.buffer.push_str("nil");
  }
  fn visit_number(&mut self, token: &Token) -> Self::Result {
    self.buffer.push_str(&token.str());
  }
  fn visit_string(&mut self, token: &Token) -> Self::Result {
    self.buffer.push('"');
    self.buffer.push_str(&token.str());
    self.buffer.push('"');
  }
  fn visit_channel(&mut self, channel: &Channel) -> Self::Result {
    self.buffer.push_str("chan(");
    if let Some(expr) = &channel.expr {
      self.visit_expr(expr)
    }
    self.buffer.push(')');
  }
  fn visit_interpolation(&mut self, string_interp: &Interpolation) -> Self::Result {
    self.buffer.push('"');
    self.buffer.push_str(&string_interp.start.str());
    self.buffer.push_str("${");

    for segment in string_interp.segments.iter() {
      match segment {
        StringSegments::Token(segment) => {
          self.buffer.push_str("${");
          self.buffer.push_str(&segment.str());
          self.buffer.push('}');
        },
        StringSegments::Expr(expr) => self.visit_expr(&expr),
      }
    }

    self.buffer.push('}');
    self.buffer.push_str(&string_interp.end.str());
    self.buffer.push('"');
  }

  fn visit_ident(&mut self, token: &Token) -> Self::Result {
    self.buffer.push_str(&token.str());
  }
  fn visit_instance_access(&mut self, instance_access: &InstanceAccess) -> Self::Result {
    self.buffer.push_str(&instance_access.access.str());
  }
  fn visit_self(&mut self, _: &Token) -> Self::Result {
    self.buffer.push_str("self");
  }
  fn visit_super(&mut self, super_: &Super) -> Self::Result {
    self.buffer.push_str("super.");
    self.buffer.push_str(&super_.access.str());
  }
  fn visit_lambda(&mut self, fun: &Fun) -> Self::Result {
    self.buffer.push('|');
    let len = fun.call_sig.params.len();
    for (idx, param) in fun.call_sig.params.iter().enumerate() {
      self.buffer.push_str(&param.name.str());

      if let Some(type_) = &param.type_ {
        self.visit_type(type_);
      }

      if idx < len - 1 {
        self.buffer.push_str(", ");
      }
    }

    self.buffer.push_str("| ");
    match &fun.body {
      FunBody::Block(block) => self.visit_block(&block),
      FunBody::Expr(expr) => self.visit_expr(&expr),
    };
  }
  fn visit_list(&mut self, list: &Collection) -> Self::Result {
    self.buffer.push('[');
    let len = list.items.len();
    for (idx, arg) in list.items.iter().enumerate() {
      self.visit_expr(&arg);

      if idx < len - 1 {
        self.buffer.push_str(", ");
      }
    }

    self.buffer.push(']');
  }
  fn visit_tuple(&mut self, list: &Collection) -> Self::Result {
    self.buffer.push('(');
    let len = list.items.len();
    for (idx, arg) in list.items.iter().enumerate() {
      self.visit_expr(&arg);

      if idx < len - 1 {
        self.buffer.push_str(", ");
      }
    }

    self.buffer.push(')');
  }
  fn visit_map(&mut self, map: &Map) -> Self::Result {
    self.buffer.push('{');
    let len = map.entries.len();
    for (idx, (key, value)) in map.entries.iter().enumerate() {
      self.visit_expr(&key);
      self.buffer.push_str(": ");
      self.visit_expr(&value);

      if idx < len - 1 {
        self.buffer.push_str(", ");
      }
    }

    self.buffer.push('}');
  }
}

impl TypeVisitor for AstPrint {
  type Result = ();

  fn visit_trait(&mut self, trait_: &Trait) -> Self::Result {
    self.pad();

    self.buffer.push_str("trait ");
    self.buffer.push_str(trait_.name.str());
    self.visit_type_params(&trait_.params);

    self.buffer.push_str(" {\n");

    self.depth += 1;

    for member in trait_.members.iter() {
      self.visit_type_member(member);
    }

    self.depth -= 1;

    self.pad();
    self.buffer.push_str("}\n");
  }

  fn visit_type_decl(&mut self, type_decl: &TypeDecl) -> Self::Result {
    self.pad();

    self.buffer.push_str("type ");
    self.buffer.push_str(type_decl.name.str());
    self.visit_type_params(&type_decl.type_params);

    self.buffer.push_str(" = ");
    self.visit_type(&type_decl.type_);
    self.buffer.push_str(";");
  }

  fn visit_type(&mut self, type_: &Type) -> Self::Result {
    match type_ {
      Type::Union(union) => self.visit_union(union),
      Type::Intersection(intersection) => self.visit_intersection(intersection),
      Type::Fun(call_sig) => self.visit_call_sig(call_sig),
      Type::List(list_type) => self.visit_list_type(list_type),
      Type::Ref(type_ref) => self.visit_type_ref(type_ref),
      Type::Primitive(primitive) => self.visit_primitive(primitive),
    }
  }

  fn visit_union(&mut self, union: &Union) -> Self::Result {
    self.visit_type(&union.lhs);
    self.buffer.push_str(" | ");
    self.visit_type(&union.rhs);
  }

  fn visit_intersection(&mut self, intersection: &Intersection) -> Self::Result {
    self.visit_type(&intersection.lhs);
    self.buffer.push_str(" & ");
    self.visit_type(&intersection.rhs);
  }

  fn visit_primitive(&mut self, primitive: &Primitive) -> Self::Result {
    match primitive {
      Primitive::Nil(_) => self.buffer.push_str("nil"),
      Primitive::Number(_) => self.buffer.push_str("number"),
      Primitive::Bool(_) => self.buffer.push_str("bool"),
      Primitive::String(_) => self.buffer.push_str("string"),
      Primitive::Any(_) => self.buffer.push_str("any"),
    }
  }

  fn visit_type_params(&mut self, type_params: &[TypeParam]) -> Self::Result {
    let len = type_params.len();
    if len == 0 {
      return;
    }

    self.buffer.push('<');
    for (idx, type_param) in type_params.iter().enumerate() {
      self.buffer.push_str(type_param.name.str());
      if let Some(constraint) = &type_param.constraint {
        self.buffer.push_str(": ");
        self.visit_type(&constraint);
      }

      if idx < len - 1 {
        self.buffer.push_str(", ");
      }
    }

    self.buffer.push('>');
  }

  fn visit_type_member(&mut self, type_member: &TypeMember) -> Self::Result {
    self.pad();
    self.buffer.push_str(type_member.name.str());
    self.buffer.push_str(": ");
    self.visit_type(&type_member.type_);
    self.buffer.push(';');
  }

  fn visit_type_method(&mut self, type_member: &TypeMethod) -> Self::Result {
    self.pad();
    self.buffer.push_str(type_member.name.str());
    self.visit_call_sig(&type_member.call_sig);
  }

  fn visit_type_ref(&mut self, type_ref: &TypeRef) -> Self::Result {
    self.buffer.push_str(type_ref.name.str());

    if type_ref.type_args.len() > 0 {
      self.buffer.push('<');
      let len = type_ref.type_args.len();

      for (idx, arg) in type_ref.type_args.iter().enumerate() {
        self.visit_type(arg);

        if idx < len - 1 {
          self.buffer.push_str(", ");
        }
      }

      self.buffer.push('>');
    }
  }

  fn visit_list_type(&mut self, list_type: &ListType) -> Self::Result {
    self.visit_type(&list_type.item_type);
    self.buffer.push_str("[]");
  }
}
