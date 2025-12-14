use std::collections::HashMap;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::thread;
use std::time::Duration;

// ===== Values =====

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
}

impl Value {
    /// Unwrap integer inside a Value
    pub fn unwrap_int(&self) -> i64 {
        match self {
            Value::Int(n) => *n,
            _ => panic!("not an int"),
        }
    }
    #[allow(unused)]
    /// Unwrap boolean inside a Value
    pub fn unwrap_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("not a bool"),
        }
    }
}

// ===== Expressions =====

#[derive(Debug, Clone)]
enum Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

// ===== Statements =====

#[derive(Debug, Clone)]
enum Stmt {
    Assign { name: String, expr: Expr },
    Jump { label: String },
    IfJump { cond: Expr, label: String },
    Print { name: String },
    Sleep { expr: Expr },
}

// ===== Lexer =====

#[derive(Debug, Clone)]
enum Token {
    Int(i64),
    Ident(String),
    Bool(bool),
    Op(String),
    LParen,
    RParen,
    Eof,
}

struct Lexer<'a> {
    chars: Vec<char>,
    pos: usize,
    _src: &'a str,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            chars: src.chars().collect(),
            pos: 0,
            _src: src,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += 1;
        Some(c)
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek(), Some(c) if c.is_whitespace()) {
            self.bump();
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_ws();
        let ch = match self.peek() {
            Some(c) => c,
            None => return Token::Eof,
        };

        if ch.is_ascii_digit() {
            let mut n = 0i64;
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.bump();
                    n = n * 10 + (c as i64 - '0' as i64);
                } else {
                    break;
                }
            }
            return Token::Int(n);
        }

        if ch.is_ascii_alphabetic() || ch == '_' {
            let mut s = String::new();
            while let Some(c) = self.peek() {
                if c.is_ascii_alphanumeric() || c == '_' {
                    self.bump();
                    s.push(c);
                } else {
                    break;
                }
            }
            return match s.as_str() {
                "true" => Token::Bool(true),
                "false" => Token::Bool(false),
                _ => Token::Ident(s),
            };
        }

        match ch {
            '(' => {
                self.bump();
                Token::LParen
            }
            ')' => {
                self.bump();
                Token::RParen
            }
            '+' | '-' | '*' | '/' | '<' | '>' | '=' | '!' => {
                let mut op = String::new();
                op.push(ch);
                self.bump();
                if let Some(next) = self.peek() {
                    if (ch == '<' || ch == '>' || ch == '=' || ch == '!') && next == '=' {
                        op.push(next);
                        self.bump();
                    }
                }
                Token::Op(op)
            }
            _ => {
                self.bump();
                Token::Op(ch.to_string())
            }
        }
    }
}

// ===== Pratt parser =====

struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Self {
        let mut lex = Lexer::new(src);
        let first = lex.next_token();
        Self {
            lexer: lex,
            current: first,
        }
    }

    fn bump(&mut self) {
        self.current = self.lexer.next_token();
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_precedence(0)
    }

    fn lbp(tok: &Token) -> u8 {
        match tok {
            Token::Op(op) => match op.as_str() {
                "==" | "!=" | "<" | "<=" | ">" | ">=" => 1,
                "+" | "-" => 2,
                "*" | "/" => 3,
                _ => 0,
            },
            _ => 0,
        }
    }

    fn parse_precedence(&mut self, min_bp: u8) -> Result<Expr, String> {
        let mut lhs = match &self.current {
            Token::Int(n) => {
                let x = *n;
                self.bump();
                Expr::Int(x)
            }
            Token::Bool(b) => {
                let x = *b;
                self.bump();
                Expr::Bool(x)
            }
            Token::Ident(s) => {
                let x = s.clone();
                self.bump();
                Expr::Var(x)
            }
            Token::Op(op) if op == "-" => {
                self.bump();
                let rhs = self.parse_precedence(4)?;
                Expr::Unary(UnaryOp::Neg, Box::new(rhs))
            }
            Token::Op(op) if op == "!" => {
                self.bump();
                let rhs = self.parse_precedence(4)?;
                Expr::Unary(UnaryOp::Not, Box::new(rhs))
            }
            Token::LParen => {
                self.bump();
                let e = self.parse_expr()?;
                match self.current {
                    Token::RParen => self.bump(),
                    _ => return Err("expected ')'".into()),
                }
                e
            }
            _ => return Err("unexpected token in expression".into()),
        };

        loop {
            let op_token = match &self.current {
                Token::Op(_) => self.current.clone(),
                _ => break,
            };

            let bp = Self::lbp(&op_token);
            if bp < min_bp {
                break;
            }

            let op = match op_token {
                Token::Op(ref s) => match s.as_str() {
                    "+" => BinOp::Add,
                    "-" => BinOp::Sub,
                    "*" => BinOp::Mul,
                    "/" => BinOp::Div,
                    "<" => BinOp::Lt,
                    "<=" => BinOp::Le,
                    ">" => BinOp::Gt,
                    ">=" => BinOp::Ge,
                    "==" => BinOp::Eq,
                    "!=" => BinOp::Ne,
                    _ => break,
                },
                _ => break,
            };

            self.bump();
            let rhs = self.parse_precedence(bp + 1)?;
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
        }

        Ok(lhs)
    }
}

// ===== Interpreter =====

pub struct Interpreter {
    stmts: Vec<Stmt>,
    labels: HashMap<String, usize>,
    vars: RwLock<HashMap<String, Value>>, // <- thread-safe vars
}

impl Interpreter {
    pub fn from_source(source: &str) -> Result<Self, String> {
        let mut stmts = Vec::new();
        let mut labels = HashMap::new();

        for raw_line in source.lines() {
            // 1) Strip line comments starting with ';'
            let code_part = match raw_line.find(';') {
                Some(idx) => &raw_line[..idx],
                None => raw_line,
            };

            // 2) Trim whitespace
            let line = code_part.trim();

            // 3) Skip empty (or fully-comment) lines
            if line.is_empty() {
                continue;
            }

            // label
            if line.starts_with('.') {
                let name = line.trim_start_matches('.').trim().to_string();
                labels.insert(name, stmts.len());
                continue;
            }

            // sleep
            if line.starts_with("sleep ") {
                let expr_src = &line["sleep ".len()..];
                let mut parser = Parser::new(expr_src);
                let expr = parser.parse_expr()?;
                stmts.push(Stmt::Sleep { expr });
                continue;
            }

            // print
            if line.starts_with("print ") {
                let name = line["print ".len()..].trim().to_string();
                stmts.push(Stmt::Print { name });
                continue;
            }

            // if cond jump .label
            if line.starts_with("if ") {
                let rest = line["if ".len()..].trim();
                let parts: Vec<&str> = rest.splitn(2, " jump ").collect();
                if parts.len() != 2 {
                    return Err("invalid if-jump".into());
                }

                let cond_src = parts[0].trim();
                let label = parts[1].trim().trim_start_matches('.').to_string();

                let mut parser = Parser::new(cond_src);
                let expr = parser.parse_expr()?;
                stmts.push(Stmt::IfJump { cond: expr, label });
                continue;
            }

            // jump .label
            if line.starts_with("jump ") {
                let label = line["jump ".len()..]
                    .trim()
                    .trim_start_matches('.')
                    .to_string();
                stmts.push(Stmt::Jump { label });
                continue;
            }

            // assignment: name = expr
            if let Some(eq) = line.find('=') {
                let (lhs, rhs) = line.split_at(eq);
                let name = lhs.trim().to_string();
                let mut parser = Parser::new(rhs[1..].trim());
                let expr = parser.parse_expr()?;
                stmts.push(Stmt::Assign { name, expr });
                continue;
            }

            return Err(format!("unknown statement: {line}"));
        }

        Ok(Self {
            stmts,
            labels,
            vars: RwLock::new(HashMap::new()),
        })
    }

    pub fn from_file(path: &str) -> Result<Self, String> {
        use std::fs;

        let src = fs::read_to_string(path)
            .map_err(|e| format!("failed to read file '{}': {}", path, e))?;

        Self::from_source(&src)
    }

    #[allow(unused)]
    pub fn run(&mut self) -> Result<(), String> {
        let mut ip: isize = 0;

        while ip >= 0 && (ip as usize) < self.stmts.len() {
            match &self.stmts[ip as usize] {
                Stmt::Assign { name, expr } => {
                    let v = self.eval_expr(expr)?;
                    self.set_var(name, v);
                    ip += 1;
                }

                Stmt::Jump { label } => {
                    ip = self.lookup_label(label)?;
                }

                Stmt::IfJump { cond, label } => {
                    let v = self.eval_expr(cond)?;
                    match v {
                        Value::Bool(true) => ip = self.lookup_label(label)?,
                        Value::Bool(false) => ip += 1,
                        _ => return Err("if condition must be bool".into()),
                    }
                }

                Stmt::Print { name } => {
                    if let Some(val) = self.get_var(name) {
                        match val {
                            Value::Int(n) => println!("{}", n),
                            Value::Bool(b) => println!("{}", b),
                        }
                    } else {
                        return Err(format!("undefined variable '{}'", name));
                    }
                    ip += 1;
                }

                Stmt::Sleep { expr } => {
                    let v = self.eval_expr(expr)?;
                    let ms = match v {
                        Value::Int(n) => n,
                        _ => return Err("sleep requires integer".into()),
                    };
                    thread::sleep(Duration::from_millis(ms as u64));
                    ip += 1;
                }
            }
        }

        Ok(())
    }

    fn lookup_label(&self, label: &str) -> Result<isize, String> {
        self.labels
            .get(label)
            .map(|&i| i as isize)
            .ok_or_else(|| format!("unknown label '{label}'"))
    }

    fn eval_expr(&self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Int(n) => Ok(Value::Int(*n)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Var(name) => self
                .get_var(name)
                .ok_or_else(|| format!("undefined variable '{name}'")),
            Expr::Unary(op, rhs) => {
                let v = self.eval_expr(rhs)?;
                match (op, v) {
                    (UnaryOp::Neg, Value::Int(x)) => Ok(Value::Int(-x)),
                    (UnaryOp::Not, Value::Bool(x)) => Ok(Value::Bool(!x)),
                    _ => Err("invalid unary operand".into()),
                }
            }
            Expr::Binary(a, op, b) => {
                let lv = self.eval_expr(a)?;
                let rv = self.eval_expr(b)?;

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        let (x, y) = match (lv, rv) {
                            (Value::Int(a), Value::Int(b)) => (a, b),
                            _ => return Err("arithmetic requires ints".into()),
                        };
                        Ok(Value::Int(match op {
                            BinOp::Add => x + y,
                            BinOp::Sub => x - y,
                            BinOp::Mul => x * y,
                            BinOp::Div => x / y,
                            _ => unreachable!(),
                        }))
                    }

                    BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        let (x, y) = match (lv, rv) {
                            (Value::Int(a), Value::Int(b)) => (a, b),
                            _ => return Err("comparison requires ints".into()),
                        };
                        Ok(Value::Bool(match op {
                            BinOp::Lt => x < y,
                            BinOp::Le => x <= y,
                            BinOp::Gt => x > y,
                            BinOp::Ge => x >= y,
                            _ => unreachable!(),
                        }))
                    }

                    BinOp::Eq | BinOp::Ne => match (lv, rv) {
                        (Value::Int(a), Value::Int(b)) => {
                            Ok(Value::Bool(if matches!(op, BinOp::Eq) {
                                a == b
                            } else {
                                a != b
                            }))
                        }
                        (Value::Bool(a), Value::Bool(b)) => {
                            Ok(Value::Bool(if matches!(op, BinOp::Eq) {
                                a == b
                            } else {
                                a != b
                            }))
                        }
                        _ => Err("==/!= requires same type".into()),
                    },
                }
            }
        }
    }

    // ===== Thread-safe external accessors =====

    /// Read-only guard for all variables
    #[allow(unused)]
    pub fn vars(&self) -> RwLockReadGuard<'_, HashMap<String, Value>> {
        self.vars.read().unwrap()
    }

    /// Writable guard for all variables (use with care)
    #[allow(unused)]
    pub fn vars_mut(&self) -> RwLockWriteGuard<'_, HashMap<String, Value>> {
        self.vars.write().unwrap()
    }

    /// Get a single variable by name (cloned)
    pub fn get_var(&self, name: &str) -> Option<Value> {
        let guard = self.vars.read().unwrap();
        guard.get(name).cloned()
    }

    /// Set or override a variable
    pub fn set_var(&self, name: &str, value: Value) {
        let mut guard = self.vars.write().unwrap();
        guard.insert(name.to_string(), value);
    }

    /// Run the interpreter in its own thread, using an Arc<Interpreter>.
    /// The interpreter runs to completion on that thread.
    pub fn run_in_thread(interp: Arc<Interpreter>) -> thread::JoinHandle<Result<(), String>> {
        thread::spawn(move || {
            let mut ip: isize = 0;

            // Same logic as your `run` method, but using the Arc<Interpreter>
            while ip >= 0 && (ip as usize) < interp.stmts.len() {
                match &interp.stmts[ip as usize] {
                    Stmt::Assign { name, expr } => {
                        let v = interp.eval_expr(expr)?;
                        interp.set_var(name, v);
                        ip += 1;
                    }

                    Stmt::Jump { label } => {
                        ip = interp.lookup_label(label)?;
                    }

                    Stmt::IfJump { cond, label } => {
                        let v = interp.eval_expr(cond)?;
                        match v {
                            Value::Bool(true) => ip = interp.lookup_label(label)?,
                            Value::Bool(false) => ip += 1,
                            _ => return Err("if condition must be bool".into()),
                        }
                    }

                    Stmt::Print { name } => {
                        if let Some(val) = interp.get_var(name) {
                            match val {
                                Value::Int(n) => println!("{}", n),
                                Value::Bool(b) => println!("{}", b),
                            }
                        } else {
                            return Err(format!("undefined variable '{}'", name));
                        }
                        ip += 1;
                    }

                    Stmt::Sleep { expr } => {
                        let v = interp.eval_expr(expr)?;
                        let ms = match v {
                            Value::Int(n) => n,
                            _ => return Err("sleep requires integer".into()),
                        };
                        thread::sleep(Duration::from_millis(ms as u64));
                        ip += 1;
                    }
                }
            }

            Ok(())
        })
    }
}
