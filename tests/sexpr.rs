#[derive(Debug, Clone, PartialEq, Eq)]
enum SExprToken {
    StrLit(String),
    IntLit(i64),
    Ident(String),
    OpenParen,
    CloseParen,
}

struct SExprLexer<'a> {
    s: &'a str
}

impl Iterator for SExprLexer<'_> {
    type Item = SExprToken;

    fn next(&mut self) -> Option<Self::Item> {
        while self.s.chars().next().unwrap().is_whitespace() {
            let mut indices = self.s.char_indices();
            indices.next();
            self.s = &self.s[indices.next().unwrap().0..];
        }

        if self.s.is_empty() {
            None
        } else if self.s.starts_with('(') {
            self.s = &self.s[1..];
            Some(SExprToken::OpenParen)
        } else if self.s.starts_with(')') {
            self.s = &self.s[1..];
            Some(SExprToken::CloseParen)
        } else if self.s.starts_with('"') {
            self.s = &self.s[1..];
            let mut end_idx = None;
            for (idx, c) in self.s.char_indices() {
                if c == '\\' {
                    todo!()
                } else if c == '"' {
                    end_idx = Some(idx);
                    break
                }
            }
            let end_idx = end_idx.unwrap();
            let name = self.s[..end_idx].to_string();
            self.s = &self.s[end_idx+1..];
            Some(SExprToken::StrLit(name))
        } else if self.s.starts_with('-') || self.s.chars().next().unwrap().is_digit(10) {
            let is_neg = if self.s.starts_with('-') {
                self.s = &self.s[1..];
                true
            } else {
                false
            };

            let radix = if self.s.starts_with("0x") || self.s.starts_with("0X") {
                self.s = &self.s[2..];
                16
            } else if self.s.starts_with("0b") || self.s.starts_with("0B") {
                self.s = &self.s[2..];
                2
            // TODO: ???
            /*
            } else if self.s.starts_with('0')  {
                    todo!("octal?")
            */
            } else {
                10
            };

            let mut s = String::new();

            loop {
                let c = self.s.chars().next().unwrap();
                if c.is_digit(radix) {
                s.push(c);
                } else if c == '_' {
                // Ignore 
                } else {
                break;
                }

                self.s = &self.s[1..];
            };

            let mut value = u64::from_str_radix(&s, radix).unwrap_or_else(|e| panic!("Failed to parse {} because {}", &s, e)) as i64;
            if is_neg {
                value *= -1;
            }

            Some(SExprToken::IntLit(value))
        } else {
            let end_idx = self.s.find(|c: char| c.is_whitespace() || c == '(' || c == ')').unwrap();
            let name = self.s[..end_idx].to_string();
            self.s = &self.s[end_idx..];
            Some(SExprToken::Ident(name))
        }
    }
}

struct SExprParser<'a> {
    s: std::iter::Peekable<SExprLexer<'a>>
}

impl<'a> SExprParser<'a> {
    pub fn new(s: &'a str) -> Self {
        SExprParser { s: SExprLexer { s }.peekable() }
    }

    pub fn parse(&mut self) -> Result<SExpr, String> {
        if self.s.peek() == Some(&SExprToken::OpenParen) {
            let (name, params) = self.parse_parenthesized()?;
            if name == "assert_return" {
                let mut params = params.into_iter();
                let lhs = params.next().unwrap();
                let rhs = params.collect();
                Ok(SExpr::AssertReturn(Box::new(lhs), rhs))
            } else if name == "assert_trap" {
                let mut params = params.into_iter();
                let lhs = params.next().unwrap();
                let rhs = params.next().unwrap();
                assert!(params.next().is_none());
                let rhs = if let SExpr::String(s) = rhs {
                    s
                } else {
                    panic!()
                };

                Ok(SExpr::AssertTrap(Box::new(lhs), rhs))
            } else {
                Ok(SExpr::Node { name, params })
            }
        } else {
            let tok = self.s.next().unwrap();
            match tok {
                SExprToken::IntLit(i) => Ok(SExpr::Int(i)),
                SExprToken::StrLit(s) => Ok(SExpr::String(s)),
                SExprToken::Ident(i) => Ok(SExpr::Ident(i)),
                _ => todo!("{:?}", tok)
            }
        }
    }

    pub fn parse_parenthesized(&mut self) -> Result<(String, Vec<SExpr>), String> {
        if self.s.next() != Some(SExprToken::OpenParen) {
            return Err("expected opening parenthesis".to_string())
        }

        let name = if let Some(SExprToken::Ident(i)) = self.s.next() {
            i
        } else {
            return Err("expected ident".to_string())
        };

        let mut params = Vec::new();
        while self.s.peek() != Some(&SExprToken::CloseParen) {
            params.push(self.parse()?);
        }

        if self.s.next() != Some(SExprToken::CloseParen) {
            return Err("expecting closing paren".to_string());
        }

        Ok((name, params))
    }
}

#[derive(Debug)]
pub enum SExpr {
    Node {
        name: String,
        params: Vec<SExpr>,
    },
    AssertReturn(Box<SExpr>, Vec<SExpr>),
    AssertTrap(Box<SExpr>, String),
    String(String),
    Ident(String),
    Int(i64),
}

impl std::str::FromStr for SExpr {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        SExprParser::new(s).parse()
    }
}
