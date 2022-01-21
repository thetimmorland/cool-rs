use std::{iter::Peekable, rc::Rc, str::Chars};

use crate::string_table::StringTable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // MCommentStart (*
    LParen, //       (

    // SCommentStart --
    Sub, //          -

    Assign, // <-
    Leq,    // <=
    Lt,     // <

    Arrow, // =>
    Eq,    // =

    At,        // @
    Colon,     // :
    Div,       // /
    Dot,       // .
    LBrace,    // {
    Mul,       // *
    Not,       // ~
    Add,       // +
    RBrace,    // }
    RParen,    // )
    Semicolon, // ;

    CaseKw,
    ClassKw,
    ElseKw,
    EsacKw,
    FiKw,
    IfKw,
    InKw,
    InheritsKw,
    IsVoidKw,
    LetKw,
    LoopKw,
    NewKw,
    NotKw,
    OfKw,
    PoolKw,
    ThenKw,
    WhileKw,

    String(Rc<String>),
    Int(u64),
    Bool(bool),

    TypeId(Rc<String>),
    ObjId(Rc<String>),

    Error,

    EOF,
}

pub struct Token {
    pub kind: TokenKind,
    pub line_num: u64,
    pub col_num: u64,
}

pub struct Lexer<'a> {
    is_done: bool,

    chars: Peekable<Chars<'a>>,
    str_table: StringTable,

    current_pos: (u64, u64),
    saved_pos: (u64, u64),
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            is_done: false,

            str_table: StringTable::new(),
            chars: src.chars().peekable(),

            current_pos: (1, 1),
            saved_pos: (1, 1),
        }
    }

    fn new_token(&mut self, kind: TokenKind) -> Token {
        Token {
            kind,
            line_num: self.saved_pos.0,
            col_num: self.saved_pos.1,
        }
    }

    fn new_string(&mut self, s: &str) -> Rc<String> {
        self.str_table.insert(s)
    }

    fn next_char(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            if c == '\n' {
                self.current_pos.0 += 1;
                self.current_pos.1 = 1;
            } else {
                self.current_pos.1 += 1;
            };

            Some(c)
        } else {
            None
        }
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn next_char_if<P: Fn(&char) -> bool>(&mut self, predicate: P) -> Option<char> {
        if let Some(true) = self.peek_char().map(predicate) {
            self.next_char()
        } else {
            None
        }
    }

    fn next_char_if_eq(&mut self, expected: &char) -> Option<char> {
        self.chars.next_if_eq(expected)
    }

    fn skip_whitespace(&mut self) {
        while let Some(_) = self.next_char_if(char::is_ascii_whitespace) {}
    }

    fn skip_multiline_comment(&mut self) {
        while let Some(c) = self.next_char() {
            if c == '*' && self.next_char_if_eq(&')').is_some() {
                break;
            }
        }
    }

    fn skip_single_line_comment(&mut self) {
        while let Some(c) = self.next_char() {
            if c == '\n' {
                break;
            }
        }
    }

    fn read_string(&mut self) -> TokenKind {
        let mut s = String::new();

        loop {
            match self.next_char() {
                None => break TokenKind::Error,
                Some('"') => break TokenKind::String(self.new_string(&s)),
                Some('\\') => match self.next_char() {
                    None => break TokenKind::Error,
                    Some('b') => s.push('\u{0008}'),
                    Some('t') => s.push('\t'),
                    Some('n') => s.push('\n'),
                    Some('f') => s.push('\u{000C}'),
                    Some(c) => s.push(c),
                },
                Some(c) => s.push(c),
            }
        }
    }

    fn read_integer(&mut self, c: char) -> TokenKind {
        let mut s = c.to_string();

        while let Some(c) = self.next_char_if(|c| c.is_ascii_alphanumeric() || c == &'_') {
            s.push(c);
        }

        if let Ok(x) = s.parse() {
            TokenKind::Int(x)
        } else {
            TokenKind::Error
        }
    }

    fn read_kw_or_ident(&mut self, c: char) -> TokenKind {
        let mut s = c.to_string();

        while let Some(c) = self.next_char_if(|c| c.is_ascii_alphanumeric() || c == &'_') {
            s.push(c);
        }

        let lower_s = s.to_ascii_lowercase();

        if lower_s == "case" {
            TokenKind::CaseKw
        } else if lower_s == "class" {
            TokenKind::ClassKw
        } else if lower_s == "else" {
            TokenKind::ElseKw
        } else if lower_s == "esac" {
            TokenKind::EsacKw
        } else if lower_s == "fi" {
            TokenKind::FiKw
        } else if lower_s == "if" {
            TokenKind::IfKw
        } else if lower_s == "in" {
            TokenKind::InKw
        } else if lower_s == "inherits" {
            TokenKind::InheritsKw
        } else if lower_s == "isvoid" {
            TokenKind::IsVoidKw
        } else if lower_s == "let" {
            TokenKind::LetKw
        } else if lower_s == "loop" {
            TokenKind::LoopKw
        } else if lower_s == "new" {
            TokenKind::NewKw
        } else if lower_s == "not" {
            TokenKind::NotKw
        } else if lower_s == "of" {
            TokenKind::OfKw
        } else if lower_s == "pool" {
            TokenKind::PoolKw
        } else if lower_s == "then" {
            TokenKind::ThenKw
        } else if lower_s == "while" {
            TokenKind::WhileKw
        } else if s.starts_with(|c: char| c.is_ascii_lowercase()) {
            if lower_s == "true" {
                TokenKind::Bool(true)
            } else if lower_s == "false" {
                TokenKind::Bool(false)
            } else {
                TokenKind::ObjId(self.new_string(&s))
            }
        } else {
            TokenKind::TypeId(self.new_string(&s))
        }
    }

    fn read_token(&mut self) -> Option<TokenKind> {
        self.saved_pos = self.current_pos;

        match self.next_char() {
            None => None,

            Some(c) if c.is_ascii_whitespace() => {
                self.skip_whitespace();
                self.read_token()
            }

            Some('(') => {
                if self.next_char_if_eq(&'*').is_some() {
                    self.skip_multiline_comment();
                    self.read_token()
                } else {
                    Some(TokenKind::LParen)
                }
            }

            Some('-') => {
                if self.next_char_if_eq(&'-').is_some() {
                    self.skip_single_line_comment();
                    self.read_token()
                } else {
                    Some(TokenKind::Sub)
                }
            }

            Some('<') => {
                if self.next_char_if_eq(&'-').is_some() {
                    Some(TokenKind::Assign)
                } else if let Some(_) = self.next_char_if_eq(&'=') {
                    Some(TokenKind::Leq)
                } else {
                    Some(TokenKind::Lt)
                }
            }

            Some('=') => {
                if self.next_char_if_eq(&'>').is_some() {
                    Some(TokenKind::Arrow)
                } else {
                    Some(TokenKind::Eq)
                }
            }

            Some('@') => Some(TokenKind::At),
            Some(':') => Some(TokenKind::Colon),
            Some('/') => Some(TokenKind::Div),
            Some('.') => Some(TokenKind::Dot),
            Some('{') => Some(TokenKind::LBrace),
            Some('*') => Some(TokenKind::Mul),
            Some('~') => Some(TokenKind::Not),
            Some('+') => Some(TokenKind::Add),
            Some('}') => Some(TokenKind::RBrace),
            Some(')') => Some(TokenKind::RParen),
            Some(';') => Some(TokenKind::Semicolon),

            Some('"') => Some(self.read_string()),
            Some(c) if c.is_ascii_digit() => Some(self.read_integer(c)),
            Some(c) if c.is_ascii_alphanumeric() => Some(self.read_kw_or_ident(c)),

            Some(_c) => Some(TokenKind::Error),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_done {
            None
        } else if let Some(kind) = self.read_token() {
            Some(self.new_token(kind))
        } else {
            self.is_done = true;
            Some(self.new_token(TokenKind::EOF))
        }
    }
}

pub fn lex(src: &str) -> Vec<Token> {
    Lexer::new(src).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let src = r#"
        class Foo {
            (* My Class *)

            -- this is a string :)
            let bar: String <- "";
        };
        "#;

        let token_kinds = lex(src).into_iter().map(|t| t.kind).collect::<Vec<_>>();

        use TokenKind::*;

        assert_eq!(
            token_kinds,
            [
                ClassKw,
                TypeId(Rc::new("Foo".into())),
                LBrace,
                LetKw,
                ObjId(Rc::new("bar".into())),
                Colon,
                TypeId(Rc::new("String".into())),
                Assign,
                String(Rc::new("".into())),
                Semicolon,
                RBrace,
                Semicolon,
                EOF
            ]
        )
    }
}
