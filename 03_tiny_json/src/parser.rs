use std::{collections::HashMap, mem};

use super::lexer::{Lexer, Token, TokenKind};
use super::span::substring;
use super::span::Span;

pub type Object = HashMap<String, Value>;

pub type Array = Vec<Value>;

#[derive(Debug, PartialEq)]
pub enum Value {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Array),
    Object(Object),
}

impl From<Object> for Value {
    fn from(o: Object) -> Self {
        Value::Object(o)
    }
}

impl From<Array> for Value {
    fn from(a: Array) -> Self {
        Value::Array(a)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Number(f)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

pub type ParseResult = (Option<Value>, Vec<Error>);

pub fn parse_json(source: &str) -> ParseResult {
    Parser::new(source).parse_document()
}

#[derive(Debug)]
struct Parser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
    errors: Vec<Error>,
    current: Token,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Parser<'a> {
        let mut parser = Parser {
            source,
            lexer: Lexer::new(source),
            errors: Vec::new(),
            current: Default::default(),
        };
        parser.advance();
        parser
    }

    fn parse_document(&mut self) -> ParseResult {
        let value = match self.parse_value() {
            None => None,
            Some(v) => match v {
                Value::Object(..) | Value::Array(..) => Some(v),
                _ => {
                    self.err("only object or array are allowed at top level");
                    None
                }
            },
        };

        // Sanity check
        if value.is_none() {
            assert!(
                !self.errors.is_empty(),
                "internal error: parsing failed without an error message"
            );
        }

        return (value, mem::take(&mut self.errors));
    }

    fn parse_value(&mut self) -> Option<Value> {
        loop {
            match self.current.kind {
                TokenKind::StartObject => return self.parse_object().map(Into::into),
                TokenKind::StartArray => return self.parse_array().map(Into::into),
                TokenKind::StringLiteral { .. } => return self.parse_string().map(Into::into),
                TokenKind::NumberLiteral => return self.parse_number().map(Into::into),
                TokenKind::True => {
                    self.advance();
                    return Value::Boolean(true).into();
                }
                TokenKind::False => {
                    self.advance();
                    return Value::Boolean(false).into();
                }
                TokenKind::Null => {
                    self.advance();
                    return Value::Null.into();
                }

                // Errors
                kind @ TokenKind::EndObject | kind @ TokenKind::EndArray => {
                    self.err_at(
                        self.current.span,
                        &format!("unbalanced {}, ignoring", kind.name()),
                    );
                    self.advance();
                }
                TokenKind::Error => {
                    self.err_at(self.current.span, "invalid syntax");
                    self.advance();
                }
                kind => {
                    self.err_at(
                        self.current.span,
                        &format!("unexpected {}, expected a value", kind.name()),
                    );
                    self.advance();

                    if kind == TokenKind::Eof {
                        return None;
                    }
                }
            }
        }
    }

    fn parse_object(&mut self) -> Option<Object> {
        self.expect(TokenKind::StartObject)?;

        let mut object = Object::new();
        if self.current.kind == TokenKind::EndObject {
            self.advance();
            return object.into();
        }

        loop {
            let name = self.parse_string()?;
            self.expect(TokenKind::Colon);
            let value = self.parse_value()?;

            object.insert(name, value);

            if self.current.kind == TokenKind::EndObject {
                break;
            }
            self.expect(TokenKind::Comma)?;
        }
        self.expect(TokenKind::EndObject)?;
        return object.into();
    }

    fn parse_array(&mut self) -> Option<Array> {
        self.expect(TokenKind::StartArray)?;

        let mut array = Array::new();
        if self.current.kind == TokenKind::EndArray {
            self.advance();
            return array.into();
        }
        loop {
            let value = self.parse_value()?;
            array.push(value);

            if self.current.kind == TokenKind::EndArray {
                break;
            }
            self.expect(TokenKind::Comma)?;
        }
        self.expect(TokenKind::EndArray);
        return array.into();
    }

    fn parse_string(&mut self) -> Option<String> {
        let token = self.current;
        match token.kind {
            TokenKind::StringLiteral { terminated } => {
                self.advance();

                let mut chars = substring(self.source, token.span).chars();

                // Skip leading and trailing quotes
                let start = chars.next();
                let end = if terminated { chars.next_back() } else { None };
                debug_assert!(
                    start.is_some() && start.unwrap() == '"',
                    "string does not start with a '\"'",
                );
                debug_assert!(
                    !terminated || end.is_some() && end.unwrap() == '"',
                    "string does not end with a '\"'",
                );

                // Handle all content chars, including (only some) escape characters.
                let mut string = String::new();
                loop {
                    let ch = match chars.next() {
                        None => break,
                        Some(n) => n,
                    };

                    if ch != '\\' {
                        string.push(ch);
                    } else {
                        let ch = match chars.next() {
                            None => {
                                self.err_at(token.span, "expected an escape character after '\\'");
                                break;
                            }
                            Some(n) => n,
                        };
                        let escaped = match ch {
                            '"' => '"',
                            't' => '\t',
                            'r' => '\r',
                            'n' => '\n',
                            _ => {
                                // TODO: Would be better to have the actual position here!
                                self.err_at(
                                    token.span,
                                    &format!("invalid escape character '{}'", ch),
                                );
                                break;
                            }
                        };
                        string.push(escaped);
                    }
                }

                if !terminated {
                    self.err_at(
                        Span::new(token.span.end, token.span.end),
                        "unterminated string",
                    );
                }
                return string.into();
            }
            _ => {
                self.err_at(
                    token.span,
                    &format!("unexpected {}, expected a string", token.kind.name()),
                );
                self.advance();
                return None;
            }
        };
    }

    fn parse_number(&mut self) -> Option<f64> {
        let token = self.current;
        match token.kind {
            TokenKind::NumberLiteral => {
                self.advance();

                let num = substring(self.source, token.span).parse::<f64>();
                match num {
                    Err(e) => {
                        self.err_at(token.span, &format!("invalid number: {}", &e));
                        return None;
                    }
                    Ok(v) => return Some(v),
                }
            }
            _ => {
                self.err_at(
                    token.span,
                    &format!("unexpected {}, expected a number", token.kind.name()),
                );
                self.advance();
                return None;
            }
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Option<Token> {
        if self.current.kind == kind {
            let token = self.current;
            self.advance();
            return token.into();
        }

        self.err_at(
            self.current.span,
            &format!(
                "unexpected {}, expected {}",
                self.current.kind.name(),
                kind.name()
            ),
        );
        return None;
    }

    fn advance(&mut self) {
        self.current = self.lexer.next()
    }

    fn err(&mut self, message: &str) {
        self.err_at(Span::new(0, self.source.len() as u32), message)
    }

    fn err_at(&mut self, span: Span, message: &str) {
        self.errors.push(Error {
            span: span,
            message: message.into(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_returns_empty_object() {
        let value = must_parse("{}");
        match value {
            Value::Object(map) => assert_eq!(map.len(), 0),
            _ => panic!("unexpected type"),
        }
    }

    #[test]
    fn parser_returns_empty_array() {
        let value = must_parse("[]");
        match value {
            Value::Array(vec) => assert_eq!(vec.len(), 0),
            _ => panic!("unexpected type"),
        }
    }

    #[test]
    fn parser_recognizes_all_value_types() {
        let source = r#"
            {
                "constants": [true, false, null],
                "nested": {
                    "number": 1,
                    "string": "foo"
                }
            }
        "#;
        let value = must_parse(source);
        let expected = {
            let constants: Array = vec![Value::Boolean(true), Value::Boolean(false), Value::Null];
            let mut nested = Object::new();
            nested.insert("number".to_string(), Value::Number(1.0));
            nested.insert("string".to_string(), Value::String("foo".to_string()));

            let mut expected = Object::new();
            expected.insert("constants".to_string(), Value::Array(constants));
            expected.insert("nested".to_string(), Value::Object(nested));
            Value::Object(expected)
        };
        assert_eq!(&value, &expected);
    }

    #[test]
    fn parser_errors_for_invalid_top_level_type() {
        let invalid = ["3", "true", "false", "null", "\"foo\""];
        for source in invalid.iter() {
            println!("source: {}", source);

            let (value, errors) = parse_json(source);
            assert!(value.is_none());
            assert!(!errors.is_empty());

            for err in &errors {
                println!("{}: {}", err.span, err.message);
            }

            let found = errors
                .iter()
                .filter(|e| e.message.contains("top level"))
                .count();
            assert_eq!(found, 1);
        }
    }

    fn must_parse(source: &str) -> Value {
        let (value, errors) = parse_json(source);
        if !errors.is_empty() {
            for err in &errors {
                println!("{}: {}", err.span, err.message);
            }
            panic!("failed to parse source");
        }
        return value.expect("must have a value if there are not errors");
    }
}
