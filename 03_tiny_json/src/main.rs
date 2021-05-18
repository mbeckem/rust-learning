use std::{collections::HashMap, mem::take, str::CharIndices, usize};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
struct Span {
    // Start byte offset (inclusive)
    start: u32,

    // End byte offset (exclusive)
    end: u32,
}

impl Span {
    fn new(start: u32, end: u32) -> Span {
        debug_assert!(
            start <= end,
            "Start must be less than or equal to end (start: {}, end: {})",
            start,
            end
        );
        Span { start, end }
    }
}

fn substring(source: &str, span: Span) -> &str {
    let start = span.start as usize;
    let end = span.end as usize;
    assert!(start <= source.len() && end <= source.len());
    return &source[start..end];
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum TokenKind {
    StartObject,                        // {
    EndObject,                          // }
    StartArray,                         // [
    EndArray,                           // ]
    Comma,                              // ,
    Colon,                              // :
    False,                              // keyword false
    True,                               // keyword true
    Null,                               // keyword null
    NumberLiteral,                      // floats
    StringLiteral { terminated: bool }, // strings
    Error,                              // Unrecognized code point(s)
    Eof,                                // End of file
}

impl TokenKind {
    fn name(&self) -> &'static str {
        use TokenKind::*;
        match self {
            StartObject => "'{'",
            EndObject => "'}'",
            StartArray => "'['",
            EndArray => "']'",
            Comma => "','",
            Colon => "':'",
            False => "'false'",
            True => "'true'",
            Null => "'null'",
            NumberLiteral => "number",
            StringLiteral { .. } => "string",
            Error => "error",
            Eof => "end of file",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Token {
    kind: TokenKind,
    span: Span,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::Eof,
            span: Span::new(0, 0),
        }
    }
}

#[derive(Debug)]
struct Lexer<'a> {
    source: &'a str,
    iter: CharIndices<'a>,

    // Current code point, or none on eof
    current: Option<char>,
    current_start: u32,

    // Start offset of the next character (if any), same as front_offset in iter (which is private)
    next_start: u32,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            source,
            iter: source.char_indices(),
            current: None,
            current_start: 0,
            next_start: 0,
        };
        lexer.advance();
        lexer
    }

    fn next(&mut self) -> Token {
        self.skip_whitespace();

        let token_start = self.current_start;
        let kind = match self.current {
            None => TokenKind::Eof,
            Some(ch) => match ch {
                '{' => {
                    self.advance();
                    TokenKind::StartObject
                }
                '}' => {
                    self.advance();
                    TokenKind::EndObject
                }
                '[' => {
                    self.advance();
                    TokenKind::StartArray
                }
                ']' => {
                    self.advance();
                    TokenKind::EndArray
                }
                ',' => {
                    self.advance();
                    TokenKind::Comma
                }
                ':' => {
                    self.advance();
                    TokenKind::Colon
                }
                '"' => self.lex_string(),
                _ if ch.is_ascii_digit() => {
                    self.lex_number();
                    TokenKind::NumberLiteral
                }
                _ if ch.is_alphabetic() => self.lex_constant(),
                _ => {
                    self.advance();
                    TokenKind::Error
                }
            },
        };

        return Token {
            kind: kind,
            span: Span::new(token_start, self.current_start),
        };
    }

    fn lex_number(&mut self) {
        debug_assert!(
            self.current.filter(|ch| ch.is_ascii_digit()).is_some(),
            "not at the start of an number"
        );

        // TODO: Floating point numbers not handled.
        while let Some(ch) = self.current {
            if !ch.is_ascii_digit() {
                break;
            }

            self.advance()
        }
    }

    fn lex_string(&mut self) -> TokenKind {
        debug_assert!(self.current == Some('"'), "not at the start of a string");

        self.advance();
        while let Some(ch) = self.current {
            if ch == '"' {
                self.advance();
                return TokenKind::StringLiteral { terminated: true };
            }

            if ch == '\\' {
                self.advance();
            }
            self.advance();
        }
        return TokenKind::StringLiteral { terminated: false };
    }

    fn lex_constant(&mut self) -> TokenKind {
        debug_assert!(
            self.current.filter(|ch| ch.is_alphanumeric()).is_some(),
            "not at the start of a constant"
        );

        let start = self.current_start;
        while let Some(ch) = self.current {
            if !ch.is_alphanumeric() {
                break;
            }

            self.advance();
        }

        let str = &self.source[(start as usize)..(self.current_start as usize)];
        match str {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "null" => TokenKind::Null,
            _ => TokenKind::Error,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn advance(&mut self) {
        if let Some((index, ch)) = self.iter.next() {
            self.current = Some(ch);
            self.current_start = index as u32;
            return;
        }

        self.current = None;
        self.current_start = self.source.len() as u32;
    }
}

type Object = HashMap<String, Value>;

type Array = Vec<Value>;

#[derive(Debug)]
enum Value {
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
struct Error {
    span: Span,
    message: String,
}

type ParseResult = (Option<Value>, Vec<Error>);

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

        return (value, take(&mut self.errors));
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

        let mut object: Object = HashMap::new();
        while self.current.kind != TokenKind::EndObject {
            let name = self.parse_string()?;
            self.expect(TokenKind::Colon);
            let value = self.parse_value()?;

            object.insert(name, value);
        }

        self.expect(TokenKind::EndObject)?;
        return object.into();
    }

    fn parse_array(&mut self) -> Option<Array> {
        todo!()
    }

    fn parse_string(&mut self) -> Option<String> {
        let token = self.current;
        match token.kind {
            TokenKind::StringLiteral { terminated } => {
                self.advance();

                let mut chars = substring(self.source, token.span).chars();
                let mut string = String::new();

                todo!("parse string content from chars");

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
        todo!()
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

fn parse_json(source: &str) -> ParseResult {
    Parser::new(source).parse_document()
}

fn main() {
    let source = "{ \"foo\": \"bar\" }";
    let mut lexer = Lexer::new(source);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_returns_single_eof_token_for_empty_source() {
        let tokens = consume_tokens("");
        let expected = [Token {
            kind: TokenKind::Eof,
            span: Span::new(0, 0),
        }];
        assert_eq!(&tokens[..], &expected)
    }

    #[test]
    fn lexer_returns_syntax_elements() {
        let kinds = consume_token_kinds("{[]},:");
        let expected = [
            TokenKind::StartObject,
            TokenKind::StartArray,
            TokenKind::EndArray,
            TokenKind::EndObject,
            TokenKind::Comma,
            TokenKind::Colon,
            TokenKind::Eof,
        ];
        assert_eq!(&kinds, &expected);
    }

    #[test]
    fn lexer_returns_string_literals() {
        let source = r#"   "hello\"hello"   "#;
        let tokens = consume_tokens(source);
        let kinds = map_kinds(&tokens);
        let expected = [
            TokenKind::StringLiteral { terminated: true },
            TokenKind::Eof,
        ];
        assert_eq!(&kinds, &expected);

        let string_token = tokens[0];
        assert_eq!(substring(source, string_token.span), r#""hello\"hello""#);
    }

    #[test]
    fn lexer_returns_unterminated_string_literals() {
        let source = r#"   "hello "#;
        let tokens = consume_tokens(source);
        let kinds = map_kinds(&tokens);
        let expected = [
            TokenKind::StringLiteral { terminated: false },
            TokenKind::Eof,
        ];
        assert_eq!(&kinds, &expected);

        let string_token = tokens[0];
        assert_eq!(substring(source, string_token.span), "\"hello ");
    }

    #[test]
    fn lexer_returns_numbers() {
        let source = r#"  12345 "#;
        let tokens = consume_tokens(source);
        let kinds = map_kinds(&tokens);
        let expected = [TokenKind::NumberLiteral, TokenKind::Eof];
        assert_eq!(&kinds, &expected);

        let number_token = tokens[0];
        assert_eq!(substring(source, number_token.span), "12345");
    }

    #[test]
    fn lexer_returns_constants() {
        let kinds = consume_token_kinds("true false null");
        let expected = [
            TokenKind::True,
            TokenKind::False,
            TokenKind::Null,
            TokenKind::Eof,
        ];
        assert_eq!(&kinds, &expected);
    }

    #[test]
    fn lexer_rejects_invalid_identifiers() {
        let kinds = consume_token_kinds("asd foo bar");
        let expected = [
            TokenKind::Error,
            TokenKind::Error,
            TokenKind::Error,
            TokenKind::Eof,
        ];
        assert_eq!(&kinds, &expected);
    }

    fn consume_tokens(source: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut lexer = Lexer::new(source);
        loop {
            let token = lexer.next();
            tokens.push(token);
            if token.kind == TokenKind::Eof {
                return tokens;
            }
        }
    }

    fn consume_token_kinds(source: &str) -> Vec<TokenKind> {
        map_kinds(&consume_tokens(source))
    }

    fn map_kinds(tokens: &[Token]) -> Vec<TokenKind> {
        tokens.iter().map(|t| t.kind).collect()
    }
}
