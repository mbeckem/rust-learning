use std::str::CharIndices;

use super::span::Span;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TokenKind {
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
    pub fn name(&self) -> &'static str {
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
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
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
pub struct Lexer<'a> {
    source: &'a str,
    iter: CharIndices<'a>,

    // Current code point, or none on eof
    current: Option<char>,
    current_start: u32,

    // Start offset of the next character (if any), same as front_offset in iter (which is private)
    next_start: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
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

    pub fn next(&mut self) -> Token {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::substring;

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

    #[test]
    fn lexer_reports_correct_positions() {
        use TokenKind::*;

        let source = r#"{"a": 4567, "b": [1, 2]}"#;
        let tokens = consume_tokens(source);
        let expected: Vec<_> = [
            (StartObject, (0, 1)),
            (StringLiteral { terminated: true }, (1, 4)),
            (Colon, (4, 5)),
            (NumberLiteral, (6, 10)),
            (Comma, (10, 11)),
            (StringLiteral { terminated: true }, (12, 15)),
            (Colon, (15, 16)),
            (StartArray, (17, 18)),
            (NumberLiteral, (18, 19)),
            (Comma, (19, 20)),
            (NumberLiteral, (21, 22)),
            (EndArray, (22, 23)),
            (EndObject, (23, 24)),
        ]
        .iter()
        .map(|t| Token {
            kind: t.0,
            span: Span::new(t.1 .0, t.1 .1),
        })
        .collect();

        for (actual, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(actual, expected);
        }
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
