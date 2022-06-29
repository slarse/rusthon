//! Rusthon's parser: takes tokens from the lexer and produces an AST.
//!
//! A parser takes a sequence of source code tokens and produces an _abstract
//! syntax tree_ (AST). Rusthon does not have an explicit _concrete syntax tree_
//! (CST), but instead parses a token sequence directly to an AST.

use crate::lexer::Token;
use crate::lexer::TokenKind;
use std::iter::Peekable;

/// The top-level syntactical construct.
///
/// Program ::= Expression
#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    expressions: Vec<Expression>,
}

#[derive(Debug)]
pub struct Expression {
    id: u32,
    tokens: Vec<Token>,
    kind: ExpressionKind,
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Expression {}

/// An expression.
///
/// Expression ::= print ( <INTEGER> )
///             | [0-9]+
#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionKind {
    Invocation {
        target: String,
        argument: Box<Expression>,
    },
    Integer(i64),
}

/// Parse a sequence of `tokens` into an AST rooted in a [`Program`] node.
///
/// # Examples
/// ```
/// use rusthon::lexer;
/// use rusthon::parser::*;
///
/// let input = "print(1)".to_string();
/// let tokens = lexer::tokenize(input.chars());
///
/// let expected_ast = program(vec![print(int(1))]);
///
/// let ast = parse(tokens.peekable()).unwrap();
///
/// assert_eq!(ast, expected_ast)
/// ```
pub fn parse(tokens: Peekable<impl Iterator<Item = Token>>) -> Result<Program, ParseError> {
    let mut mutable_tokens = tokens;
    let program = program(parse_expressions(&mut mutable_tokens)?);
    Result::Ok(program)
}

/// Convenience function to construct a [`Program`].
pub fn program(expressions: Vec<Expression>) -> Program {
    Program { expressions }
}

/// Convenience function to construct an `Expression::Print`.
pub fn print(expression: Expression) -> Expression {
    print_(expression, 0, vec![])
}

fn print_(expression: Expression, id: u32, tokens: Vec<Token>) -> Expression {
    let kind = ExpressionKind::Invocation {
        target: "print".to_string(),
        argument: Box::new(expression),
    };
    Expression { id, tokens, kind }
}

/// Convenience function to construct an `Expression::Integer`.
pub fn int(value: i64) -> Expression {
    int_(value, 0, vec![])
}

fn int_(value: i64, id: u32, tokens: Vec<Token>) -> Expression {
    Expression {
        id,
        tokens,
        kind: ExpressionKind::Integer(value),
    }
}

/// A parsing error due to an unexpected token or EOF.
#[derive(Debug, Eq, PartialEq)]
pub struct ParseError {
    position: u32,
    message: String,
    bad_token: Option<TokenKind>,
}

impl ParseError {
    pub fn error_string(&self) -> String {
        format!(
            "{}: found {:?} at position {}",
            self.message, self.bad_token, self.position
        )
    }
}

fn parse_error<T>(message: String, bad_token: Option<TokenKind>) -> Result<T, ParseError> {
    Result::Err(ParseError {
        message,
        bad_token,
        position: 0,
    })
}

fn unexpected_eof<T>() -> Result<T, ParseError> {
    Result::Err(ParseError {
        message: "unexpected EOF".to_string(),
        bad_token: None,
        position: 0,
    })
}

fn parse_expressions(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Vec<Expression>, ParseError> {
    Result::Ok(vec![parse_print(tokens)?])
}

fn parse_print(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Expression, ParseError> {
    match tokens.next() {
        Some(token) => match token.kind {
            TokenKind::Identifier(identifier) if identifier == "print" => {
                consume(TokenKind::LeftParen, tokens)?;
                let expression = print_(parse_integer(tokens)?, 0, vec![]);
                consume(TokenKind::RightParen, tokens)?;
                Result::Ok(expression)
            }
            other_token => parse_error("expected print".to_string(), Some(other_token)),
        },
        None => unexpected_eof(),
    }
}

fn consume<'a>(
    token_to_consume: TokenKind,
    tokens: &mut Peekable<impl Iterator<Item = Token> + 'a>,
) -> Result<(), ParseError> {
    match tokens.next() {
        Some(token_info) if token_info.kind == token_to_consume => Result::Ok(()),
        Some(token_info) => parse_error(
            format!("expected {:?}", token_to_consume),
            Some(token_info.kind),
        ),
        None => unexpected_eof(),
    }
}

fn parse_integer(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Expression, ParseError> {
    match tokens.next() {
        Some(token) => match token.kind {
            TokenKind::Integer(value) => Result::Ok(int_(value, 0, vec![token])),
            other_token => parse_error("expected integer".to_string(), Some(other_token)),
        },
        None => unexpected_eof(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    #[test]
    fn parse_print_1_is_ok() {
        let input = "print(1)".to_string();
        let tokens = lexer::tokenize(input.chars());

        let expected_ast = program(vec![print(int(1))]);

        let ast = parse(tokens.peekable()).unwrap();

        assert_eq!(ast, expected_ast)
    }

    #[test]
    fn parse_print_1_without_closing_parenthesis_is_error() {
        let input = "print(1".to_string();
        let tokens = lexer::tokenize(input.chars());

        let error = parse(tokens.peekable()).err().unwrap();

        assert_eq!(
            error.error_string(),
            "unexpected EOF: found None at position 0"
        )
    }

    #[test]
    fn parse_misspelled_print_is_error() {
        let input = "prit(1)".to_string();
        let tokens = lexer::tokenize(input.chars());

        let error = parse(tokens.peekable()).err().unwrap();

        assert_eq!(
            error.error_string(),
            "expected print: found Some(Identifier(\"prit\")) at position 0"
        )
    }
}
