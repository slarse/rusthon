use crate::lexer::Token;
use crate::lexer::TokenInfo;
use std::iter::Peekable;

#[derive(Debug, Eq, PartialEq)]
pub enum Program {
    Expressions(Vec<Expression>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Print(Box<Expression>),
    Integer(i64),
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseError {
    position: u32,
    message: String,
    bad_token: Option<Token>,
}

impl ParseError {
    pub fn error_string(&self) -> String {
        format!(
            "{}: found {:?} at position {}",
            self.message, self.bad_token, self.position
        )
    }
}

fn parse_error<T>(message: String, bad_token: Option<Token>) -> Result<T, ParseError> {
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

pub fn parse(
    tokens: Peekable<impl Iterator<Item = TokenInfo>>,
) -> Result<Program, ParseError> {
    let mut mutable_tokens = tokens;
    let program = program(parse_expressions(&mut mutable_tokens)?);
    Result::Ok(program)
}

fn parse_expressions(
    tokens: &mut Peekable<impl Iterator<Item = TokenInfo>>,
) -> Result<Vec<Expression>, ParseError> {
    Result::Ok(vec![parse_print(tokens)?])
}

fn parse_print(
    tokens: &mut Peekable<impl Iterator<Item = TokenInfo>>,
) -> Result<Expression, ParseError> {
    match tokens.next() {
        Some(token_info) => match token_info.token {
            Token::Identifier(identifier) if identifier == "print" => {
                    consume(Token::LeftParen, tokens)?;
                    let token = print(parse_integer(tokens)?);
                    consume(Token::RightParen, tokens)?;
                    Result::Ok(token)
            },
            other_token => parse_error("expected print".to_string(), Some(other_token)),
        },
        None => unexpected_eof(),
    }
}

fn consume<'a>(
    token_to_consume: Token,
    tokens: &mut Peekable<impl Iterator<Item = TokenInfo> + 'a>,
) -> Result<(), ParseError> {
    match tokens.next() {
        Some(token_info) if token_info.token == token_to_consume => Result::Ok(()),
        Some(token_info) => parse_error(format!("expected {:?}", token_to_consume), Some(token_info.token)),
        None => unexpected_eof(),
    }
}

fn parse_integer(tokens: &mut Peekable<impl Iterator<Item = TokenInfo>>) -> Result<Expression, ParseError> {
    match tokens.next() {
        Some(token_info) => match token_info.token {
            Token::Integer(value) => Result::Ok(int(value)),
            other_token => parse_error("expected integer".to_string(), Some(other_token)),
        }
        None => unexpected_eof(),
    }
}

fn int(value: i64) -> Expression {
    Expression::Integer(value)
}

fn print(expression: Expression) -> Expression {
    Expression::Print(Box::new(expression))
}

fn program(expressions: Vec<Expression>) -> Program {
    Program::Expressions(expressions)
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

        assert_eq!(error.error_string(), "unexpected EOF: found None at position 0")
    }

    #[test]
    fn parse_misspelled_print_is_error() {
        let input = "prit(1)".to_string();
        let tokens = lexer::tokenize(input.chars());

        let error = parse(tokens.peekable()).err().unwrap();

        assert_eq!(error.error_string(), "expected print: found Some(Identifier(\"prit\")) at position 0")
    }
}
