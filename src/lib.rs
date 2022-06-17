pub mod lexer {
    use std::iter::Peekable;

    #[derive(Debug, Eq, PartialEq)]
    pub enum Token {
        LeftParen,
        RightParen,
        Integer(i64),
        Identifier(String),
        Error(String),
    }

    pub struct TokenInfo {
        pub token: Token,
        pub position: u32,
    }

    struct Tokenizer<I> {
        characters: I,
    }

    impl<I: Iterator<Item = char>> Iterator for Tokenizer<Peekable<I>> {
        type Item = TokenInfo;

        fn next(&mut self) -> Option<Self::Item> {
            let token = match self.characters.peek() {
                Some('A'..='z') => {
                    let token = lex_word(&mut self.characters);
                    token
                }
                Some('0'..='9') => {
                    let token = lex_digit(&mut self.characters);
                    token
                }
                Some('(') => {
                    self.characters.next();
                    Token::LeftParen
                }
                Some(')') => {
                    self.characters.next();
                    Token::RightParen
                }
                Some(_) => {
                    let error = self.characters.next().unwrap();
                    Token::Error(error.to_string())
                }
                None => return None,
            };

            // TODO make position "real"
            Some(TokenInfo { token, position: 0 })
        }
    }

    fn lex_word(characters: &mut Peekable<impl Iterator<Item = char>>) -> Token {
        let to_word_token = |text: String| Token::Identifier(text);

        let is_letter = |character: &char| ('A'..'z').contains(&character);

        return lex_token(&is_letter, &to_word_token, characters);
    }

    fn lex_digit(characters: &mut Peekable<impl Iterator<Item = char>>) -> Token {
        let to_integer_token = |text: String| Token::Integer(text.parse().unwrap());

        let is_digit = |character: &char| ('0'..='9').contains(&character);

        return lex_token(&is_digit, &to_integer_token, characters);
    }

    fn lex_token(
        should_include: &dyn Fn(&char) -> bool,
        to_token: &dyn Fn(String) -> Token,
        characters: &mut Peekable<impl Iterator<Item = char>>,
    ) -> Token {
        let mut characters_to_include = String::new();

        loop {
            match characters.peek() {
                Some(character) => {
                    if should_include(character) {
                        characters_to_include.push(characters.next().unwrap());
                    } else {
                        break;
                    }
                }
                None => {
                    break;
                }
            }
        }

        return to_token(characters_to_include);
    }

    pub fn tokenize(characters: impl Iterator<Item = char>) -> impl Iterator<Item = TokenInfo> {
        Tokenizer {
            characters: characters.peekable(),
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use std::fmt::Debug;

        #[test]
        fn lex_print_1() {
            let input = "print(1)".to_string();
            let expected_tokenization = vec![
                Token::Identifier("print".to_string()),
                Token::LeftParen,
                Token::Integer(1),
                Token::RightParen,
            ];

            let actual_tokenization = tokenize(input.chars()).map(|ti| ti.token);

            assert_vectors_equal(&actual_tokenization.collect(), &expected_tokenization);
        }

        #[test]
        fn lex_bad_character() {
            let input = "print(;)";
            let expected_tokenization = vec![
                Token::Identifier("print".to_string()),
                Token::LeftParen,
                Token::Error(";".to_string()),
                Token::RightParen,
            ];

            let actual_tokenization = tokenize(input.chars()).map(|ti| ti.token);

            assert_vectors_equal(&actual_tokenization.collect(), &expected_tokenization);
        }

        fn assert_vectors_equal<T: Debug + Eq>(actual: &Vec<T>, expected: &Vec<T>) {
            if actual.len() != expected.len() {
                panic!(
                    "expected vector has length {}, but actual vector has length {}",
                    expected.len(),
                    actual.len()
                );
            }

            for (actual, expected) in actual.iter().zip(expected.iter()) {
                if actual != expected {
                    panic!(
                        "mismatching characters, expected={:?}, actual={:?}",
                        expected, actual
                    );
                }
            }
        }
    }
}

pub mod parser {
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

    pub fn parse(tokens: Peekable<impl Iterator<Item = TokenInfo>>) -> Result<Program, String> {
        let mut mutable_tokens = tokens;
        let program = program(parse_expressions(&mut mutable_tokens)?);
        Result::Ok(program)
    }

    fn parse_expressions(
        tokens: &mut Peekable<impl Iterator<Item = TokenInfo>>,
    ) -> Result<Vec<Expression>, String> {
        Result::Ok(vec![parse_print(tokens)?])
    }

    fn parse_print(
        tokens: &mut Peekable<impl Iterator<Item = TokenInfo>>,
    ) -> Result<Expression, String> {
        match tokens.next().unwrap().token {
            Token::Identifier(identifier) => match identifier.as_str() {
                "print" => {
                    consume(Token::LeftParen, tokens)?;
                    let token = print(parse_integer(tokens));
                    consume(Token::RightParen, tokens)?;
                    Result::Ok(token)
                }
                _ => Result::Err("Badness!".to_string()),
            },
            _ => Result::Err("Badness!".to_string()),
        }
    }

    fn consume(
        token_to_consume: Token,
        tokens: &mut Peekable<impl Iterator<Item = TokenInfo>>,
    ) -> Result<(), String> {
        match tokens.next() {
            Some(token_info) if token_info.token == token_to_consume => Result::Ok(()),
            _ => Result::Err("Badness!".to_string()),
        }
    }

    fn parse_integer(tokens: &mut Peekable<impl Iterator<Item = TokenInfo>>) -> Expression {
        match tokens.next().unwrap().token {
            Token::Integer(value) => int(value),
            _ => panic!(),
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
        fn parse_print_1() {
            let input = "print(1)".to_string();
            let tokens = lexer::tokenize(input.chars());

            let expected_ast = program(vec![print(int(1))]);

            let ast = parse(tokens.peekable()).unwrap();

            assert_eq!(ast, expected_ast)
        }
    }
}
