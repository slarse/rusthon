pub mod lexer {

    #[derive(Debug, Eq, PartialEq)]
    pub enum Token {
        LeftParen,
        RightParen,
        Integer(i64),
        Word(String),
        Error(String),
    }

    pub fn tokenize(characters: &mut impl Iterator<Item = char>) -> Vec<Token> {
        let mut next_character = characters.next();

        let mut tokens = Vec::new();

        loop {
            match next_character {
                Some(letter @ 'A'..='z') => {
                    let (maybe_next, token) = lex_word(letter, characters);
                    next_character = maybe_next;
                    tokens.push(token);
                }
                Some(digit @ '0'..='9') => {
                    let (maybe_next, token) = lex_digit(digit, characters);
                    next_character = maybe_next;
                    tokens.push(token);
                }
                Some('(') => {
                    tokens.push(Token::LeftParen);
                    next_character = characters.next();
                }
                Some(')') => {
                    tokens.push(Token::RightParen);
                    next_character = characters.next();
                }
                Some(error) => {
                    tokens.push(Token::Error(error.to_string()));
                    next_character = characters.next();
                }
                None => {
                    break;
                }
            }
        }

        tokens
    }

    fn lex_word(
        leading_character: char,
        characters: &mut impl Iterator<Item = char>,
    ) -> (Option<char>, Token) {
        let to_word_token = |text: String| Token::Word(text);

        let is_letter = |character: char| ('A'..'z').contains(&character);

        return lex_token(leading_character, &is_letter, &to_word_token, characters);
    }

    fn lex_digit(
        leading_digit: char,
        characters: &mut impl Iterator<Item = char>,
    ) -> (Option<char>, Token) {
        let to_integer_token = |text: String| Token::Integer(text.parse().unwrap());

        let is_digit = |character: char| ('0'..='9').contains(&character);

        return lex_token(leading_digit, &is_digit, &to_integer_token, characters);
    }

    fn lex_token(
        leading_character: char,
        should_include: &dyn Fn(char) -> bool,
        to_token: &dyn Fn(String) -> Token,
        characters: &mut impl Iterator<Item = char>,
    ) -> (Option<char>, Token) {
        let mut characters_to_include = String::from(leading_character);

        for character in characters {
            if should_include(character) {
                characters_to_include.push(character);
            } else {
                return (Some(character), to_token(characters_to_include));
            }
        }

        return (None, to_token(characters_to_include));
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use std::fmt::Debug;

        #[test]
        fn lex_print_1() {
            let input = "print(1)".to_string();
            let expected_tokenization = vec![
                Token::Word("print".to_string()),
                Token::LeftParen,
                Token::Integer(1),
                Token::RightParen,
            ];

            let actual_tokenization = tokenize(&mut input.chars());

            assert_vectors_equal(&actual_tokenization, &expected_tokenization);
        }

        #[test]
        fn lex_bad_character() {
            let input = "print(;)";
            let expected_tokenization = vec![
                Token::Word("print".to_string()),
                Token::LeftParen,
                Token::Error(";".to_string()),
                Token::RightParen,
            ];

            let actual_tokenization = tokenize(&mut input.chars());
            
            assert_vectors_equal(&actual_tokenization, &expected_tokenization);
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
