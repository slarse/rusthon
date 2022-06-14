pub mod lexer {
    use std::iter::Peekable;

    #[derive(Debug, Eq, PartialEq)]
    pub enum Token {
        LeftParen,
        RightParen,
        Integer(i64),
        Word(String),
        Error(String),
    }

    struct Tokenizer<I> {
        characters: I,
    }

    impl<I: Iterator<Item = char>> Iterator for Tokenizer<Peekable<I>> {
        type Item = Token;

        fn next(&mut self) -> Option<Self::Item> {
            match self.characters.peek() {
                Some('A'..='z') => {
                    let token = lex_word(&mut self.characters);
                    Some(token)
                }
                Some('0'..='9') => {
                    let token = lex_digit(&mut self.characters);
                    Some(token)
                }
                Some('(') => {
                    self.characters.next();
                    Some(Token::LeftParen)
                }
                Some(')') => {
                    self.characters.next();
                    Some(Token::RightParen)
                }
                Some(_) => {
                    let error = self.characters.next().unwrap();
                    Some(Token::Error(error.to_string()))
                }
                None => None,
            }
        }
    }

    fn lex_word(characters: &mut Peekable<impl Iterator<Item = char>>) -> Token {
        let to_word_token = |text: String| Token::Word(text);

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

    pub fn tokenize(characters: impl Iterator<Item = char>) -> impl Iterator<Item = Token> {
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
                Token::Word("print".to_string()),
                Token::LeftParen,
                Token::Integer(1),
                Token::RightParen,
            ];

            let actual_tokenization = tokenize(input.chars());

            assert_vectors_equal(&actual_tokenization.collect(), &expected_tokenization);
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

            let actual_tokenization = tokenize(input.chars());

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
