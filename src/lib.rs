pub mod lexer {

    #[derive(Debug, Eq, PartialEq)]
    pub enum Token {
        LeftParen,
        RightParen,
        Integer(i64),
        Word(String),
    }

    pub fn tokenize(characters: &mut impl Iterator<Item = char>) -> Vec<Token> {
        let mut next_character = characters.next();

        let mut tokens = Vec::new();

        loop {
            match next_character {
                Some(character) => {
                    if is_letter(character) {
                        let (maybe_next, token) = lex_word(character, characters);
                        next_character = maybe_next;
                        tokens.push(token);
                    } else if is_digit(character) {
                        let (maybe_next, token) = lex_digit(character, characters);
                        next_character = maybe_next;
                        tokens.push(token);
                    } else if character == '(' {
                        tokens.push(Token::LeftParen);
                        next_character = characters.next();
                    } else if character == ')' {
                        tokens.push(Token::RightParen);
                        next_character = characters.next();
                    } else {
                        next_character = characters.next();
                    }
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
        let mut word = String::from(leading_character);

        for character in characters {
            if is_letter(character) {
                word.push(character);
            } else {
                return (Some(character), Token::Word(word));
            }
        }

        return (None, Token::Word(word));
    }

    fn lex_digit(
        leading_digit: char,
        characters: &mut impl Iterator<Item = char>,
    ) -> (Option<char>, Token) {
        let mut word = String::from(leading_digit);

        for character in characters {
            if is_digit(character) {
                word.push(character);
            } else {
                return (Some(character), Token::Integer(word.parse().unwrap()));
            }
        }

        return (None, Token::Word(word));
    }

    fn is_letter(character: char) -> bool {
        return character >= 'A' && character <= 'z';
    }

    fn is_digit(character: char) -> bool {
        return character >= '0' && character <= '9';
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
