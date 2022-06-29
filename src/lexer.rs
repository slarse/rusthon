//! Rusthon's lexer: takes source code and produces a sequence of tokens.
//!
//! A lexer takes raw source code as input and produces an ordered sequence of tokens that are
//! easier to reason.

/// A token is the smallest significant unit of the Rusthon syntax.
#[derive(Debug, Eq, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Integer(i64),
    Identifier(String),
    Error(String),
    EOF,
}

/// A wrapper struct for a token that also contains a source code position. The
/// `position` member denotes the starting position of the token.
#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub position: u32,
}

/// Tokenize the provided `characters` into an iterator of [`Token`].
///
/// # Examples
///
/// ```
/// use rusthon::lexer;
/// use rusthon::lexer::TokenKind;
/// use rusthon::lexer::Token;
///
/// let input = "print(1)".to_string();
/// let expected_tokenization = vec![
///     Token { position: 0, kind: TokenKind::Identifier("print".to_string()) },
///     Token { position: 5, kind: TokenKind::LeftParen },
///     Token { position: 6, kind: TokenKind::Integer(1) },
///     Token { position: 7, kind: TokenKind::RightParen },
/// ];
///
/// let actual_tokenization: Vec<Token> = lexer::tokenize(input.chars()).collect();
///
/// assert_eq!(actual_tokenization.len(), expected_tokenization.len());
/// actual_tokenization
///     .iter()
///     .zip(expected_tokenization.iter())
///     .for_each(|(actual_token, expected_token)| assert_eq!(actual_token, expected_token));
/// ```
pub fn tokenize(characters: impl Iterator<Item = char>) -> impl Iterator<Item = Token> {
    Tokenizer {
        characters: PositionedCharacterIterator {
            characters,
            position: 0,
            peeked: None,
        },
    }
}

struct PositionedCharacterIterator<I> {
    characters: I,
    position: u32,
    peeked: Option<char>,
}

impl<I: Iterator<Item = char>> Iterator for PositionedCharacterIterator<I> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.peek();
        match self.peeked {
            Some(_) => {
                self.position += 1;
                let character = self.peeked.unwrap();
                self.peeked = None;
                Some(character)
            }
            None => None,
        }
    }
}

impl<I: Iterator<Item = char>> PositionedCharacterIterator<I> {
    fn peek(&mut self) -> Option<&char> {
        if self.peeked.is_some() {
            self.peeked.as_ref()
        } else {
            self.peeked = self.characters.next();
            self.peeked.as_ref()
        }
    }
}

struct Tokenizer<I> {
    characters: I,
}

impl<I: Iterator<Item = char>> Iterator for Tokenizer<PositionedCharacterIterator<I>> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let position = self.characters.position;

        let kind = match self.characters.peek() {
            Some('A'..='z') => {
                let kind = lex_word(&mut self.characters);
                kind
            }
            Some('0'..='9') => {
                let kind = lex_digit(&mut self.characters);
                kind
            }
            Some('(') => {
                self.characters.next();
                TokenKind::LeftParen
            }
            Some(')') => {
                self.characters.next();
                TokenKind::RightParen
            }
            Some(_) => {
                let error = self.characters.next().unwrap();
                TokenKind::Error(error.to_string())
            }
            None => return None,
        };

        Some(Token { kind, position })
    }
}

fn lex_word(characters: &mut PositionedCharacterIterator<impl Iterator<Item = char>>) -> TokenKind {
    let to_word_token = |text: String| TokenKind::Identifier(text);

    let is_letter = |character: &char| ('A'..'z').contains(&character);

    return lex_token(&is_letter, &to_word_token, characters);
}

fn lex_digit(
    characters: &mut PositionedCharacterIterator<impl Iterator<Item = char>>,
) -> TokenKind {
    let to_integer_token = |text: String| TokenKind::Integer(text.parse().unwrap());

    let is_digit = |character: &char| ('0'..='9').contains(&character);

    return lex_token(&is_digit, &to_integer_token, characters);
}

fn lex_token(
    should_include: &dyn Fn(&char) -> bool,
    to_token: &dyn Fn(String) -> TokenKind,
    characters: &mut PositionedCharacterIterator<impl Iterator<Item = char>>,
) -> TokenKind {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Debug;

    #[test]
    fn lex_print_1() {
        let input = "print(1)".to_string();
        let expected_tokenization = vec![
            Token {
                position: 0,
                kind: TokenKind::Identifier("print".to_string()),
            },
            Token {
                position: 5,
                kind: TokenKind::LeftParen,
            },
            Token {
                position: 6,
                kind: TokenKind::Integer(1),
            },
            Token {
                position: 7,
                kind: TokenKind::RightParen,
            },
        ];

        let actual_tokenization = tokenize(input.chars());

        assert_vectors_equal(&actual_tokenization.collect(), &expected_tokenization);
    }

    #[test]
    fn lex_bad_character() {
        let input = "print(;)";
        let expected_tokenization = vec![
            Token {
                position: 0,
                kind: TokenKind::Identifier("print".to_string()),
            },
            Token {
                position: 5,
                kind: TokenKind::LeftParen,
            },
            Token {
                position: 6,
                kind: TokenKind::Error(";".to_string()),
            },
            Token {
                position: 7,
                kind: TokenKind::RightParen,
            },
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
