//! Rusthon's lexer: takes source code and produces a sequence of tokens.
//!
//! A lexer takes raw source code as input and produces an ordered sequence of tokens that are
//! easier to reason.

use std::iter::Peekable;

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
///
/// let input = "print(1)".to_string();
/// let expected_tokenization = vec![
///     TokenKind::Identifier("print".to_string()),
///     TokenKind::LeftParen,
///     TokenKind::Integer(1),
///     TokenKind::RightParen,
/// ];
///
/// // The position in `Token` currently isn't usable, so we ignore it
/// let actual_tokenization: Vec<TokenKind> = lexer::tokenize(input.chars()).map(|ti| ti.kind).collect();
///
/// assert_eq!(actual_tokenization.len(), expected_tokenization.len());
/// actual_tokenization
///     .iter()
///     .zip(expected_tokenization.iter())
///     .for_each(|(actual_token, expected_token)| assert_eq!(actual_token, expected_token));
/// ```
pub fn tokenize(characters: impl Iterator<Item = char>) -> impl Iterator<Item = Token> {
    Tokenizer {
        characters: characters.peekable(),
    }
}

struct Tokenizer<I> {
    characters: I,
}

impl<I: Iterator<Item = char>> Iterator for Tokenizer<Peekable<I>> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
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

        // TODO make position "real"
        Some(Token { kind, position: 0 })
    }
}

fn lex_word(characters: &mut Peekable<impl Iterator<Item = char>>) -> TokenKind {
    let to_word_token = |text: String| TokenKind::Identifier(text);

    let is_letter = |character: &char| ('A'..'z').contains(&character);

    return lex_token(&is_letter, &to_word_token, characters);
}

fn lex_digit(characters: &mut Peekable<impl Iterator<Item = char>>) -> TokenKind {
    let to_integer_token = |text: String| TokenKind::Integer(text.parse().unwrap());

    let is_digit = |character: &char| ('0'..='9').contains(&character);

    return lex_token(&is_digit, &to_integer_token, characters);
}

fn lex_token(
    should_include: &dyn Fn(&char) -> bool,
    to_token: &dyn Fn(String) -> TokenKind,
    characters: &mut Peekable<impl Iterator<Item = char>>,
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
            TokenKind::Identifier("print".to_string()),
            TokenKind::LeftParen,
            TokenKind::Integer(1),
            TokenKind::RightParen,
        ];

        let actual_tokenization = tokenize(input.chars()).map(|ti| ti.kind);

        assert_vectors_equal(&actual_tokenization.collect(), &expected_tokenization);
    }

    #[test]
    fn lex_bad_character() {
        let input = "print(;)";
        let expected_tokenization = vec![
            TokenKind::Identifier("print".to_string()),
            TokenKind::LeftParen,
            TokenKind::Error(";".to_string()),
            TokenKind::RightParen,
        ];

        let actual_tokenization = tokenize(input.chars()).map(|ti| ti.kind);

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
