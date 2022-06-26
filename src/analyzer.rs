//! Rusthon's name analyzer: takes an AST and produces a graph with identifier
//! uses connected to their definitions.
//!
//! A name analyzer is used to connect a use of an identifier to its definition.
//! The archetypical example is a function invocation `func(a, b)`. The name
//! analyzer is responsible for finding the definition of the identifiers
//! `func`, `a` and `b`. This will then allow the type checker to verify
//! type integrity, and the code generator to produce executable code
//! with correct jumps and references.

use crate::parser::Program;

/**
 * Perform name analysis on an AST.
 *
 * Currently, Rusthon is so simple that name analysis is in fact not necessary.
 * So we currently do nothing at all!
 */
pub fn analyze_names(program: Program) -> Program {
    program
}
