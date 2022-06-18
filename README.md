# Rusthon - A Python compiler built in Rust

Rusthon is (well, at this point _will be_) a statically typed Python-like
language implemented in Rust. Rusthon is a learning project for me to
both learn the ins and outs of the Rust programming language, and to
rediscover my let's say _dormant_ skills in creating compilers and runtime
environments.

## Design

Rusthon follows the well-proven language implementation design of a compiler
that produces bytecode, which in turn is executed by an interpreter. To ease
debugging and learning, the bytecode format is human-readable rather than
in a binary format. Calling it bytecode is therefore a bit of a stretch, but
we'll go with this terminology anyway.

## Syntax

Leading up to the 0.1.0 release of Rusthon, the syntax is tiny. This is
currently all there is to it:

```
program ::= expression

expression ::= print ( INTEGER )

INTEGER ::= [0-9]+
```

Not aiming particularly high here, but this is sufficient to setup a barebones
compiler and runtime.

But what about the static type checking, you ask? This will work itself out
later as most of the time we will be able to infer types. Literal integers
for example are easily identifiable as just that.

## Progress toward 0.1.0

Rusthon 0.1.0 will be released once the entire compiler and runtime is fully
functional.

* Lexer - DONE
* Parser - DONE
* Type checker - N/A
    - Omitted from 0.1.0 as there is only one value type
* Code generation - TODO
* VM/Runtime - TODO
