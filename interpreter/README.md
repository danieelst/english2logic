# Interpreter

Interprets constituency parses as first-order logic formulas, in accordance to Montague grammar.

## Requirements

Cabal is installed and can be found with the command `cabal`.

## How to use

Build with `cabal build`.

Run with `cabal run eng2fol-interpreter -- path/to/parser/output/file`, where `path/to/parser/output/file` is a valid JSON-file.

## Testing

The test suite simply checks that all examples from `/examples` are interpreted as valid first-order propositional formulas (i.e. without any errors happening).

Run the tests with `cabal test --test-show-details=direct --verbose=0`.