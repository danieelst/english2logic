# Interpreter

Interprets constituency parses as first-order logic formulas, in accordance to Montague grammar.

The results of the interpretation of file `path/to/file.json` will be written to `path/to/_file.json`, with the following contents:

```
{
  "text": <<input text>>,
  "grammar": <<grammar trees>>,
  "logic": <<interpretations>>
}
```

## Grammar trees

A node can be one of two things:
  * Lexical nodes: hold a word with no sub-trees
  * Categorical nodes: hold a category, a list of sub-trees, and a list of categories corresponding to those of its direct sub-trees (useful for pattern rules)

Thus, all leafs will be lexical and all inner nodes will be categorical.

## First-order logic

The following is the current logic implemented:

| Identifier     | Symbolic    | Type                    | Constructor              |
|----------------|-------------|-------------------------|--------------------------|
| Predicates     | P(a,...,z)  | `Str -> [Str] -> Prop`  | `Pred "P" ["a",...,"z"]` |
| Negation       | ¬(Q)        | `Prop -> Prop`          | `Neg Q`                  |
| Conjunction    | (Q ∧ R)     | `Prop -> Prop -> Prop`  | `Conj Q R`               |
| Implication    | (Q → R)     | `Prop -> Prop -> Prop`  | `Impl Q R`               |
| Existential q. | ∃x[...x...] | `(Str -> Prop) -> Prop` | `Exists (\x -> ...x...)` |
| Universal q.   | ∀x[...x...] | `(Str -> Prop) -> Prop` | `ForAll (\x -> ...x...)` |

## Requirements

Cabal is installed and can be found with the command `cabal`.

## How to use

Build with `cabal build`.

Run with `cabal run eng2fol-interpreter -- path/to/parser/output/file`, where `path/to/parser/output/file` is a valid JSON-file.

## Testing

The test suite simply checks that all examples from `/examples/interpreter` are interpreted as the documented formulas and that the logic QuickCheck properties hold.

Run the tests with `cabal test --test-show-details=direct --verbose=0`.
