# 🇬🇧 English to First-Order Logic 💫

Parsing English text and interpreting it as first-order logic formulas in accordance to [Montague grammar](https://en.wikipedia.org/wiki/Montague_grammar).

## Parser

The parser performs a constituency parse on English text, using Stanza. The input should be a valid JSON-file, and the output will be written to the same file.

## Interpreter

Interprets a constituency parse as a first-order logic formula, using Montague grammar and Haskell.

### Current interpretation rules

The following grammar rules are currently possible to interpret:

| Category | Grammar rule   | Example        |
|----------|----------------|----------------|
| S        | NP VP          | Nemo is a fish |
| NP       | NNP            | Nemo           |
| NP       | DT NN          | a fish         |
| NP       | NN             | fish           |
| VP       | VBZ            | swims          |
| VP       | VBZ NP         | is a fish      |

### Determiner lexicon

Since determiners are interpreted as quantifiers, we have to build a lexicon of determiners mapped to the desired quantifier. The following determiners are currently added:

| Quantifier        | Mappings   |
|-------------------|------------|
| Existential       | A, an, the |
| Universal         | Every      |
| Negated universal | No         |

### Examples

Interpreting `examples/nemo-is-a-fish.json` gives the following output:

```
Nemo is a fish.

ROOT
|
`- S
   |
   +- NP
   |  |
   |  `- NNP
   |     |
   |     `- Nemo
   |
   `- VP
      |
      +- VBZ
      |  |
      |  `- is
      |
      `- NP
         |
         +- DT
         |  |
         |  `- a
         |
         `- NN
            |
            `- fish

∃x₀[(fish(x₀) ∧ is(x₀,Nemo))]
```

Interpreting `examples/no-fish-walks.json` gives the following output:

```
No fish walks.

ROOT
|
`- S
   |
   +- NP
   |  |
   |  +- DT
   |  |  |
   |  |  `- No
   |  |
   |  `- NN
   |     |
   |     `- fish
   |
   `- VP
      |
      `- VBZ
         |
         `- walks

∀x₀[(fish(x₀) → ¬(walks(x₀)))]
```

## Building

Install all prerequisites and build the project using `install.sh`.

## Running

Run the full program (parser and interpreter) with `run.sh path/to/json/file`.
