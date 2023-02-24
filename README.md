# 🇬🇧 English to First-Order Logic 💫

Parsing English text and interpreting it as first-order logic formulas in accordance to Montague grammar.

## Parser

The parser performs a constituency parse on English text, using Stanza and Python. The output of the parse is provided as a JSON-file.

For example, parsing `examples/nemo-is-a-fish.txt` outputs `examples/nemo-is-a-fish.json`.

## Interpreter

Interprets a constituency parse as a first-order logic formula, using Montague grammar and Haskell.

### Current interpretation rules

The following grammar rules are currently possible to interpret:

```
S  <- NP VP   # Nemo is a fish
NP <- NNP     # Nemo
NP <- DT NN   # a fish
NP <- NN      # fish
VP <- VBZ     # is
VP <- VBZ NP  # is a fish
```

### Determiner lexicon

Since determiners are interpreted as quantifiers, we have to build a lexicon of determiners mapped to the desired quantifier. The following determiners are currently added:

| Quantifier  | Mappings   |
|-------------|------------|
| Existential | A, an, the |
| Universal   | Every      |

### Example

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

## Building

Install all prerequisites and build the project using `install.sh`.

## Running

Run the full program (parser and interpreter) with `run.sh`.
