# English to First-Order Logic

Parsing English text as first-order logic according to Montague grammar.

## Parser

The parser performs a constituency parse on English text, using Stanza and Python. The output of the parse is provided as a JSON-file.

For example, parsing `examples/nemo-is-a-fish.txt` outputs `examples/nemo-is-a-fish.json`.

## Interpreter

Interprets a constituency parse as first-order logic, using Montague grammar and Haskell.

### Current intrepretation rules

The following grammar rules are currently possible to interpret:

```
S  <- NP VP
NP <- NNP
NP <- DT NN
VP <- VBZ
VP <- VBZ NP
```

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

∃x[(fish(x) ∧ is(x,Nemo))]
```

## Building

Install all prerequisites and build the project using `install.sh`.

## Running

Run the full program (parser and interpreter) with `run.sh`.
