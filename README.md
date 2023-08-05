# 🇬🇧 English to First-Order Logic 💫

Parsing English text and interpreting it as first-order logic formulas can be done in accordance to [Montague grammar](https://en.wikipedia.org/wiki/Montague_grammar) using [Stanza](https://stanfordnlp.github.io/stanza/).

Consider the following examples of interpretations á la Montague grammar:

| Text           | Category | Constituents | Interpretation                    |
|----------------|----------|--------------|-----------------------------------|
| Nemo is a fish | `S`      | `NP` `VP`    | `∃x₀[(fish(x₀) ∧ is(x₀,"Nemo"))]` |
| Nemo           | `NP`     | `NNP`        | `λP → P("Nemo")`                  |
| is a fish      | `VP`     | `VBZ` `NP`   | `λx → ∃x₀[fish(x₀) ∧ is(x₀,x)]`   |
| is             | `VBZ`    |              | `λx → is(x)`                      |
| a fish         | `NP`     | `DT` `NN`    | `λQ λx → ∃x₀[fish(x₀) ∧ Q(x₀,x)]` |
| a              | `DT`     |              | `λP λQ λx → ∃x₀[P(x₀) ∧ Q(x₀,x)]` |
| fish           | `NN`     |              | `λx → fish(x)`                    |

## Parser

We can use Stanza's constituency parser to acquire the constituents of the provided text. The resulting grammar tree can be used for interpretation. See `/parser` for more information about the parser.

## Interpreter

The interpreter implements interpretation rules in accordance to Montague grammar. See `GRAMMAR.md` for an account of the currently available categories and interpretation rules. See `/interpreter` for more information about the interpreter.

## Examples

Interpreting `examples/parser/nemo-is-a-fish.json` gives the following output:

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

∃x₀[(fish(x₀) ∧ is(x₀,"Nemo"))]
```

Interpreting `examples/parser/no-fish-walks.json` gives the following output:

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

## Using the container

Build the image with `docker build -t english2logic .`. (Takes around 40 minutes.)

Then run a container using `docker run -it -v .:/english2logic english2logic`. You can skip the `-v` if you want to run the container in isolation.
