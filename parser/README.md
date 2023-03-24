# Parser

Performs a constituency parse on provided English text, using Stanza.

The parser expects to be provided with a JSON file á la:

```
{
  "input": <<Text to be parsed.>>,
  ...
}
```

The output of the parse will then be added to this file. (Note: the whole file will be rewritten!)

## Requirements

Python is installed and can be found with the command `python3`. PIP is installed and can be found with the command `pip`.

## How to use

Install prerequisites with `pip install -r requirements.txt` and download Stanza resources with `python3 src/download.py`, or just use `install.sh`.

Run with `python3 src/main.py -f path/to/json/file`.
