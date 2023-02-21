from parsing import parse_as_json
from pathlib import Path
from os.path import exists
import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument("-in",  "--input",  help="Path to input file")
parser.add_argument("-out", "--output", help="Path to output folder")

DEFAULT_OUTPUT_PATH = r'output'

if __name__=='__main__':
  args = parser.parse_args()

  if not args.input:
    print('Input is missing!')
    sys.exit(-1)

  if not args.input.lower().endswith('.txt'):
    print('Input is not a text file!')
    sys.exit(-1)

  if args.output:
    # If an argument is passed on command-line, set it as path
    output_path = Path(args.output)
  else:
    # If not, then just run with default
    output_path = Path(DEFAULT_OUTPUT_PATH)

  output_file = output_path / (Path(args.input).stem + '.json')

  if exists(output_file):
    print('Output file ' + str(output_file.resolve()) + ' already exists!')
    sys.exit(-1)

  text = ''
  with open(args.input, 'r') as f:
    text = f.readlines()
    text = [(' ' if c=='\n' else c) for line in text for c in line]
    text = ''.join(text)

  output = parse_as_json(text)

  output_path.mkdir(parents=True, exist_ok=True)

  with open(output_file, 'w') as f:
    print('\nWriting to ' + str(output_file.resolve()))
    f.write(output)
