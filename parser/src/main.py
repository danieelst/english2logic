from parsing import parse_as_json
from os.path import exists
from json import load
from pathlib import Path
import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument("-f", "--file", help="Path to file")

if __name__=='__main__':
  args = parser.parse_args()

  path_to_file = args.file

  if not path_to_file:
    print('Missing argument -f!')
    sys.exit(-1)

  if not path_to_file.lower().endswith('.json'):
    print('Input is not a JSON file!')
    sys.exit(-1)

  if not exists(path_to_file):
    print('File does not exist!')
    sys.exit(-1)

  text = ''
  with open(path_to_file, 'r') as f:
    data = load(f)
    text = data['input']

  output = parse_as_json(text)

  with open(path_to_file, 'w') as f:
    print('\nWriting to ' + str(Path(path_to_file).resolve()))
    f.write(output)
