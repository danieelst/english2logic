import stanza
from stanza.pipeline.core import DownloadMethod
from stanza.models.constituency.parse_tree import Tree as ParseTree
from json import dumps
from settings import LANG

nlp = stanza.Pipeline(LANG,
                      processors='tokenize,pos,constituency',
                      download_method=DownloadMethod.REUSE_RESOURCES)

# Take a text and parse it as a tree
def parse_as_trees(text):
  doc = nlp(text)
  return [remove_punctuation(sentence.constituency)
          for sentence
          in doc.sentences]

# Remove punctuation from the parse tree.
def remove_punctuation(tree):
  return ParseTree(tree.label, [remove_punctuation(child)
                                for child
                                in tree.children
                                if child.label != '.' and child.label != ','])

# Turn a tree structure into a Python dictionary
def tree_to_dict(tree):
  return {
    'node'     : tree.label,
    'children' : [tree_to_dict(child) for child in tree.children]
  }

# Take a text, parse it and return JSON-data
def parse_as_json(text):
  trees = parse_as_trees(text)
  return dumps({'input':  text,
                'output': [tree_to_dict(tree) for tree in trees]},
               indent=2)
