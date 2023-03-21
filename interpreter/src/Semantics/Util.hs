module Semantics.Util where

import Logic
import GrammarTree(GrammarTree(LexemeNode))

-- First projection
p1 :: [a] -> a
p1 = head

-- Second projection
p2 :: [a] -> a
p2 = head . (drop 1)

-- Inserts an argument in an argument chain to a predicate
(<+) :: Predicate -> Ind -> Predicate
(<+) f x = \xs -> f (xs ++ [x])

-- Interpret a lexeme as an individual
word :: GrammarTree -> Ind
word (LexemeNode x) = x
