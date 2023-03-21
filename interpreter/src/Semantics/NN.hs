module Semantics.NN where

import Grammar
import GrammarTree
import Semantics.Util
import Logic

-- Interpret a common noun as a predicate
i :: GrammarTree -> Predicate
i (CategoryNode NN [] ts) = \x -> Pred (word $ p1 ts) x

-- Interpret a common noun as a word in a predicate
-- i.e. i2 x = \P -> P(...,x)
i2 :: GrammarTree -> (Predicate -> Predicate)
i2 (CategoryNode NN [] ts) = \p -> p <+ (word $ p1 ts)
