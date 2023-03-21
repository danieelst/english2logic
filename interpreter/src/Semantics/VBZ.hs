module Semantics.VBZ where

import Grammar
import GrammarTree
import Semantics.Util
import Logic

-- Interpret a verbalizer as a predicate
i :: GrammarTree -> Predicate
i (CategoryNode VBZ [] ts) = \x -> Pred (word $ p1 ts) x
