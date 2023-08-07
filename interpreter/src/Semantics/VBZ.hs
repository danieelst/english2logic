module Semantics.VBZ where

import Grammar
import GrammarTree
import Semantics.Util
import Logic.Prop

-- Interpret a verbalizer as a predicate
i :: GrammarTree -> Predicate
i (CategoryNode VBZ [] ts) = Pred (word $ p1 ts)

patterns :: [[Category]]
patterns = []

