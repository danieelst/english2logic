module Semantics.NNP where

import Grammar
import GrammarTree
import Semantics.Util
import Logic

-- Interpret a proper noun as a higher-order proposition
i :: GrammarTree -> (Predicate -> Prop)
i (CategoryNode NNP [] ts) = \p -> p $ [word $ p1 ts]
