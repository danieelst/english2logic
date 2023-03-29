module Semantics.S where

import Grammar
import GrammarTree
import Semantics.Util
import Logic.Prop

import qualified Semantics.NP as NP
import qualified Semantics.VP as VP

-- Interpret a sentence as a formula
i :: GrammarTree -> Prop
i (CategoryNode S [NP,VP] ts) = (NP.i $ p1 ts) (VP.i $ p2 ts)
