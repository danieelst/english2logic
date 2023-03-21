module Semantics.VP where

import Grammar
import GrammarTree
import Semantics.Util
import Logic

import qualified Semantics.VBZ as VBZ
import qualified Semantics.NP as NP

-- Interpret a verb phrase as a predicate
i :: GrammarTree -> Predicate
i (CategoryNode VP [VBZ]    ts) = VBZ.i $ p1 ts
i (CategoryNode VP [VBZ,NP] ts) = (NP.i2 $ p2 ts) (VBZ.i $ p1 ts)
