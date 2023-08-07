module Semantics.NP where

import Grammar
import GrammarTree
import Semantics.Util
import Logic.Prop

import qualified Semantics.NNP as NNP
import qualified Semantics.NN as NN
import qualified Semantics.DT as DT

-- Interpret a noun phrase as a higher-order proposition
i :: GrammarTree -> (Predicate -> Prop)
i (CategoryNode NP [NNP]   ts) = NNP.i $ p1 ts
i (CategoryNode NP [DT,NN] ts) = (DT.i $ p1 ts) (NN.i $ p2 ts)

-- Interpret a noun phrase used in a verb phrase
i2 :: GrammarTree -> (Predicate -> Predicate)
i2 (CategoryNode NP [NN]    ts) = NN.i2 $ p1 ts
i2 (CategoryNode NP [DT,NN] ts) = (DT.i2 $ p1 ts) (NN.i $ p2 ts)

patterns :: [[Category]]
patterns = [[NNP]
           ,[DT,NN]
           ,[NN]]
