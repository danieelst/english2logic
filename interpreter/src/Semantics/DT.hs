module Semantics.DT where

import Grammar
import GrammarTree
import Semantics.Util
import Logic.Prop

import qualified Determiners as Det

-- Interpret a determiner in the general sense
i :: GrammarTree -> (Predicate -> (Predicate -> Prop))
i (CategoryNode DT [] ts) = case (Det.quantifier $ word $ p1 ts) of
  Det.Exists    -> \p q -> Exists (\x -> Conj (p [x]) (q [x]))
  Det.ForAll    -> \p q -> ForAll (\x -> Impl (p [x]) (q [x]))
  Det.NegForAll -> \p q -> ForAll (\x -> Impl (p [x]) (Neg $ q [x]))

-- Interpret a determiner as a predicate which takes two predicates
-- Used when e.g. the determiner is in a noun phrase in a verb phrase
i2 :: GrammarTree -> (Predicate -> Predicate -> Predicate)
i2 (CategoryNode DT [] ts) = case (Det.quantifier $ word $ p1 ts) of
  Det.Exists    -> \p q x -> Exists (\y -> Conj (p [y]) (      q $ y:x))
  Det.ForAll    -> \p q x -> ForAll (\y -> Impl (p [y]) (      q $ y:x))
  Det.NegForAll -> \p q x -> ForAll (\y -> Impl (p [y]) (Neg $ q $ y:x))

patterns :: [[Category]]
patterns = []
