module Semantics(interpret) where

import Prelude hiding (fst,snd)
import Grammar
import GrammarTree(GrammarTree(..))
import Logic
import qualified Determiners as Det

interpret :: GrammarTree -> Prop
interpret (CategoryNode ROOT [S] [t]) = iS t
interpret _ = error "Interpretation must start from ROOT and contain a sentence"

-------------------------------------------------------------------------------
-- Internal interpretation functions
-------------------------------------------------------------------------------

-- Interpret a sentence
iS :: GrammarTree -> Prop
iS (CategoryNode S [NP,VP] ts) = (iNP $ fst ts) (iVP $ snd ts)

-- Interpret a noun phrase
iNP :: GrammarTree -> (Function -> Prop)
iNP (CategoryNode NP [NNP] ts)   = iNNP $ fst ts
iNP (CategoryNode NP [DT,NN] ts) = (iDT $ fst ts) (iNN $ snd ts)

-- Interpret a proper noun
iNNP :: GrammarTree -> (Function -> Prop)
iNNP (CategoryNode NNP [] ts) = \p -> p $ [iT $ fst ts]

-- Interpret a common noun
iNN :: GrammarTree -> Function
iNN (CategoryNode NN [] ts) = \x -> Pred (iT $ fst ts) x

-- Interpret a verb phrase
iVP :: GrammarTree -> Function
iVP (CategoryNode VP [VBZ]    ts) = iVBZ $ fst ts
iVP (CategoryNode VP [VBZ,NP] ts) = (iNPVP $ snd ts) (iVBZ $ fst ts)

-- Interpret a noun phrase used in a verb phrase
iNPVP :: GrammarTree -> (Function -> Function)
iNPVP (CategoryNode NP [NN] ts)    = (iNN2 $ fst ts)
iNPVP (CategoryNode NP [DT,NN] ts) = (iDTNPVP $ fst ts) (iNN $ snd ts)

-- Interpret a common noun as a word in a predicate
iNN2 :: GrammarTree -> (Function -> Function)
iNN2 (CategoryNode NN [] ts) = \p -> p <+ (iT $ fst ts)

iDT :: GrammarTree -> (Function -> (Function -> Prop))
iDT (CategoryNode DT [] ts) = case (Det.quantifier $ iT $ fst ts) of
  Det.Exists -> \p q -> Exists (\x -> Conj (p x) (q x))
  Det.ForAll -> \p q -> ForAll (\x -> Impl (p x) (q x))

-- Interpret a determiner used in a noun phrase in a verb phrase
iDTNPVP :: GrammarTree -> (Function -> Function -> Function)
iDTNPVP (CategoryNode DT [] ts) = case (Det.quantifier $ iT $ fst ts) of
  Det.Exists -> \p q x -> Exists (\y -> Conj (p y) (q $ y ++ x))
  Det.ForAll -> \p q x -> ForAll (\y -> Impl (p y) (q $ y ++ x))

-- Interpret a verbalizer
iVBZ :: GrammarTree -> Function
iVBZ (CategoryNode VBZ [] ts) = \x -> Pred (iT $ fst ts) x

-- Interpret a lexeme as an individual
iT :: GrammarTree -> Ind
iT (LexemeNode lex) = lex

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

-- First item in a list
fst :: [a] -> a
fst = head

-- Second item in a list
snd :: [a] -> a
snd = head . (drop 1)

-- Inserts an argument in an argument chain to a function
(<+) :: Function -> Ind -> Function
(<+) f x = \xs -> f (xs ++ [x])
