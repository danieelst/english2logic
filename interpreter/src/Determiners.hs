module Determiners(quantifier,Quantifier(..),table) where

import qualified Logic.Prop as Logic

import Data.Char(toLower)
import Data.Map(Map)
import qualified Data.Map as Map

-- Just a little helper data type for pattern matching on quantifiers
data Quantifier = Exists    -- Existential
                | ForAll    -- Universal
                | NegForAll -- Negated universal quantification
  deriving (Eq)

instance Show Quantifier where
  show qtf = case qtf of
    Exists    -> "Existential"
    ForAll    -> "Universal"
    NegForAll -> "Negated universal"

-- A simple lexicon mapping determiners to quantifiers
determiners :: Map String Quantifier
determiners = Map.fromList table

table :: [(String,Quantifier)]
table = [("a"    , Exists   ),
         ("an"   , Exists   ),
         ("the"  , Exists   ),
         ("every", ForAll   ),
         ("no"   , NegForAll)]

toLogic :: Quantifier -> ((Logic.Ind -> Logic.Prop) -> Logic.Prop)
toLogic Exists = Logic.Exists
toLogic ForAll = Logic.ForAll

quantifier :: String -> Quantifier
quantifier determiner = determiners Map.! map toLower determiner
