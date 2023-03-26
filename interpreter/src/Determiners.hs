module Determiners(quantifier,Quantifier(..)) where

import qualified Logic as Logic

import Data.Char(toLower)
import Data.Map(Map)
import qualified Data.Map as Map

-- Just a little helper data type for pattern matching on quantifiers
data Quantifier = Exists    -- Existential
                | ForAll    -- Universal
                | NegForAll -- Negated universal quantification
  deriving (Show,Eq)

-- A simple lexicon mapping determiners to quantifiers
determiners :: Map String Quantifier
determiners = Map.fromList
  [("a"    , Exists   ),
   ("an"   , Exists   ),
   ("the"  , Exists   ),
   ("every", ForAll   ),
   ("no"   , NegForAll)]

toLogic :: Quantifier -> ((Logic.Ind -> Logic.Prop) -> Logic.Prop)
toLogic (Exists) = \f -> Logic.Exists f
toLogic (ForAll) = \f -> Logic.ForAll f

quantifier :: String -> Quantifier
quantifier determiner = determiners Map.! (map toLower determiner)

