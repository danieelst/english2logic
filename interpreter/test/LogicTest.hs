module LogicTest where

import Logic(Prop,parse)
import Test.QuickCheck(quickCheck)

test :: IO ()
test = do
  putStrLn "Testing reflexive equality of propositional formulas"
  quickCheck prop_ReflexiveEquality
  putStrLn "\nTesting parsing/printing-symmetry of propositional formulas"
  quickCheck prop_ParseShowSymmetry

prop_ReflexiveEquality :: Prop -> Bool
prop_ReflexiveEquality p = p == p

prop_ParseShowSymmetry :: Prop -> Bool
prop_ParseShowSymmetry p = p == parse (show p)
