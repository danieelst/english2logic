module LogicTest(test) where

import Logic.Parser(parse)
import Logic.Gen(Prop)
import Test.QuickCheck(quickCheck)

test :: IO ()
test = do
  putStrLn "Testing reflexive equality of propositional formulas"
  quickCheck prop_ReflexiveEquality
  putStrLn "\nTesting parsing/printing-symmetry of propositional formulas"
  quickCheck prop_ParseShowSymmetry
  putStrLn "\nTesting that no spacing in the stringified propositional formulas is okay"
  quickCheck prop_NoSpacingIsOK
  putStrLn "\nTesting that extra spacing in the stringified propositional formulas is okay"
  quickCheck prop_ExcessSpacingIsOK

prop_ReflexiveEquality :: Prop -> Bool
prop_ReflexiveEquality p = p == p

prop_ParseShowSymmetry :: Prop -> Bool
prop_ParseShowSymmetry p = p == parse (show p)

-- This test works because the generated individuals will never have spacing
prop_NoSpacingIsOK :: Prop -> Bool
prop_NoSpacingIsOK p = p == parse [c | c <- show p, c/=' ']

prop_ExcessSpacingIsOK :: Prop -> Int -> Bool
prop_ExcessSpacingIsOK p n = p == parse s'
  where
    s = show p
    idx = n `mod` length s
    (lh,rh) = splitAt idx s
    s' = lh ++ " " ++ rh
