module Logic.Gen(Prop(..)) where

import Logic.Prop(Prop(..),Ind,Name)

import Test.QuickCheck

-- Generate a "general" proposition
-- For simplicification at loss of generality:
--  * Predicates will only either be P, Q or R.
--  * Individuals will only either be x, y or z.
--  * Quantifiers will always include one predicate
--    applied to the quantifed variables.
instance Arbitrary Prop where
  arbitrary = sized arbProp

genName :: Gen Name
genName = elements ["P","Q","R"]

arbArgs :: Gen [Ind]
arbArgs = elements [["x"],["y"],["z"]]

arbProp :: Int -> Gen Prop
arbProp s = frequency [
    (1, Pred <$> genName <*> arbArgs),
    (s, do
          p <- arbProp s'
          return $ Neg p),
    (s, do
          p1 <- arbProp s'
          p2 <- arbProp s'
          conn <- elements [Conj,Impl]
          return $ conn p1 p2),
    (s, do
          conn <- elements [Conj,Impl]
          qtf  <- elements [Exists,ForAll]
          p    <- arbProp s'
          name <- genName
          e    <- elements [
            \x -> Pred name [x],
            \x -> conn (Pred name [x]) p,
            \x -> conn p (Pred name [x])]
          return $ qtf e)
  ]
  where s' = s `div` 2
