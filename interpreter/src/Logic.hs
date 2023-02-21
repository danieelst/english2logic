module Logic(Prop(..),Ind,Function) where

type Name = String
type Ind  = String
type Function = ([Ind] -> Prop)

data Prop = Pred   Name [Ind] -- P(a,...,z)
          | Conj   Prop Prop  -- P ∧ Q
          | Impl   Prop Prop  -- P → Q
          | Exists Function   -- ∃x[...x...]
          | ForAll Function   -- ∀x[...x...]

instance Show Prop where
  show (Pred name args) = name ++ (pth $ foldr1 (\a b -> a ++ "," ++ b) args)
  show (Conj p1 p2)     = pth $ show p1 ++ " ∧ " ++ show p2
  show (Impl p1 p2)     = pth $ show p1 ++ " → " ++ show p2
  show (Exists f)       = "∃x" ++ (brkt $ show $ f ["x"])
  show (ForAll f)       = "∀x" ++ (brkt $ show $ f ["x"])

-- s -> (s)
pth :: String -> String
pth s = "(" ++ s ++ ")"

-- s -> [s]
brkt :: String -> String
brkt s = "[" ++ s ++ "]"
