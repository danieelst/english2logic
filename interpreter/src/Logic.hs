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
  show = prop2Str

prop2Str :: Prop -> String
prop2Str = p2S 0
  where
    p2S :: Int -> Prop -> String
    p2S x (Pred name args) = name ++ (pth $ foldr1 (\a b -> a ++ "," ++ b) args)
    p2S x (Conj p1 p2)     = pth $ p2S x p1 ++ " ∧ " ++ p2S x p2
    p2S x (Impl p1 p2)     = pth $ p2S x p1 ++ " → " ++ p2S x p2
    p2S x (Exists f)       = "∃" ++ var x ++ (brkt $ p2S (x+1) $ f [var x])
    p2S x (ForAll f)       = "∀" ++ var x ++ (brkt $ p2S (x+1) $ f [var x])

-- var 0 -> x₀, var 21 -> x₂₁
var :: Int -> String
var x = "x" ++ getSubscript x
  where
    getSubscript :: Int -> String
    getSubscript i | i < 0     = ""
                   | i < 10    = (subs !! i) : []
                   | otherwise = getSubscript (i `div` 10) ++ getSubscript (i `mod` 10)
    subs :: [Char]
    subs = "₀₁₂₃₄₅₆₇₈₉"

-- s -> (s)
pth :: String -> String
pth s = "(" ++ s ++ ")"

-- s -> [s]
brkt :: String -> String
brkt s = "[" ++ s ++ "]"
