module Logic.Prop(Prop(..),Ind,Predicate,Name) where

type Name = String
type Ind  = String
type Predicate = ([Ind] -> Prop)

data Prop = Pred   Name [Ind]    -- P(a,...,z)
          | Neg    Prop          -- ¬ P
          | Conj   Prop Prop     -- P ∧ Q
          | Impl   Prop Prop     -- P → Q
          | Exists (Ind -> Prop) -- ∃x[...x...]
          | ForAll (Ind -> Prop) -- ∀x[...x...]

instance Eq Prop where
  (==) p q = eqProp p q 0

eqProp :: Prop -> Prop -> Int -> Bool
eqProp (Pred n1 xs1) (Pred n2 xs2) _ = n1 == n2 && xs1 == xs2
eqProp (Neg  p)      (Neg  q)      x = eqProp p q x
eqProp (Conj p1 p2)  (Conj q1 q2)  x = eqProp p1 q1 x && eqProp p2 q2 x
eqProp (Impl p1 p2)  (Impl q1 q2)  x = eqProp p1 q1 x && eqProp p2 q2 x
eqProp (Exists f1)   (Exists f2)   x = eqProp (f1 $ var x) (f2 $ var x) (x+1)
eqProp (ForAll f1)   (ForAll f2)   x = eqProp (f1 $ var x) (f2 $ var x) (x+1)
eqProp _ _                         x = False

instance Show Prop where
  show = prop2Str

prop2Str :: Prop -> String
prop2Str = p2S 0
  where
    p2S :: Int -> Prop -> String
    p2S x (Pred name args) = name ++ pth (foldr1 (\a b -> a ++ "," ++ b)
                                         $ map (\x -> if isVar x
                                                        then x
                                                        else quote x) args)
    p2S x (Neg p)          = "¬" ++ pth (p2S x p)
    p2S x (Conj p1 p2)     = pth $ p2S x p1 ++ " ∧ " ++ p2S x p2
    p2S x (Impl p1 p2)     = pth $ p2S x p1 ++ " → " ++ p2S x p2
    p2S x (Exists f)       = "∃" ++ var x ++ brkt (p2S (x+1) $ f $ var x)
    p2S x (ForAll f)       = "∀" ++ var x ++ brkt (p2S (x+1) $ f $ var x)

subs :: [Char]
subs = "₀₁₂₃₄₅₆₇₈₉"

-- var 0 -> x₀, var 21 -> x₂₁
var :: Int -> String
var x = "x" ++ getSubscript x
  where
    getSubscript :: Int -> String
    getSubscript i | i < 0     = ""
                   | i < 10    = [subs !! i]
                   | otherwise = getSubscript (i `div` 10) ++ getSubscript (i `mod` 10)

-- A little naive check, but should be mostly functional
isVar :: String -> Bool
isVar s = or [c `elem` subs | c <- s]

-- "s" -> "(s)"
pth :: String -> String
pth s = "(" ++ s ++ ")"

-- "s" -> "[s]"
brkt :: String -> String
brkt s = "[" ++ s ++ "]"

quote :: String -> String
quote s = "\"" ++ s ++ "\""
