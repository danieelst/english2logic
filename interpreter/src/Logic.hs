module Logic(Prop(..),Ind,Predicate,parse) where

import Test.QuickCheck

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
  (==) = \p q -> eqProp p q 0

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
    p2S x (Pred name args) = name ++ (pth $ foldr1 (\a b -> a ++ "," ++ b) args)
    p2S x (Neg p)          = "¬" ++ (pth $ p2S x p)
    p2S x (Conj p1 p2)     = pth $ p2S x p1 ++ " ∧ " ++ p2S x p2
    p2S x (Impl p1 p2)     = pth $ p2S x p1 ++ " → " ++ p2S x p2
    p2S x (Exists f)       = "∃" ++ var x ++ (brkt $ p2S (x+1) $ f $ var x)
    p2S x (ForAll f)       = "∀" ++ var x ++ (brkt $ p2S (x+1) $ f $ var x)

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

-- "s" -> "(s)"
pth :: String -> String
pth s = "(" ++ s ++ ")"

-- "s" -> "[s]"
brkt :: String -> String
brkt s = "[" ++ s ++ "]"

-- Parse a string of symbols into a proposition (if valid)
parse :: String -> Prop
parse s = case head s of
  '¬'       -> Neg    $ parse $ init $ tail $ tail s
  '∃'       -> Exists $ parseQuantifier s
  '∀'       -> ForAll $ parseQuantifier s
  '('       -> parseConn $ init $ tail s
  otherwise -> parsePred s

parseConn :: String -> Prop
parseConn s = case splitOnConn s of
  (s1,'∧',s2) -> Conj (parse s1) (parse s2)
  (s1,'→',s2) -> Impl (parse s1) (parse s2)

-- "P x Q" -> ("P",'x',"Q")
splitOnConn :: String -> (String,Char,String)
splitOnConn s = (p,c,q)
  where
    -- The 3 is the length of " x "
    (p,cq) = takePth s
    c  = head $ tail $ take 3 cq
    q  = drop 3 cq

-- "(P) x Q" -> ("(P)", " x Q")
takePth :: String -> (String,String)
takePth s = takePth' ("",s) (0,0)
  where
    takePth' :: (String,String) -> (Int,Int) -> (String,String)
    takePth' (sl,sr) (l,r) | l == r && (l+r) > 0 = (sl,sr)
                           | otherwise           = case head sr of
      '('       -> takePth' (sl ++ "("          , tail sr) (l+1 ,   r)
      ')'       -> takePth' (sl ++ ")"          , tail sr) (l   , r+1)
      '['       -> takePth' (sl ++ "["          , tail sr) (l+1 ,   r)
      ']'       -> takePth' (sl ++ "]"          , tail sr) (l   , r+1)
      otherwise -> takePth' (sl ++ (head sr:[]) , tail sr) (l   ,   r)

-- "P(x,y,z)" -> Pred "P" ["x","y","z"]
parsePred :: String -> Prop
parsePred s = Pred name xs
  where
    name = takeWhile (/= '(') s
    argList = drop (length name) s
    xs = split $ init $ tail argList

parseQuantifier :: String -> (Ind -> Prop)
parseQuantifier s = \x -> parse s''
  where
    s'  = tail s -- ∀x[...x...] -> x[...x...]
    xN  = takeWhile (/='[') s'
    s'' = init $ tail $ drop (length xN) s' -- x[...x...] -> ...x...
    replaceInd :: Prop -> Ind -> Ind -> Prop
    replaceInd (Pred name xs) old new = Pred name [if x==old then new else x
                                                      | x <- xs]
    replaceInd (Neg p)        old new = Neg $ replaceInd p old new
    replaceInd (Conj p1 p2)   old new = Conj (replaceInd p1 old new)
                                             (replaceInd p2 old new)
    replaceInd (Impl p1 p2)   old new = Impl (replaceInd p1 old new)
                                             (replaceInd p2 old new)
    replaceInd (Exists f)     old new = Exists $ \x -> replaceInd (f x) old new
    replaceInd (ForAll f)     old new = ForAll $ \x -> replaceInd (f x) old new

-- "x,y,z" -> ["x","y","z"]
split :: String -> [String]
split s = case dropWhile (== ',') s of
            "" -> []
            s' -> w : split s''
              where
                (w, s'') = break (== ',') s'

-- QuickCheck
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

instance Arbitrary Prop where
  arbitrary = sized arbProp
