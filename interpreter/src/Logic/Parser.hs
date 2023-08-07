module Logic.Parser(parse) where

import Logic.Prop(Prop(..),Ind)
import Data.Char(isSpace)
import Data.List.Split(splitOn)

-- Parse a string of symbols into a proposition (if valid)
parse :: String -> Prop
parse s = let s' = removeSpaces s in case head s' of
  '¬' -> Neg    $ parse $ init $ tail $ tail s'
  '∃' -> Exists $ parseQuantifier s'
  '∀' -> ForAll $ parseQuantifier s'
  '(' -> parseConn $ init $ tail s'
  _   -> parsePred s'

-- Given a stringified proposition, remove all non-individual spacing
removeSpaces :: String -> String
removeSpaces s = s'''
  where
    s'   = markIndividualSpacing s                -- Replace spaces in individuals
    s''  = [c | c <- s', c/=' ']                  -- Remove spaces
    s''' = [if c=='_' then ' ' else c | c <- s''] -- Re-replace spaces

-- All spaces that are enclosed within quotation marks gets
-- replaced with underscores
markIndividualSpacing :: String -> String
markIndividualSpacing s = markIndividualSpacing' s False
  where
    markIndividualSpacing' :: String -> Bool -> String
    markIndividualSpacing' (c:s) quoted = case c of
      '\"' -> if quoted then  c  : markIndividualSpacing' s False
                        else  c  : markIndividualSpacing' s True
      ' '  -> if quoted then '_' : markIndividualSpacing' s quoted
                        else       markIndividualSpacing' s quoted
      _    -> c : markIndividualSpacing' s quoted
    markIndividualSpacing' []    _      = []

parseConn :: String -> Prop
parseConn s = case splitOnConn s of
  (s1,'∧',s2) -> Conj (parse s1) (parse s2)
  (s1,'→',s2) -> Impl (parse s1) (parse s2)

-- "PxQ" -> ("P",'x',"Q")
splitOnConn :: String -> (String,Char,String)
splitOnConn s = (p,c,q)
  where
    (p,cq) = takePth s
    c  = head cq
    q  = drop 1 cq

-- "(P) x Q" -> ("(P)", " x Q")
takePth :: String -> (String,String)
takePth s = takePth' ("",s) (0,0)
  where
    takePth' :: (String,String) -> (Int,Int) -> (String,String)
    takePth' (sl,sr) (l,r) | l == r && (l+r) > 0 = (sl,sr)
                           | otherwise           = case head sr of
      '(' -> takePth' (sl ++ "("       , tail sr) (l+1 ,   r)
      ')' -> takePth' (sl ++ ")"       , tail sr) (l   , r+1)
      '[' -> takePth' (sl ++ "["       , tail sr) (l+1 ,   r)
      ']' -> takePth' (sl ++ "]"       , tail sr) (l   , r+1)
      _   -> takePth' (sl ++ [head sr] , tail sr) (l   ,   r)

-- "P("x","y","z")" -> Pred "P" ["x","y","z"]
parsePred :: String -> Prop
parsePred s = Pred name xs
  where
    name = takeWhile (/= '(') s
    argList = drop (length name) s
    xs = map (\s -> if '\"' `elem` s then (removeExcessSpaces . init . tail) s
                                     else  removeExcessSpaces s)
             $ splitOn "," $ init $ tail argList

removeExcessSpaces :: String -> String
removeExcessSpaces = foldr1 (\a b -> a ++ " " ++ b) . words

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
