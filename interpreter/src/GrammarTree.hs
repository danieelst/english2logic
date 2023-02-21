{-# LANGUAGE DeriveGeneric #-}

module GrammarTree(ParseRecord(..),GrammarTree(..),readJson,prettyPrint) where

import Control.Applicative((<$>))
import Data.Aeson(FromJSON,parseJSON,decode)
import GHC.Generics(Generic)
import Data.Tree(Tree(..),drawTree)
import Grammar(Category(..),fromString)
import qualified Data.ByteString.Lazy.UTF8 as BLU

----------------------------------------------------
-- Define the grammar tree
----------------------------------------------------

data ParseRecord = Parse {input :: String, output :: [GrammarTree]}
  deriving (Show,Generic)
instance FromJSON ParseRecord

-- A data type for expressing a node in a grammar tree
-- `CategoryNode cat cats trees` tells us that this node's category is cat
-- and the direct next trees have the categories cats
-- cats are helpful for deciphering intepretation patterns
-- Example: CategoryNode S [NP,VP] [(CategoryNode NP ...), (CategoryNode VP ...)]
data GrammarTree = LexemeNode   String
                 | CategoryNode Category [Category] [GrammarTree]
  deriving (Show)

readJson :: String -> Maybe ParseRecord
readJson = decode . BLU.fromString

prettyPrint :: GrammarTree -> IO ()
prettyPrint t = putStrLn $ drawTree $ fromGrammarTreeToTree t

----------------------------------------------------
-- Internals, used when parsing JSON
----------------------------------------------------
instance FromJSON GrammarTree where
  parseJSON json = fromRecord <$> parseJSON json

fromGrammarTreeToTree :: GrammarTree -> Tree String
fromGrammarTreeToTree (LexemeNode s)        = Node s []
fromGrammarTreeToTree (CategoryNode t _ ts) = Node (show t)
                                            $ map fromGrammarTreeToTree ts

-- This one is used for parsing JSON into the data type
data GrammarTreeRecord = Obj {node :: String, children :: [GrammarTreeRecord]}
  deriving (Show,Generic)
instance FromJSON GrammarTreeRecord

-- Converting between the record and the ADT
fromRecord :: GrammarTreeRecord -> GrammarTree
fromRecord Obj{node=w,children=[]} = LexemeNode w
fromRecord Obj{node=t,children=ts} = CategoryNode (fromString t)
                                                  (getCats ts)
                                                  (map fromRecord ts)
getCats :: [GrammarTreeRecord] -> [Category]
getCats (Obj{node=w,children=[]}:ts') = getCats ts'
getCats (Obj{node=t,children=ts}:ts') = fromString t : getCats ts'
getCats []                            = []
