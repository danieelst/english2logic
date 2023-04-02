module GrammarTree(GrammarTree(..),prettyPrint) where

import Data.Tree(Tree(..),drawTree)
import Grammar(Category)

-- A data type for expressing a node in a grammar tree
-- `CategoryNode cat cats trees` tells us that this node's category is cat
-- and the direct next trees have the categories cats
-- cats are helpful for deciphering intepretation patterns
-- Example: CategoryNode S [NP,VP] [(CategoryNode NP ...), (CategoryNode VP ...)]
data GrammarTree = LexemeNode   String
                 | CategoryNode Category [Category] [GrammarTree]
  deriving (Show)

prettyPrint :: GrammarTree -> IO ()
prettyPrint t = putStrLn $ drawTree $ fromGrammarTreeToTree t

fromGrammarTreeToTree :: GrammarTree -> Tree String
fromGrammarTreeToTree (LexemeNode s)        = Node s []
fromGrammarTreeToTree (CategoryNode t _ ts) = Node (show t)
                                            $ map fromGrammarTreeToTree ts
