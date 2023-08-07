module Grammar(Category(..),fromString,getAllCategories) where

-------------------------------------------------------------------------------
-- Define the grammatical categories
-------------------------------------------------------------------------------

-- Data type for grammatical constituents
-- Based on Penn treebanks, see e.g. https://cs.nyu.edu/~grishman/jet/guide/PennPOS.html
data Category = ROOT  -- Root of the text
              | S     -- Sentences
              | NP    -- Noun phrases
              | NNP   -- Proper nouns
              | NN    -- Common nouns
              | VP    -- Verb phrases
              | VBZ   -- Verbalizers, e.g. `is`
              | DT    -- Determiner
  deriving (Show,Eq,Ord,Enum,Bounded)

-------------------------------------------------------------------------------
-- Some functions
-------------------------------------------------------------------------------

-- Function to derive all defined categories
getAllCategories :: [Category]
getAllCategories = enumFrom minBound

-- Translate from string to a label
fromString :: String -> Category
fromString s | length l == 1 = head l
             | otherwise     = error $ "No defined category " ++ s
  where
    l = [cat | cat <- getAllCategories, s == show cat]
