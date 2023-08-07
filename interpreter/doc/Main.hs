import Grammar(Category(..),getAllCategories)

import qualified Semantics.DT  as DT
import qualified Semantics.NN  as NN
import qualified Semantics.NNP as NNP
import qualified Semantics.NP  as NP
import qualified Semantics.S   as S
import qualified Semantics.VBZ as VBZ
import qualified Semantics.VP  as VP

import Determiners(table)

pathToFile :: FilePath
pathToFile = "GRAMMAR.md"

header :: String
header = "# Supported grammar\n\n"

catHeader :: String
catHeader = "## Categories\n\n"

rulesHeader :: String
rulesHeader = "## Interpretation rules\n\n\
              \| Category | Construction |\n\
              \|----------|--------------|\n"

detHeader :: String
detHeader = "## Determiner lexicon\n\n\
            \| Determiner | Quantifier |\n\
            \|------------|------------|\n"

ending :: String
ending = "\n*This is a generated file.*\n"

ruleToText :: Category -> [Category] -> String
ruleToText _   []   = ""
ruleToText cat cats = "| "
                    ++ show cat
                    ++ " | "
                    ++ (tail $ concatMap (\c -> " " ++ show c) cats)
                    ++ " |"

rulesToText :: Category -> [[Category]] -> String
rulesToText _   []     = ""
rulesToText cat (cs:css) = ruleToText cat cs ++ "\n" ++ rulesToText cat css

main :: IO ()
main = do
  let cats = concatMap (\c -> " * " ++ show c ++ "\n") getAllCategories ++ "\n"
  let patterns = rulesToText S   S.patterns
              ++ rulesToText NP  NP.patterns
              ++ rulesToText NNP NNP.patterns
              ++ rulesToText NN  NN.patterns
              ++ rulesToText VP  VP.patterns
              ++ rulesToText VBZ VBZ.patterns
              ++ rulesToText DT  DT.patterns
              ++ "\n"
  let dets = concatMap (\(d,q) -> "|" ++ d ++ "|" ++ show q ++ "|\n") table
  putStrLn $ "Generating " ++ pathToFile
  writeFile pathToFile $ header ++ catHeader ++ cats
                                ++ rulesHeader ++ patterns
                                ++ detHeader ++ dets
                                ++ ending
