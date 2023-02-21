module Main where

import System.Environment(getArgs)
import Data.List(isSuffixOf)
import System.FilePath(isValid)

import GrammarTree(ParseRecord(..),readJson,prettyPrint)

import Semantics(interpret)

main :: IO ()
main = do
  args <- getArgs
  let fp = head args
  if checkFilePath fp
    then do
      text <- readFile fp
      case readJson text of
        Just record -> program record
        Nothing     -> putStrLn "Invalid JSON"
    else
      putStrLn "Invalid file path"

checkFilePath :: String -> Bool
checkFilePath fp = isValid fp && ".json" `isSuffixOf` fp

program :: ParseRecord -> IO ()
program record = do
  putStrLn  $ input record
  putStrLn ""
  let trees = output record
  sequence_ $ map prettyPrint trees
  sequence_ $ map (putStrLn . show . interpret) trees
