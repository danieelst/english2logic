module Examples(test) where

import Prelude hiding (read)

import System.Exit(exitFailure,exitSuccess)
import System.Directory(getDirectoryContents)
import System.FilePath(joinPath)

import Data.Maybe(isJust,fromJust)

import Test.HUnit(Test(TestCase,TestList),Counts(..)
                 ,assertBool,runTestTT,showCounts)

import IO(OutputRecord(..),read)
import Logic.Parser(parse)
import Interpretation(interpret)

jsonExt :: String
jsonExt = ".json"

pathToExamples :: FilePath
pathToExamples = "../examples/interpreter/"

test :: IO ()
test = do
  putStrLn "Testing examples"
  files <- getDirectoryContents pathToExamples
  let jsonFiles = map (\f -> joinPath [pathToExamples,f]) (filter isJson files)
  cs@(Counts _ _ errs fails) <- runTestTT $ TestList $ map makeTest jsonFiles
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess

isJson :: FilePath -> Bool
isJson fp = drop (length fp - length jsonExt) fp == jsonExt

makeTest :: FilePath -> Test
makeTest fp = TestCase $ do
  putStrLn $ "\n\nTesting " ++ fp
  outputRecord <- read fp
  let ps  = map parse $ logic outputRecord
  let ps' = map interpret $ grammar outputRecord
  mapM_ (putStrLn . ("  >> " ++) . show) ps
  mapM_ (putStrLn . ("  == " ++) . show) ps'
  assertBool "Incorrect interpretation(s)" (ps == ps')
  putStrLn ""
