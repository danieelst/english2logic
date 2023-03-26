module Examples(test) where

import System.Exit(exitFailure,exitSuccess)
import System.Directory(getDirectoryContents)
import System.FilePath(joinPath)

import Data.Maybe(isJust,fromJust)

import Test.HUnit(Test(TestCase,TestList),Counts(..),assertBool,runTestTT,showCounts)

import GrammarTree(readJson,output)
import Interpretation(interpret)

jsonExt :: String
jsonExt = ".json"

pathToExamples :: FilePath
pathToExamples = "../examples/"

test :: IO ()
test = do
  putStrLn "Testing examples"
  files <- getDirectoryContents pathToExamples
  let jsonFiles = map (\f -> joinPath [pathToExamples,f]) (filter isJson files)
  cs@(Counts _ _ errs fails) <- runTestTT $ TestList $ map makeTest jsonFiles
  if (errs > 0 || fails > 0)
    then exitFailure
    else exitSuccess

isJson :: FilePath -> Bool
isJson fp = (drop (length fp - length jsonExt) fp) == jsonExt

makeTest :: FilePath -> Test
makeTest fp = TestCase $ do
  putStrLn $ "\n\nTesting " ++ fp
  text <- readFile fp
  let parseRecord = readJson text
  assertBool "Invalid file" (isJust parseRecord)
  let ps = map interpret $ output $ fromJust parseRecord
  -- The print is mostly to force the otherwise lazy interpretation to complete
  sequence_ $ map (putStrLn . (\s -> "  >> " ++ s) . show) ps
  putStrLn ""
