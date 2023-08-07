module Main where

import qualified Examples
import qualified LogicTest

main :: IO ()
main = do
  putStrLn ""
  LogicTest.test
  putStrLn ""
  Examples.test
