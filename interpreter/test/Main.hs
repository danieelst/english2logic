module Main where

import qualified Examples as Examples
import qualified LogicTest as LogicTest

main :: IO ()
main = do
  putStrLn ""
  LogicTest.test
  putStrLn ""
  Examples.test
