module Main where

import REPL
import System.Environment
main :: IO ()
main = eval_file

eval_file :: IO ()
eval_file = do
  filepath <- fmap head getArgs
  content <- readFile filepath
  putStrLn $ eval content
