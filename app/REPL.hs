module REPL where
import TypeChecker
import Parsers
import Text.Parsec


eval :: String -> String
eval e = let
  expr =  fmap astBuilderExp . parse sexp "test" $ e 
  tyder = fmap ( typeCheck [] ) expr
  in
  pprint' tyder 


repl :: IO()
repl = do
  putStr "> "
  input <- getLine
  case input of
    ":q" -> pure ()
    _ -> do
      putStrLn $ eval input
      repl
    

  

