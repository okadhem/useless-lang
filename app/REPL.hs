module REPL where
import TypeChecker
import Parsers


eval :: String -> String
eval e = let
  expr =  parse e 
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
    

  

