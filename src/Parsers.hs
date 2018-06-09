{-# LANGUAGE FlexibleContexts #-}

module Parsers where

import DataTypes

import Text.Parsec
import Text.Parsec.Char
import Data.Functor.Identity

separator :: Stream s Identity Char => Parsec s u Char
separator = space <|> newline <|> tab

atom :: Stream s Identity Char => Parsec s u Atom
atom = f <$>  (many1 (alphaNum <|> char '_') <|> string ":" <|> string "*" <|> string "->")
  where f x = case x of
          "lambda" ->  (Keyword Lambda)
          ":" -> (Keyword Column)
          str -> (Id str)
          
sexp :: Stream s Identity Char => Parsec s u Sexp
sexp = (fmap Cell atom) <|> do
  char '('
  many separator
  x <- fmap List $  sexp `sepBy` many1 separator
  char ')'
  pure x 

astBuilderTypes :: Sexp -> TExp
astBuilderTypes s = case s of
  List ((Cell (Id "->")) : a : b : []) ->  Arr (astBuilderTypes a) (astBuilderTypes b)
  Cell (Id "*") -> Single 
  Cell (Id "Nat") -> Nat 
  
astBuilderExp :: Sexp -> Exp 
astBuilderExp s = case s of

    Cell (Id "Zero") -> Zero
    Cell (Id sym) -> Var sym  

    List (Cell ( Id "Succ") : e : []) -> Succ $ astBuilderExp e

    List (Cell (Id "rec_nat") : e0 : List (Cell (Id x) : Cell (Id y)  : e : []) : f : []) ->
      Rec_Nat (astBuilderExp e0) x y (astBuilderExp e) (astBuilderExp f)

    List (f : g :[]) -> LApp  (astBuilderExp f) (astBuilderExp g) 

    List (Cell( Keyword Lambda) : List (Cell (Keyword Column) : Cell (Id x) : t : [] ) : body : [])
        -> Lam (x,astBuilderTypes t) (astBuilderExp body) 
    
