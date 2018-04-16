{-# LANGUAGE FlexibleContexts #-}

module Parsers where

import DataTypes

import Text.Parsec
import Text.Parsec.Char
import Data.Functor.Identity

separator :: Stream s Identity Char => Parsec s u Char
separator = space <|> newline <|> tab

atom :: Stream s Identity Char => Parsec s u Atom
atom = f <$>  (many1 alphaNum <|> string ":" <|> string "*" <|> string "->")
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

  
astBuilderExp :: Sexp -> Exp 
astBuilderExp s = case s of
    List (Cell (Keyword Column) : Cell (Id sym) : type_ : [] )
        -> Var sym $ astBuilderTypes type_ 

    List (Cell (Keyword Column) : List (f : g :[]) : type_ : [] )
        -> LApp  (astBuilderExp f) (astBuilderExp g ) (astBuilderTypes type_)

    List (Cell (Keyword Column) :  List (Cell( Keyword Lambda) : List (Cell (Keyword Column) : Cell (Id x) : t : [] ) : body : []): type_ : [] )
        -> LExp (astBuilderExp body) (x,astBuilderTypes t) (astBuilderTypes type_)

  -- callpat =  List (f : g :[]) 
  -- lambdaPattern = List ( Keyword Lambda : List (Cell (Keyword Column) : Cell (Id x) : t : [] ) : body : [])
