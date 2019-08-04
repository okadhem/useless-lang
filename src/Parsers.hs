{-# LANGUAGE FlexibleContexts #-}

module Parsers where

import DataTypes

import Text.Parsec
import Text.Parsec.Char
import Data.Functor.Identity
import Bound

separator :: Stream s Identity Char => Parsec s u Char
separator = space <|> newline <|> tab

          

sep :: Stream s Identity Char => Parsec s u ()
sep =  fmap (const ()) $ space <|> newline <|> tab   

oParen :: Stream s Identity Char => Parsec s u ()
oParen = do 
    char '('
    many sep
    return ()

cParen :: Stream s Identity Char => Parsec s u ()
cParen = do 
    many sep
    char ')'
    return ()

identifier :: Stream s Identity Char => Parsec s u String
identifier = many1 $ alphaNum 
    <|> char '_' <|> char '>' 
    <|> char '<' <|> char '*'
    <|> char '-' <|> char '/' 
    <|> char '\\'


variable :: Stream s Identity Char => Parsec s u Exp'
variable = Var <$> identifier


application :: Stream s Identity Char => Parsec s u Exp'
application = do
    oParen
    f <- expression
    many1 sep
    g <- expression
    cParen
    return $ App f g


lambda :: Stream s Identity Char => Parsec s u Exp'
lambda = do
    oParen
    string "lambda"
    many1 sep
    
    oParen
    char ':'
    many1 sep
    x <- identifier
    many1 sep
    ty <- typeExpression
    cParen 
    
    many sep
    body <- expression
    cParen
    return $ Lam ty x $ abstract1 x body 


zero :: Stream s Identity Char => Parsec s u Exp'
zero = const Zero <$> string "Zero"


succesor :: Stream s Identity Char => Parsec s u Exp'
succesor = do
    oParen
    string "Succ"
    many1 sep
    e <- expression
    cParen  
    return $ Succ e


-- parses the like of this (rec_nat m (p fp (Succ fp)) val)
rec_nat :: Stream s Identity Char => Parsec s u Exp'
rec_nat = do 
    oParen
    string "rec_nat"
    many1 sep
    zero_case <- expression
    many1 sep

    oParen
    p <- identifier
    many1 sep
    fp <- identifier
    many1 sep
    succ_case <- expression
    cParen

    many1 sep 
    val <- expression
    cParen
    return $ Rec_Nat zero_case p fp (abstract2 p fp succ_case) val
	where
	abstract2 x y e = let select x = Just True
			      select y = Just False
			      select _ = Nothing
			      in
			      abstract select e
    

typeSingle :: Stream s Identity Char => Parsec s u TExp
typeSingle = const Single <$> char '*'


typeArr::Stream s Identity Char => Parsec s u TExp
typeArr = do
    oParen
    string "->"
    many1 sep
    f <- typeExpression
    many1 sep
    g <- typeExpression
    cParen 
    return $ Arr f g


typeNat :: Stream s Identity Char => Parsec s u TExp
typeNat = const Nat <$> string "Nat"


typeExpression :: Stream s Identity Char => Parsec s u TExp
typeExpression = try typeNat <|> try typeSingle <|> try typeArr 


expression :: Stream s Identity Char => Parsec s u Exp'
expression = try lambda <|> try rec_nat <|> try succesor <|> try zero <|> try variable <|> try application 


parse :: String -> Either ParseError Exp'   
parse = Text.Parsec.parse expression ""


parse' :: SourceName -> String -> Either ParseError Exp'   
parse' = Text.Parsec.parse expression 


