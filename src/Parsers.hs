{-# LANGUAGE FlexibleContexts #-}

module Parsers where

import DataTypes

import Text.Parsec
import Text.Parsec.Char
import Data.Functor.Identity
import Bound

type Location = (SourcePos,SourcePos) 

type Exp' = Exp Location Name -- work in progress Exp datatype

located :: Stream s Identity Char => Parsec s u a -> Parsec s u (Location,a) 
located p = do 
    s <- getPosition
    a <- p
    e <- getPosition
    return $ let 
            -- it seams like getPosition is the next character the parser will handle
            -- so, we need to subtract one to get the position of the last handled character
            e' = setSourceColumn e correctedColumn
            correctedColumn = sourceColumn e - 1 
            in ((s,e'),a)
            


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

identifier :: Stream s Identity Char => Parsec s u Name
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
    f <- located expression
    many1 sep
    g <- located expression
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
    body <- located expression
    cParen
    return $ Lam ty x $ abstract1 x <$> body

zero :: Stream s Identity Char => Parsec s u Exp'
zero = const Zero <$> string "Zero"


succesor :: Stream s Identity Char => Parsec s u Exp'
succesor = do
    oParen
    string "Succ"
    many1 sep
    e <- located expression
    cParen  
    return $ Succ e


-- parses the like of this (rec_nat m (p fp (Succ fp)) val)
rec_nat :: Stream s Identity Char => Parsec s u Exp'
rec_nat = do 
    oParen
    string "rec_nat"
    many1 sep
    zero_case <- located expression
    many1 sep

    oParen
    p <- identifier
    many1 sep
    fp <- identifier
    many1 sep
    succ_case <- located expression
    cParen

    many1 sep 
    val <- located expression
    cParen
    return $ Rec_Nat zero_case p fp (abstract2 p fp <$> succ_case) val
        where
        abstract2 x y e = let select t | t == x = Just True
                              select t | t == y = Just False
                              select _ = Nothing
                          in abstract select e
    

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


