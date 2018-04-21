module ParserTests where

import Test.Tasty.Hspec
import Text.Parsec
import DataTypes
import Parsers

parser_spec :: Spec
parser_spec = do
  it "allows to have arbitrary nested list" $
    parse sexp "source" "((a b) (((c)) (d e)) x)" `shouldBe` Right  
    (List [List [cellId "a",cellId "b"],
          List [List [ List [cellId "c"] ],
               List [cellId "d",cellId "e"] ],
          cellId "x"
          
         ])
  parser_atom_spec
  it "allows for spaces , tabs or newLines at the start of sexp" $
    parse sexp "source" "(   \t \n a)" `shouldBe` Right (List [cellId "a"])

  it "allows for spaces , tabs or newLines at the end of sexp" $
    parse sexp "source" "(a    \t \n)" `shouldBe` Right (List [cellId "a"])


  
parser_atom_spec :: Spec
parser_atom_spec = do
    it "parses lambda to Keyword lambda" $
     parse atom "test" "lambda" `shouldBe` Right (Keyword Lambda)

    it "parses text with lambda in it as Id" $
      parse atom "test" "ddlambdaxx" `shouldBe` Right (Id "ddlambdaxx" ) 
    it "parses text with lambda at the front of it as Id" $
      parse atom "test" "lambdass" `shouldBe` Right (Id "lambdass" )

    it "parses text with lambda at the end of it as Id" $
      parse atom "test" "eelambda" `shouldBe` Right (Id "eelambda" )

    it "allows Identifier to have any alphaNum character " $
      parse atom "test" "1ed45z8" `shouldBe` Right (Id "1ed45z8" )

    it "allows for - > < _ # & $ characters to appear in Id" $
      parse atom "test" "-><_#1$SomeText" `shouldBe` Right (Id "-><_#1$SomeText" )
    it "parses : as Keyword Column" $
      parse sexp "test" ":" `shouldBe` Right (cellKeyword Column)
    
    it "parses : in the start of a word as an ID" $
      parse sexp "test" ":ddz" `shouldBe` Right (cellId ":ddz")
  
    it "parses : in the end of a word as an ID" $
      parse sexp "test" "dz:" `shouldBe` Right (cellId "dz:")

    it "parses : in the middle of a word as an ID" $
      parse sexp "test" "dz:ss" `shouldBe` Right (cellId "dz:ss")
