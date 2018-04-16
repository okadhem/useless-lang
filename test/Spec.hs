module Main where
import Test.Tasty
import Test.Tasty.Hspec
import DataTypes
import Parsers
import Text.Parsec
import Test.QuickCheck

main :: IO ()
main = do
  spec <- testSpec "atom_spec" atom_spec
  defaultMain spec

atom_spec :: Spec
atom_spec = do
    it "parses lambda to Keyword lambda" $
     parse atom "test" "lambda" `shouldBe` Right (Keyword Lambda)

    it "parses text with lambda in it as Id" $
      parse atom "test" "ddlambdaxx" `shouldBe` Right (Id "ddlambdaxx" ) 
    it "parses text with lambda at the front of it as Id" $
      parse atom "test" "lambdass" `shouldBe` Right (Id "lambdass" )

    it "parses text with lambda at the end of it as Id" $
      parse atom "test" "eelambda" `shouldBe` Right (Id "eelambda" )
