module ParserTests where

import Test.Tasty.Hspec
import DataTypes
import Parsers

parseSpec :: Spec 
parseSpec = do
    it "interpret newlines , tabs, and spaces as separtors" $
        parse "(f \n \t   g)" `shouldBe` Right (App f g)

    it "allows for separtors at the end of sexp" $ 
        parse "(f g \n \n )" `shouldBe` Right (App f g)

    it "allows for separtors at the start of sexp" $ 
        parse "(   f g)" `shouldBe` Right (App f g)

    it "parses text with lambda at the front of it as Id" $ 
        parse "(lambdazzz f)" `shouldBe` Right (App (Var "lambdazzz") f) 

            where
                f = Var "f"
                g = Var "g"
