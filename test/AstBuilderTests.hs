module AstBuilderTests where

import Test.Tasty.Hspec
import Text.Parsec
import DataTypes
import Parsers

astBuilderType_spec :: Spec
astBuilderType_spec = let parse' = fmap astBuilderTypes . parse sexp "test" in
  do
  it "transforms * to Type Single" $
     parse' "*" `shouldBe` Right Single
  it "transforms (-> * *) to Type Arr Single Single" $
     parse' "(-> * *)" `shouldBe` Right (Arr Single Single)
  it "transfroms (-> (-> * *) *) to Type Arr (Arr Single Single) Single" $
     parse' "(-> (-> * *) *)" `shouldBe` Right ( Arr (Arr Single Single) Single) 


astBuilderExp_spec :: Spec
astBuilderExp_spec = let
  parse' = fmap astBuilderExp . parse sexp "test" in do
   describe "parsing and building Ast" $ do
     it "" $
       parse' "f" `shouldBe` Right  f 

     it "" $
       parse' "g" `shouldBe` Right g

     it "" $
       parse' "(f g)" `shouldBe` Right (LApp f g)

     it "" $
       parse' "(lambda (: x *) x)" `shouldBe` Right (
         Lam ("x", Single) (Var "x") ) 

     it "" $
       parse' "((lambda (: x *) x) g)" `shouldBe`
         Right (LApp (Lam ("x", Single) (Var "x") ) g)
   
    where f = Var "f" 
          g = Var "g"
