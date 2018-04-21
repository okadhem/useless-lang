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

  it "trasforms (: f (-> * *)) to Var f (Arr Single Single)" $
   parse' "(: f (-> * *))" `shouldBe` Right  f

  it "transforms (: g *) to Var g Single" $
    parse' "(: g *)" `shouldBe` Right g

  it "tranforms (: ((: f (-> * *)) (: g *)) *) to LApp f g ..." $
    parse' "(: ( (: f (-> * *)) (: g *)) *)" `shouldBe` Right (LApp f g Single)

  it "transforms (: (lambda ((: x *)) (: x *)) (-> * *))" $
    parse' "(: (lambda (: x *) (: x *)) (-> * *))" `shouldBe` Right (
    LExp ("x", Single) (Var "x" Single) (Arr Single Single)) 
  it "transforms ((lambda x x) g) correctly"  $
    parse' "(: ((: (lambda (: x *) (: x *)) (-> * *)) (: g *)) *)" `shouldBe`
    Right (LApp (LExp ("x", Single) (Var "x" Single) (Arr Single Single)) g Single)
   
    where f = Var "f" (Arr Single Single)
          g = Var "g" Single
