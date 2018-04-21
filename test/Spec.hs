module Main where
import Test.Tasty
import Test.Tasty.Hspec
import ParserTests
import AstBuilderTests

main :: IO ()
main = do
  let
    tests = sequenceA [testSpec "Sexp Parser" parser_spec,
                       testSpec "AstBuilderTypes" astBuilderType_spec,
                       testSpec "AstBuilderExp" astBuilderExp_spec
                      ]
  tests_ <- tests
  defaultMain $ testGroup "all tests" tests_

