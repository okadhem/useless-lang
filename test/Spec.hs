module Main where
import Test.Tasty
import Test.Tasty.Hspec
import ParserTests

main :: IO ()
main = do
  let
    tests = sequenceA [testSpec "Parser" parseSpec
                      ]
  tests_ <- tests
  defaultMain $ testGroup "all tests" tests_

