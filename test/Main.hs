module Main (main) where


import Test.HUnit
import Molecules

test1 = TestCase (assertEqual "Should parse Open brackets" 
  [Token {tokenType=OBrackets, tokenStr="("}]
  (lexer "("))

tests = TestList [TestLabel "test1" test1]

main :: IO ()
--main = putStrLn "Test suite not yet implemented."
main = do
  _ <- runTestTT tests
  putStrLn "Tests finished"
