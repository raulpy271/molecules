module Main (main) where

import Data.Either
import Test.Hspec
import Control.Exception (evaluate)
import Molecules

main :: IO ()
main = hspec $ do
  describe "brackets" $ do
    it "brackets" $ do
      lexer "()" `shouldBe` Right [Token {tokenType=OBrackets, tokenStr="("}, Token {tokenType=CBrackets, tokenStr=")"}]
    it "square brackets" $ do
      lexer "[]" `shouldBe` Right [Token {tokenType=OSquareBrackets, tokenStr="["}, Token {tokenType=CSquareBrackets, tokenStr="]"}]

  describe "numbers" $ do
    it "One digit" $ do
      lexer "1" `shouldBe` Right [Token {tokenType=Number 1, tokenStr="1"}]
    it "Multiple digits" $ do
      lexer "12" `shouldBe` Right [Token {tokenType=Number 12, tokenStr="12"}]

  describe "molecule name" $ do
    it "Single letter" $ do
      lexer "H" `shouldBe` Right [Token {tokenType=Atom, tokenStr="H"}]
    it "Multiple letters" $ do
      lexer "He" `shouldBe` Right [Token {tokenType=Atom, tokenStr="He"}]

  describe "Multiple molecules" $ do
    it "Single letter" $ do
      lexer "HF" `shouldBe` Right [Token {tokenType=Atom, tokenStr="H"}, Token {tokenType=Atom, tokenStr="F"}]
    it "Two letters" $ do
      lexer "HeFFa" `shouldBe` Right [
        Token {tokenType=Atom, tokenStr="He"},
        Token {tokenType=Atom, tokenStr="F"},
        Token {tokenType=Atom, tokenStr="Fa"}]

  describe "Handle erros" $ do
    it "Is not a molecule" $ do
      isLeft (lexer "h") `shouldBe` True
    it "Is not a valid char" $ do
      isLeft (lexer "+") `shouldBe` True

  describe "splitNumber" $ do
    it "One digit" $ do
      splitNumber "1" `shouldBe` ("1", "")
    it "Two digit" $ do
      splitNumber "12" `shouldBe` ("12", "")
    it "Two digit with remaining" $ do
      splitNumber "12s2" `shouldBe` ("12", "s2")
    it "No digits" $ do
      splitNumber "s12" `shouldBe` ("", "s12")

