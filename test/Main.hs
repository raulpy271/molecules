module Main (main) where


import Test.Hspec
import Control.Exception (evaluate)
import Molecules

main :: IO ()
main = hspec $ do
  describe "brackets" $ do
    it "brackets" $ do
      lexer "()" `shouldBe` [Token {tokenType=OBrackets, tokenStr="("}, Token {tokenType=CBrackets, tokenStr=")"}]
    it "square brackets" $ do
      lexer "[]" `shouldBe` [Token {tokenType=OSquareBrackets, tokenStr="["}, Token {tokenType=CSquareBrackets, tokenStr="]"}]

  describe "numbers" $ do
    it "One digit" $ do
      lexer "1" `shouldBe` [Token {tokenType=Number 1, tokenStr="1"}]
    it "Multiple digits" $ do
      lexer "12" `shouldBe` [Token {tokenType=Number 12, tokenStr="12"}]

  describe "molecule name" $ do
    it "Single letter" $ do
      lexer "H" `shouldBe` [Token {tokenType=Molecule, tokenStr="H"}]
    it "Multiple letters" $ do
      lexer "He" `shouldBe` [Token {tokenType=Molecule, tokenStr="He"}]
    it "wrong name" $ do
      lexer "h" `shouldBe` []

  describe "Multiple molecules" $ do
    it "Single letter" $ do
      lexer "HF" `shouldBe` [Token {tokenType=Molecule, tokenStr="H"}, Token {tokenType=Molecule, tokenStr="F"}]
    it "Two letters" $ do
      lexer "HeFFa" `shouldBe` [
        Token {tokenType=Molecule, tokenStr="He"},
        Token {tokenType=Molecule, tokenStr="F"},
        Token {tokenType=Molecule, tokenStr="Fa"}]

  describe "splitNumber" $ do
    it "One digit" $ do
      splitNumber "1" `shouldBe` ("1", "")
    it "Two digit" $ do
      splitNumber "12" `shouldBe` ("12", "")
    it "Two digit with remaining" $ do
      splitNumber "12s2" `shouldBe` ("12", "s2")
    it "No digits" $ do
      splitNumber "s12" `shouldBe` ("", "s12")

