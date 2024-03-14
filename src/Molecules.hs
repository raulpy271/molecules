module Molecules where

import Data.Char


data TokenType = Molecule | Number Int | OBrackets | CBrackets | OSquareBrackets | CSquareBrackets | OCurlyBrackets | CCurlyBrackets 
  deriving (Eq, Ord, Read, Show)
  
data Token = Token { 
  tokenType :: TokenType,
  tokenStr :: String
} deriving (Eq, Ord, Read, Show)

lexer :: String -> Either String [Token]
lexer [] = Right []
lexer ('(':s) = case lexer s of
  Right tokens -> Right (Token { tokenType=OBrackets, tokenStr="(" } : tokens)
  Left err -> Left err
lexer (')':s) = case lexer s of
  Right tokens -> Right (Token { tokenType=CBrackets, tokenStr=")" } : tokens)
  Left err -> Left err
lexer ('[':s) = case lexer s of
  Right tokens -> Right (Token { tokenType=OSquareBrackets, tokenStr="[" } : tokens)
  Left err -> Left err
lexer (']':s) = case lexer s of
  Right tokens -> Right (Token { tokenType=CSquareBrackets, tokenStr="]" } : tokens)
  Left err -> Left err
lexer ('{':s) = case lexer s of
  Right tokens -> Right (Token { tokenType=OCurlyBrackets, tokenStr="{" } : tokens)
  Left err -> Left err
lexer ('}':s) = case lexer s of
  Right tokens -> Right (Token { tokenType=CCurlyBrackets, tokenStr="}" } : tokens)
  Left err -> Left err

lexer str =
  let (c:s) = str in
    if isAlphaNum c then
      if isDigit c then
        let (digits, rest) = splitNumber str in
          case lexer rest of
            Right tokens -> Right (Token { tokenType=Number (read digits), tokenStr=digits } : tokens)
            Left err -> Left err
      else 
        if isUpper c then
          let (lower, rest) = splitLower s in
            case lexer rest of
              Right tokens -> Right (Token { tokenType=Molecule, tokenStr=c:lower } : tokens)
              Left err -> Left err
        else Left ("The character '" ++ [c] ++ "' should be in upper case or a number")
    else Left ("The character '" ++ [c] ++ "' is not valid")

splitNumber = splitWhile isDigit

splitLower = splitWhile (\c -> isAlpha c && isLower c)

splitWhile :: (Char -> Bool) -> String -> (String, String)
splitWhile _ [] = ("", "")
splitWhile f (c:s) = if f c then
  let (n, r) = splitWhile f s in
    (c:n, r)
  else ("", c:s)

