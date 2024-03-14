module Molecules where

import Data.Char


data TokenType = Molecule | Number Int | OBrackets | CBrackets | OSquareBrackets | CSquareBrackets | OCurlyBrackets | CCurlyBrackets 
  deriving (Eq, Ord, Read, Show)
  
data Token = Token { 
  tokenType :: TokenType,
  tokenStr :: String
} deriving (Eq, Ord, Read, Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('(':s) = Token { tokenType=OBrackets, tokenStr="(" } : lexer s
lexer (')':s) = Token { tokenType=CBrackets, tokenStr=")" } : lexer s
lexer ('[':s) = Token { tokenType=OSquareBrackets, tokenStr="[" } : lexer s
lexer (']':s) = Token { tokenType=CSquareBrackets, tokenStr="]" } : lexer s
lexer ('{':s) = Token { tokenType=OCurlyBrackets, tokenStr="{" } : lexer s
lexer ('}':s) = Token { tokenType=CCurlyBrackets, tokenStr="}" } : lexer s

lexer str =
  let (c:s) = str in
    if isAlphaNum c then
      if isDigit c then
        let (digits, rest) = splitNumber str in
          Token { tokenType=Number (read digits), tokenStr=digits } : lexer rest
      else 
        if isUpper c then
          let (lower, rest) = splitLower s in
            Token { tokenType=Molecule, tokenStr=c:lower } : lexer rest
        else lexer s
    else lexer s

splitNumber = splitWhile isDigit

splitLower = splitWhile (\c -> isAlpha c && isLower c)

splitWhile :: (Char -> Bool) -> String -> (String, String)
splitWhile _ [] = ("", "")
splitWhile f (c:s) = if f c then
  let (n, r) = splitWhile f s in
    (c:n, r)
  else ("", c:s)

