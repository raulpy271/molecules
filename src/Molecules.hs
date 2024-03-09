module Molecules where

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
lexer (c:s) =   Token { tokenType=Number 0, tokenStr="(" }  : lexer s
