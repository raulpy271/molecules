# A parser of chemical formulas

A haskell library to solve the challange [molecules to atoms](https://www.codewars.com/kata/52f831fa9d332c6591000511/train/haskell) on CodeWars.

# Use library

To explore the library running commands on the GHCi interpreter, you should type:

```sh
$ ghci src/Molecules.hs 
ghci> lexer "H2O" -- Parser the water formula
Right [Token {tokenType = Atom, tokenStr = "H"},Token {tokenType = Number 2, tokenStr = "2"},Token {tokenType = Atom, tokenStr = "O"}]
```

# Run tests



There are a suit of unit tests, to run them type:

```sh

cabal test

```

This command assumes that you have the haskell ecosystem installed on your machine.
