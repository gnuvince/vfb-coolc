module Main where

import Lexer
import Parser
import AST
import Pretty

getAST src = case scan src of
                  Left err -> error err
                  Right toks -> parseCool toks

main :: IO ()
main = do
  src <- getContents
  -- let ast = getAST src
  -- print (pretty ast)
  print (getTokens src)
  print (pretty $ getAST src)
