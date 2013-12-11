module Main where

import Lexer

main :: IO ()
main = do
  src <- getContents
  print (alexScanTokens src)
