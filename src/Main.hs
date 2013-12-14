module Main where

import qualified Data.ByteString.Lazy.Char8 as B

import Lexer

main :: IO ()
main = do
  src <- B.getContents
  print (alexScanTokens src)
