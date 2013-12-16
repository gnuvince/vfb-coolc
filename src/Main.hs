module Main where

import qualified Data.ByteString.Lazy.Char8 as B

import Lexer
import Parser
import AST
import Pretty

main :: IO ()
main = do
  src <- B.getContents
  let tokens = alexScanTokens src
  let ast = parseCool tokens
  print ast
