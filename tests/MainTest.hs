module Main where

import Test.HUnit

import Hierarchy

testAcyclicGraph :: Test
testAcyclicGraph =
    TestCase $ assertBool "acyclic class graph" (validateUserClasses [])

tests = TestList [
         TestLabel "acyclic class graph" testAcyclicGraph
        ]

main :: IO ()
main = do
  runTestTT tests
  return ()
