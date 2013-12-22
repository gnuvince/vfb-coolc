{-# LANGUAGE OverloadedStrings #-}


module Main where

import Test.HUnit

import AST
import Hierarchy

testDefaultGraph =
    TestCase $ assertBool
                 "default graph"
                 (validateUserClasses [])

testNoInherit =
    TestCase $ assertBool
                 "no inherit"
                 (validateUserClasses [Class "X" Nothing [] ()])

testInheritObject =
    TestCase $ assertBool
                 "inherit Object"
                 (validateUserClasses [Class "X" (Just "Object") [] ()])

testRejectCycles =
    TestCase $ assertBool
                 "class cycle rejected"
                 (not (validateUserClasses [Class "X" (Just "Y") [] (),
                                            Class "Y" (Just "X") [] ()]))

testNoRedef =
    TestCase $ assertBool
                 "final classes (Int, Bool, String) cannot be redefined"
                 (and [not (validateUserClasses [Class c Nothing [] ()])
                       | c <- ["Int", "Bool", "String", "IO", "Object"]])

testCannotInherit =
    TestCase $ assertBool
                 "final classes (Int, Bool, String) cannot be redefined"
                 (and [not (validateUserClasses [Class "X" (Just c) [] ()])
                       | c <- ["Int", "Bool", "String"]])


tests = TestList [
          testDefaultGraph
        , testNoInherit
        , testInheritObject
        , testRejectCycles
        , testNoRedef
        , testCannotInherit
        ]

main :: IO ()
main = do
  c <- runTestTT tests
  return ()
