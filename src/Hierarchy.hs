{-# LANGUAGE OverloadedStrings #-}

module Hierarchy ( validateUserClasses
                 , mkClassDigraph
                 , initialClassGraph
                 )
where

import qualified Data.Set as S
import Data.List (nub)

import qualified Digraph as D
import AST


builtInClasses :: S.Set Type
builtInClasses = S.fromList ["Object", "Bool", "Int", "String", "IO"]

finalClasses :: S.Set Type
finalClasses = S.fromList ["Bool", "Int", "String"]

{-|Make sure that all user-defined classes respect the following conditions:

  * No class is defined more than once;

  * None of the built-in classes (Object, Bool, Int, String, IO) are redefined;

  * None of the final built-in classes (Bool, Int, String) are redefined.
 -}
validateUserClasses :: [Class a] -> Bool
validateUserClasses cls =
    nub clsNames == clsNames &&
    all (`S.notMember` builtInClasses) clsNames &&
    all (`S.notMember` finalClasses) clsParents &&
    and [D.isAcyclic cn clsDigraph | cn <- clsNames]
    where clsNames = map className cls
          clsParents = map classInheritedClass cls
          clsDigraph = mkClassDigraph cls


mkClassDigraph :: [Class a] -> D.Digraph
mkClassDigraph cls =
    foldr (\c g -> D.addEdge (classInheritedClass c) (className c) g) initialClassGraph cls

initialClassGraph :: D.Digraph
initialClassGraph = D.empty
                  |> D.addEdge "Object" "Int"
                  |> D.addEdge "Object" "String"
                  |> D.addEdge "Object" "Bool"
                  |> D.addEdge "Object" "IO"

(|>) :: a -> (a -> b) -> b
x |> f = f x
