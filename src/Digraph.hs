module Digraph ( Digraph
               , empty
               , addEdge
               , neighbors
               , isAcyclic
               )
where

import qualified Data.Map as M
import qualified Data.Set as S

newtype Digraph = MkDigraph (M.Map String (S.Set String))
    deriving (Show)

empty :: Digraph
empty = MkDigraph (M.empty)

addEdge :: String -> String -> Digraph -> Digraph
addEdge start end (MkDigraph m) =
    MkDigraph $ M.alter (\set -> case set of
                       Nothing -> Just $ S.singleton end
                       Just s  -> Just $ S.insert end s) start m

neighbors :: String -> Digraph -> Maybe (S.Set String)
neighbors node (MkDigraph m) = M.lookup node m


isAcyclic :: String -> Digraph -> Bool
isAcyclic startNode g@(MkDigraph m) = dfs (S.singleton startNode) S.empty
    where dfs toBeVisited visited
              | S.null toBeVisited = True
              | otherwise =
                  let (curr, toBeVisited') = S.deleteFindMin toBeVisited in
                  if curr `S.member` visited then
                      False
                  else
                      case neighbors curr g of
                        Nothing -> dfs toBeVisited' (S.insert curr visited)
                        Just ns -> dfs (ns `S.union` toBeVisited') (S.insert curr visited)
