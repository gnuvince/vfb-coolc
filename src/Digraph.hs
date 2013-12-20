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

-- |Create an empty graph.
empty :: Digraph
empty = MkDigraph (M.empty)

-- |Add an edge from A to B into a Digraph.
addEdge :: String -> String -> Digraph -> Digraph
addEdge start end (MkDigraph m) =
    MkDigraph $ M.alter (\set -> case set of
                                   Nothing -> Just $ S.singleton end
                                   Just s  -> Just $ S.insert end s) start m

-- |Return the nodes pointed to by a node; if the
-- node is not in the digraph (or has no outgoing edge),
-- return the empty set.
neighbors :: String -> Digraph -> S.Set String
neighbors node (MkDigraph m) =
    case M.lookup node m of
      Nothing -> S.empty
      Just s  -> s


-- |Verify that a starting at a given node, the graph has no cycles.
isAcyclic :: String -> Digraph -> Bool
isAcyclic startNode g@(MkDigraph m) = dfs (S.singleton startNode) S.empty
    where dfs toBeVisited visited
              | S.null toBeVisited = True
              | otherwise =
                  let (curr, toBeVisited') = S.deleteFindMin toBeVisited in
                  if curr `S.member` visited then
                      False
                  else
                      dfs (neighbors curr g `S.union` toBeVisited') (S.insert curr visited)
