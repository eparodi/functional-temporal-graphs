module SimpleGraph.DFS where

import Definitions
import Tree

import SimpleGraph.Graph

import Data.Array
import Data.Array.ST
import Control.Monad.ST

type Set s = STUArray s Index Bool

-- | Returns an empty state.
mkEmpty :: Bounds -> ST s (Set s)
mkEmpty bnds = newArray bnds False

-- | Returns whether or not the Vertex was visited.
contains :: Set s -> Index -> ST s Bool
contains m v = readArray m v

-- | Marks the vertex as visited.
include :: Set s -> Index -> ST s ()
include m v = writeArray m v True

-- | This function reads the state of a vertex. If it was visited, it
-- continous expanding other node, else it will expand that node. This
-- function avoids the continous expansion of nodes.
chop :: Set s -> Forest Index -> ST s (Forest Index)
chop m [] = return []
chop m (Node v ts : us) = contains m v >>= \visited ->
    if visited then
        chop m us
    else
        include m v >>= \_  ->
        chop m ts   >>= \as ->
        chop m us   >>= \bs ->
        return ((Node v as) : bs)

-- | Takes an infinite Forest and prunes the repeated parts. That means that
-- the tree will not have repeated vertex.
prune :: Bounds -> Forest Index -> Forest Index
prune bnds ts =
    runST (mkEmpty bnds >>= \m -> chop m ts)

-- | From a Graph and a Vertex generates a Tree whose root is the vertex
-- selected. This tree is infinite so please take care.
generate :: Graph -> Index -> Tree Index
generate g v = Node v (map (generate g) (g!v))

-- | Executes the DFS algorithm in an initial order and then returns a forest.
dfs :: Graph -> [Index] -> Forest Index
dfs g vs = prune (bounds g) (map (generate g) vs)