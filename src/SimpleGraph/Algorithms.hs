module SimpleGraph.Algorithms where

import Definitions
import Tree

import SimpleGraph.Graph
import SimpleGraph.DFS

import Data.Array

-- | Depth First Forest. Returns a Forest that was built using the DFS algorithm using the order of the vertices.
dff :: Graph -> Forest Index
dff g = dfs g (indices g)

preOrd :: Graph -> [Index]
preOrd g = preorderF (dff g)

postOrd :: Graph -> [Index]
postOrd g = postorderF (dff g)

topSort :: Graph -> [Index]
topSort g = reverse (postOrd g)

reachable :: Graph -> Index -> [Index]
reachable g v = preorderF (dfs g [v])

path :: Graph -> Index -> Index -> Bool
path g v w = w `elem` (reachable g v)

undirected :: Graph -> Graph
undirected g = buildG (bounds g) (edges g ++ reverseE g)

components :: Graph -> Forest Index
components g = dff (undirected g)

scc :: Graph -> Forest Index
scc g = dfs (transposeG g) (reverse (postOrd g))

tree :: Bounds -> Forest Index -> Graph
tree bnds ts = buildG bnds (concat (map flat ts))
    where
        flat (Node v ts) = [(v, w) | Node w us <- ts] ++ concat (map flat ts)

back :: Graph -> Table Int -> Graph
back g post = mapT select g
    where select v ws = [w | w <- ws, post!v<post!w]

cross :: Graph -> Table Int -> Table Int -> Graph
cross g pre post = mapT select g
    where select v ws = [w | w <- ws, post!v>post!w, pre!v>pre!w]

forward :: Graph -> Graph -> Table Int -> Graph
forward g tree pre = mapT select g
    where select v ws = [w | w <- ws, pre!v<pre!w]

bcc :: Graph -> Forest [Index]
bcc g =  (concat . map bicomps . map (label g dnum)) forest
    where forest = (dff g)
          dnum = preArr (bounds g) forest

label :: Graph -> Table Int -> Tree Index -> Tree (Index, Int, Int)
label g dnum (Node v ts) = Node (v, dnum!v, lv) us
    where us = map (label g dnum) ts
          lv = minimum ([dnum!v] ++ [dnum!w | w <- g!v] ++ [lu | Node(u, dw, lu) xs <- us])

bicomps :: Tree (Index, Int, Int) -> Forest [Index]
bicomps (Node (v, dv, lv) ts) = [Node (v:vs) us | (l, Node vs us) <- map collect ts]

collect :: Tree (Index, Int, Int) -> (Int, Tree [Index])
collect (Node (v, dv, lv) ts) = (lv, Node (v:vs) cs)
    where collected = map collect ts
          vs = concat [ws | (lw, Node ws us) <- collected, lw < dv]
          cs = concat [if lw <dv then us else [Node (v:ws) us] | (lw, Node ws us) <- collected]

traversal :: Graph -> Index -> [Index]
traversal g v = g!v