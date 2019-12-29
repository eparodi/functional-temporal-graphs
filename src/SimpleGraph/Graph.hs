module SimpleGraph.Graph where

import Definitions
import Data.Array

-- | This is a simple directed graph represented with and adjacency list. All the
-- algorithms in this type are defined for graphs where there is only one edge
-- between two nodes.
type Graph = Table [Index]

-- | An edge, defined by the source vertex and the destination vertex.
type Edge = (Index, Index)

-- | Returns a list of Edges from a Graph.
edges :: Graph -> [Edge]
edges g = [(v, w) | v <- indices g, w <- g!v]

-- | Returns a Table that matches Index of a vertex with the amount of
-- edges that start in that node.
outdegree :: Graph -> Table Int
outdegree g = mapT numEdges g
    where numEdges v ws = length ws

-- | From a list of edges, it creates a Graph.
buildG :: Bounds -> [Edge] -> Graph
buildG bnds es = accumArray (flip (:)) [] bnds es

-- | Returns a list of the edges reversed. This means that if the edge
-- goes from a to b, the reversed edge will go from b to a.
reverseE :: Graph -> [Edge]
reverseE g = [(w, v) | (v, w) <- edges g]

-- | Returns the Graph with all the edges reversed.
transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

-- | Returns a Table that matches Index of a vertex with the amount of
-- edges that end in that node.
indegree :: Graph -> Table Int
indegree g = outdegree (transposeG g)