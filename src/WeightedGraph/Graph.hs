module WeightedGraph.Graph where

import Definitions
import Data.Array

-- | This is a tuple that describes where the edge ends and the weight of the edge.
type EdgeNode a = (Index, a)

-- | This is a weighted directed graph represented with and adjacency list.
type WeightedGraph a = Table [EdgeNode a]

-- | An edge, defined by the source vertex and the destination vertex.
type WeightedEdge a = (Index, Index, a)

-- | Returns a list of WeightedEdge from a WeightedGraph.
weightedEdges :: WeightedGraph a -> [WeightedEdge a]
weightedEdges g = [(v1, v2, w) | v1 <- indices g, (v2, w) <- g!v1]

-- | Returns a Table that matches Index of a vertex with the amount of
-- edges that start in that node.
outdegreeW :: WeightedGraph a -> Table Int
outdegreeW g = mapT numEdges g
    where numEdges v ws = length ws

-- | From a list of WeightedEdges, it creates a WeightedGraph.
buildWG :: Bounds -> [WeightedEdge a] -> WeightedGraph a
buildWG bnds es = accumArray (flip (:)) [] bnds [(v1, (v2, w)) | (v1, v2, w) <- es]

-- | Returns a list of the edges reversed. This means that if the edge
-- goes from a to b, the reversed edge will go from b to a.
reverseWE :: WeightedGraph a -> [WeightedEdge a]
reverseWE g = [(v2, v1, w) | (v1, v2, w) <- weightedEdges g]

-- | Returns the Graph with all the edges reversed.
transposeWG :: WeightedGraph a -> WeightedGraph a
transposeWG g = buildWG (bounds g) (reverseWE g)

-- | Returns a Table that matches Index of a vertex with the amount of
-- edges that end in that node.
indegreeW :: WeightedGraph a -> Table Int
indegreeW g = outdegreeW (transposeWG g)