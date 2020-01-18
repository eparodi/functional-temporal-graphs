module Definitions where

import Data.Array

-- | The index of the vertex. This may be useful to get the data inside
-- the vertex from another source.
type Index = Int

-- | An array to retrieve the data.
type Table a = Array Index a

-- | The map function for the Table datatype.
mapT :: (Index -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [(v, f v (t!v)) | v <- indices t]

-- | Transform to list.
mapToList :: (Index -> a -> b) -> Table a -> [(Index, b)]
mapToList f t = [(v, f v (t!v)) | v <- indices t]

toList :: Table a -> [(Index, a)]
toList t = [(v, (t!v)) | v <- indices t]

-- | The minimum and maximum index of the Table. This is very useful for
-- array operations.
type Bounds = (Index, Index)
