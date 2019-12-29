module Tree where

-- | The definition of a Tree. The Tree is defined as a Node whose children
-- can be as many Trees as we want. This set of Trees is called Forest.
data Tree a = Node a (Forest a) deriving (Show)
type Forest a = [Tree a]

-- | From a Tree gets a list of the nodes in preorder.
preorder :: Tree a -> [a]
preorder (Node a ts) = [a]  ++ preorderF ts

-- | From a Forest it calculates the preorder of all the Trees inside and
-- flats the list.
preorderF :: Forest a -> [a]
preorderF ts = concat (map preorder ts)

-- | From a Tree gets a list of the nodes in postorder.
postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

-- | From a Forest it calculates the postorder of all the Trees inside and
-- flats the list.
postorderF :: Forest a -> [a]
postorderF ts = concat (map postorder ts)