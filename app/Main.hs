module Main where

import SimpleGraph.Graph

graph = buildG (1, 10) [(1, 10), (1, 7),(2, 9),(2, 1),(3, 8),(3, 5),(5, 10),(5, 8),(5, 4),(6, 9),(7, 6),(7, 2)]

main :: IO ()
main = print graph



