module Tree(Tree, EmptyT, NodeT) where

data Tree a = EmptyT | NodeT (Tree a) a (Tree a) deriving (Show)
