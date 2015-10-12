module Tree(Tree(EmptyT, NodeT), isEmptyT, emptyT, rootT) where

data Tree a = EmptyT | NodeT (Tree a) a (Tree a)
instance (Show a) => Show (Tree a) where
  show a = unlines (printT a)

isEmptyT :: Tree a -> Bool
isEmptyT EmptyT = True
isEmptyT _ = False

emptyT :: Tree a
emptyT = EmptyT

rootT :: Tree a -> a
rootT (NodeT _ a _) = a




printT (NodeT t1 a t2)
    = (show a) : (printSubT t1 t2)
        where
            printSubT t1 t2 =
                ((pad "D " "| ") (printT t2))
                    ++ ((pad "I " "  ") (printT t1))
            pad first rest = zipWith (++) (first : repeat rest)

printT (EmptyT)   = []
