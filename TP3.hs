data Tree a = EmptyT | NodeT (Tree a) a (Tree a) deriving (Show)

sumarT :: Tree Integer -> Integer
sumarT EmptyT = 0
sumarT (NodeT t1 a t2) = sumarT t1 + a + sumarT t2

sizeT :: Tree a -> Integer
sizeT EmptyT = 0
sizeT (NodeT t1 a t2) = sizeT t1 + 1 + sizeT t2

mapDobleT :: Tree Integer -> Tree Integer 
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT t1 a t2) = NodeT (mapDobleT t1) (a*2) (mapDobleT t2)

-- mapOpuestoT :: Tree Dir.Dir -> Tree Dir.Dir 
-- mapOpuestoT EmptyT = EmptyT
-- mapOpuestoT (NodeT t1 a t2) = NodeT (mapOpuestoT t1) Dir.opuesto Dir (mapOpuestoT t2)

-- mapLongitudT :: Tree String -> Tree Int 
-- mapLongitudT EmptyT = 
-- mapLongitudT (NodeT t1 a t2) = 

perteneceT :: Eq a => a -> Tree a -> Bool 
perteneceT _ EmptyT = False
perteneceT e (NodeT t1 a t2) = (perteneceT e t1) || a==e || (perteneceT e t2) 

aparicionesT :: Eq a => a -> Tree a -> Int 
aparicionesT _ EmptyT = 0
--aparicionesT e (NodeT t1 a t2) = (aparicionesT e t1) + intToBool(a==e) + (aparicionesT e t2) 


-- t1 = NodeT (NodeT EmptyT 4 EmptyT) 7 (NodeT EmptyT 3 EmptyT)
--   7
--  / \
-- 4   3
