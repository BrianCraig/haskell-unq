module Heap(Heap, emptyH, isEmptyH, insertH, findMinH, deleteH) where

import Tree

data Dir = Izq | Der deriving (Show)

type Ocurrencia = [Dir]

data Heap a = H (Tree a) Ocurrencia
instance (Show a) => Show (Heap a) where
  show (H a _) = show a


emptyH :: Heap a -- O(1)
emptyH = H emptyT []

isEmptyH :: Heap a -> Bool -- O(1)
isEmptyH (H t _) = isEmptyT t

findMinH :: Heap a -> a -- O(1)
findMinH (H t _) = rootT t

insertH :: (Ord a) => Heap a -> a -> Heap a -- O(log n)
insertH (H t o) x = H (agregarEn (reverse o) t x) (nextPos o)

deleteH :: (Ord a) => Heap a -> Heap a -- O(alachoten)
deleteH (H t o) = 
  let prevO = prevPos o
  in H (borrarDe (reverse prevO) t) prevO



prevPos :: Ocurrencia -> Ocurrencia
prevPos [Izq]    = []
prevPos (Izq:xs) = (Der: (prevPos xs))
prevPos (Der:xs) = (Izq: xs)

nextPos :: Ocurrencia -> Ocurrencia
nextPos []       = [Izq]
nextPos (Izq:xs) = (Der:xs)
nextPos (Der:xs) = (Izq:(nextPos xs))



agregarEn :: (Ord a) => Ocurrencia -> Tree a -> a -> Tree a
agregarEn _ EmptyT x = NodeT EmptyT x EmptyT
agregarEn (Izq:xs) (NodeT t1 a t2) x = 
  if x > a
    then NodeT (agregarEn xs t1 x) a t2
    else NodeT (agregarEn xs t1 a) x t2
agregarEn (Der:xs) (NodeT t1 a t2) x = 
  if x > a
    then NodeT t1 a (agregarEn xs t2 x)
    else NodeT t1 x (agregarEn xs t2 a)

-- Acá viene lo feo feo feo

borrarDe :: (Ord a) => Ocurrencia -> Tree a -> Tree a
borrarDe o t = reordenarT (quitarYReemplazar o t)

quitarYReemplazar :: Ocurrencia -> Tree a -> Tree a
quitarYReemplazar [] _ = EmptyT
quitarYReemplazar (Izq:xs) (NodeT (NodeT t1P aP t2P) a t2) = 
  NodeT (quitarYReemplazar xs (NodeT t1P a t2P)) aP t2
quitarYReemplazar (Der:xs) (NodeT t1 a (NodeT t1P aP t2P)) = 
  NodeT t1 aP (quitarYReemplazar xs (NodeT t1P a t2P))

reordenarT :: (Ord a) => Tree a -> Tree a
reordenarT EmptyT = EmptyT
reordenarT (NodeT EmptyT x EmptyT) = NodeT EmptyT x EmptyT
reordenarT (NodeT (NodeT t1 a t2) x EmptyT) = 
  if x > a
    then NodeT (NodeT t1 x t2) a EmptyT
    else NodeT (NodeT t1 a t2) x EmptyT
reordenarT (NodeT (NodeT t1 a t2) x (NodeT t1P aP t2P)) =
  if (x < a) && (x < aP)
    then NodeT (NodeT t1 a t2) x (NodeT t1P aP t2P)
    else if a < aP
      then NodeT (reordenarT (NodeT t1 x t2)) a (NodeT t1P aP t2P) -- Izq
      else NodeT (NodeT t1 a t2) aP (reordenarT (NodeT t1P x t2P)) -- Der


listaAHeap :: (Ord a) => [a] -> Heap a -> Heap a
listaAHeap [] a = a
listaAHeap (x:xs) a = insertH (listaAHeap xs a) x  

-- h1 = listaAHeap [5,6,7,-10,33,457,2,45,799,87,111,2535,-10,4] emptyH
