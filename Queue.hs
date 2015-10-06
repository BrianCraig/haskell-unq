module Queue(Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue) where

data Queue a = Q [a] deriving (Show)

emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ _ = False

queue :: a -> Queue a -> Queue a
queue a (Q b) = Q ([a] ++ b)

firstQ :: Queue a -> a
firstQ (Q []) = error "NO HAY ELEMENTOS"
firstQ (Q (x:[])) = x
firstQ (Q (x:xs)) = firstQ (Q xs)

dequeue :: Queue a -> Queue a
dequeue (Q []) = error "NO SE PUEDE QUITAR UN ELEMENTO DE UNA COLA VACIA"
dequeue (Q (x:[])) = Q []
dequeue (Q (x:xs)) = Q ([x] ++ getList (dequeue (Q xs)))

getList :: Queue a -> [a]
getList (Q a) = a
