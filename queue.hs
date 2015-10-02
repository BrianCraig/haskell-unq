module Queue(Queue, emptyQ, queue, firstQ, dequeue) where

data Queue a = Q [a]

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

dequeue :: Queue a -> Queue a -- Esta mal implementado
dequeue (Q []) = Q []
dequeue (Q (x:xs)) = Q xs

