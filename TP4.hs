import Queue


-- interfaz de usuario de Queue

largoQ :: Queue.Queue a -> Int
largoQ a = if Queue.isEmptyQ a
              then 0
              else 1 + largoQ (Queue.dequeue a)

listToQueue :: [a] -> Queue.Queue a
listToQueue [] = Queue.emptyQ
listToQueue a = Queue.queue (last a) (listToQueue (init a))

queueToList :: Queue.Queue a -> [a]
queueToList a = if Queue.isEmptyQ a
                  then []
                  else [Queue.firstQ a] ++ queueToList (Queue.dequeue a)

-- let q1 = Queue.queue 5 (Queue.queue 4(Queue.queue 3 Queue.emptyQ))
-- > Q [5,4,3]

-- let l1 = queueToList q1
-- > [3,4,5]

-- listToQueue l1
-- > Q [5,4,3]
