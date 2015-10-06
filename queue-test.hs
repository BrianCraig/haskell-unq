import Queue

q1 = Queue.queue 2 (Queue.queue 1 Queue.emptyQ)
-- > Q[2,1]


test1 = Queue.firstQ q1
-- > 1

test2 = Queue.dequeue q1
-- > Q[2]

test3 = Queue.firstQ (Queue.dequeue (Queue.dequeue (Queue.queue 3 q1) ))
-- > 3

test4 = Queue.firstQ (Queue.dequeue (Queue.dequeue q1))
-- > error
