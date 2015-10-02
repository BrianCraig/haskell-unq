import Queue

q1 = Queue.queue 2 (Queue.queue 1 Queue.emptyQ)
-- q1 = "primero" y "segundo"


test1 = firstQ q1
--> 1

test2 = dequeue q1
--> Q[2]

test3 = firstQ (Queue.dequeue (Queue.dequeue (Queue.queue 3 q1) ))
--> 3

test4 = firstQ (Queue.dequeue (Queue.dequeue q1))
--> error