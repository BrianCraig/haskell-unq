import Queue
import Stack

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



-- interfaz de usuario de Stack

reverseS :: Stack.Stack a -> Stack.Stack a
reverseS a = reverseSAux a emptyS

reverseSAux :: Stack.Stack a -> Stack.Stack a -> Stack.Stack a 
reverseSAux a b = if Stack.isEmptyS a
                     then b
					 else reverseSAux (Stack.pop a) (Stack.push (Stack.top a) b)

balanceado :: String -> Bool
balanceado a = balanceadoAux a Stack.emptyS

balanceadoAux :: String -> Stack.Stack Bool -> Bool
balanceadoAux a b = if a == ""
					  then Stack.isEmptyS b
					  else balanceadoAuxAux (head a) (tail a) b
					    
balanceadoAuxAux :: Char -> String -> Stack.Stack Bool -> Bool
balanceadoAuxAux c s st = if c == '('
					        then balanceadoAux s (Stack.push True st)
					      else if c == ')'
						    then 
							  if Stack.isEmptyS st 
							    then False
							    else balanceadoAux s (Stack.pop st)
					      else balanceadoAux s st

						  
					 
-- Stack.top (reverseS (Stack.push 3 (Stack.push 2 (Stack.push 1 Stack.emptyS))))
-- > 1




