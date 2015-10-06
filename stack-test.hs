import Stack

s1 = Stack.push 4(Stack.push 3 (Stack.push 2 Stack.emptyS))
-- > S[4,3,2]

test1 = Stack.top s1
-- > 4

test2 = Stack.push 8 s1
-- > S[8,4,3,2]

test3 = Stack.pop (Stack.pop s1)
-- > S[2]