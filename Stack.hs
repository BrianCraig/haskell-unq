module Stack(Stack, emptyS, isEmptyS, push, top, pop) where

data Stack a = S [a] deriving (Show)

emptyS :: Stack a
emptyS = S []

isEmptyS :: Stack a -> Bool
isEmptyS (S []) = True
isEmptyS _ = False

push :: a -> Stack a -> Stack a
push a (S b) = S (a:b)

top :: Stack a -> a
top (S []) = error "NO HAY ELEMENTOS"
top (S (x:_)) = x

pop :: Stack a -> Stack a
pop (S []) = error "NO SE PUEDE QUITAR UN ELEMENTO DE UN STACK VACIO"
pop (S (_:xs)) = S xs