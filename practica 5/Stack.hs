module Stack (Stack, emptyS, isEmptyS, push, top, pop, lenS)
    where

data Stack = S [] Int


emptyS :: Stack a
emptyS = S [] 0
isEmptyS :: Stack a -> Bool
isEmptyS S _ 0 = True

push :: a -> Stack a -> Stack a
push x (S l c) = (S (x:l) (c+1))

top :: Stack a -> a
top (S (x:xs) _) = x

pop :: Stack a -> Stack a
pop (S (x:xs) c) = S xs (c-1)

lenS :: Stack a -> Int
lenS (S _ l) = l