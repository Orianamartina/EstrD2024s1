module QueueD (QueueD, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where

data QueueD a = Q [a] [a]

emptyQ :: QueueD a
emptyQ = Q [] []

isEmptyQ :: QueueD a -> Bool
isEmptyQ (Q [] _) = True
isEmptyQ _ = False

enqueue :: a -> QueueD a -> QueueD a
enqueue x (Q fs bs) = (Q fs (bs ++ [x]))

firstQ :: QueueD a -> a
firstQ (Q (x:xs) _) = x

dequeue :: QueueD a -> QueueD a
dequeue (Q [x] bs) = (Q (reverse bs) [])
dequeue (Q (x:xs) bs) = (Q xs bs) 

