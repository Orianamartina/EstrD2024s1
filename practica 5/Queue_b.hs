module Queue_b (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where
    
data Queue a = Q [a] deriving Show

emptyQ :: Queue a 
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ _ = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Q l) = (Q (l ++ [x]))

firstQ :: Queue a -> a
firstQ (Q (x:xs)) = x

dequeue :: Queue a -> Queue a
dequeue (Q (x:xs)) = (Q (xs))


