module Queue_f (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where
    
data Queue a = Q [a]

emptyQ :: Queue a 
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ _ = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Q l) = (Q x:l)

lastOnList :: [a] -> a
lastOnList [x] = x
lastOnList (_ :xs) = lastOnList xs 

removeLast :: [a] -> [a]
removeLast [_] = []
removeLast (x:xs) = x : removeLast xs

firstQ :: Queue a -> a
firstQ (Q l) = lastOnList l

dequeue :: Queue a -> Queue a
dequeue (Q l) = removeLast l
