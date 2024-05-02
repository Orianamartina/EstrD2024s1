import Set
import Queue_b
import User

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] set = []
losQuePertenecen (x:xs) set = if belongs x set then x : losQuePertenecen xs set else losQuePertenecen xs set


addManyS :: Eq a => [a] -> Set a -> Set a
addManyS [] set = set -- 0 (1)
addManyS (x:xs) set = unionS (addS x set) (addManyS xs set) -- addS  o(n) unionsS o(n) = o(n2)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos l = setToList(addManyS l emptyS)

-- unirTodos :: Eq a => Tree (Set a) -> Set a
-- -- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
-- unirTodos emptyT = emptyS
-- unirTodos (T x b1 b2) = unionS (x (unionsS (unirTodos b1)(unirTodos b2)))
-- new :: Queue a
-- new = enqueue 1 (enqueue 2 (enqueue 3 (enqueue 4 (enqueue 5 emptyQ))))

lengthQ :: Queue a -> Int
lengthQ q = if not (isEmptyQ q) then 1 + lengthQ (dequeue q) else 0

queueToList :: Queue a -> [a]
queueToList q = if not (isEmptyQ q) then [firstQ q] ++ queueToList (dequeue q) else []

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if not (isEmptyQ q2) then enqueue (firstQ q2) (unionQ q1 (dequeue q2)) else emptyQ

-- Stack

pushList :: [a] -> Stack a
pushList (x:xs) = push x (pushList xs)

apilar :: [a] -> Stack a
apilar l = pushList (reverse l)

insertarEnPos :: Int -> a -> Stack a -> Stack a 
insertarEnPos n i s = if n > 0 then insertarEnPos((n-1) i (pop s)) else push i s