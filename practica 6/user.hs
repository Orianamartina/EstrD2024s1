-- import PriorityQueue


-- listToPQ :: Ord a => [a] -> PriorityQueue a
-- listToPQ [] = emptyPQ
-- listToPQ (x:xs) = insertPQ x (listToPQ xs)

-- pqToList :: Ord a => PriorityQueue a -> [a]
-- pqToList q = if q isEmptyPQ then [] else findMinPQ : (deleteMinPQ q)

-- heapSort :: Ord a => [a] -> [a]
-- heapSort l =  pqToList(listToPQ l)

import Map
-- Dict

val :: Maybe v -> v
val (Just v) = v

values :: Eq k => [k] -> Map k v -> [Maybe v] 
values [] _ = []
values (x:xs) map = lookupM x map : values xs map

valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = values (keys map) map

todasAsociadas :: (Eq k, Eq v) => [k] -> Map k v -> Bool
todasAsociadas [] _ = True
todasAsociadas (x:xs) m = not (lookupM x m == Nothing) && todasAsociadas xs m

listToMap :: Eq k => [(k,v)] -> Map k v
listToMap [] = emptyM
listToMap (x:xs) = assocM (fst x) (snd x) (listToMap xs)

get :: Eq k => [k] -> Map k v -> [(k,v)]
get [] _ = []
get (x:xs) m = (x, val((lookupM x m))) : get xs m 

mapToList :: Eq k => Map k v -> [(k,v)]
mapToList m = get (keys m) m

manyFst :: Eq k => [(k,v)] -> [k]
manyFst [] = []
manyFst (x:xs) = fst x : manyFst xs

agruparEq :: (Eq k, Eq v) => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq (x:xs) = 
    let m = agruparEq xs in
        let isAssoc = lookupM (fst x) m in
            if isAssoc /= Nothing then assocM (fst x) ([snd x] ++ val isAssoc) m else assocM (fst x) [snd x] (m)

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Precondicion: todas las keys de la lista deben existir en el diccionario
incrementar [] _ = emptyM
incrementar (x:xs) m = assocM x ((val (lookupM x m)) +1) (incrementar xs m)

indexarLista :: Int -> [a] -> [(Int, a)]
indexarLista _ [] = []
indexarLista n (x: xs) = indexarLista (n+1) xs ++ [(n, x)] 

indexar :: [a] -> Map Int a 
indexar [] = emptyM
indexar l = listToMap (indexarLista 0 l) 

ocurrencias :: String -> Map Char Int
ocurrencias "" = emptyM
ocurrencias str = 
    let rest_of = ocurrencias (tail str) in
        let found = lookupM (head str) rest_of in
            if found == Nothing then assocM (head str) 1 rest_of else assocM (head str) ((val found )+ 1) (deleteM (head str) rest_of)
