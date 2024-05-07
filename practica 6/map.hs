module Map (Map, emptyM, assocM, lookupM, deleteM, keys) 
where

data Map k v = M [(k,v)] deriving Show
{- INV.REP.: en M kvs, no hay claves repetidas en kvs -}

first :: (k,v) -> k
first (a, _) = a

second :: (k,v) -> v
second (_, a) = a

emptyM :: Map k v
emptyM = M []

isAssoc :: Eq k => k-> [(k,v)] -> Bool
isAssoc _ [] = False
isAssoc a (x:xs) =  (first x) == a || isAssoc a xs

removeFromPairs :: Eq k => k -> [(k,v)] -> [(k,v)]
removeFromPairs a (x:xs) = if a == fst x then xs else x : removeFromPairs a xs

assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM a b (M pairs) = if not (isAssoc a pairs) then M (pairs ++ [(a,b)]) else (M ((removeFromPairs a pairs) ++ [(a,b)]) )

lookupL ::  Eq k => k -> [(k,v)] -> Maybe v
lookupL a [] = Nothing
lookupL a (x:xs) = if a == first(x) then Just (second(x)) else lookupL a xs

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM a (M pairs) = lookupL a pairs

deleteL :: Eq k => k -> [(k,v)] -> [(k,v)]
deleteL _ [] = []
deleteL a (x:xs) = if first(x) == a then xs else x : deleteL a xs

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM a (M pairs) = (M (deleteL a pairs))

keysL :: [(k,v)] -> [k]
keysL [] = []
keysL (x:xs) = first(x) : keysL xs

keys :: Map k v -> [k]
keys (M pairs) = keysL pairs
