module Set
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
    where

data Set a = S [a] deriving Show
--                  longitud de set
{-
    INV.REP: 
        - No debe tener elementos repetidos
        - La longitud del set debe ser igual a la cantidad de elementos
-}

estaEn :: Eq a => a -> Set a -> Bool
estaEn _ (S [] _) = False
estaEn i (S (x:xs)) = i == x || estaEn i (S xs)


emptyS :: Set a
emptyS = (S [])

setHead :: Set a -> a
setHead (S (x:xs)) = x

setTail :: Set a -> Set a
setTail (S (x:xs)) = (S (xs))

addS :: Eq a => a -> Set a -> Set a
addS a (S is) = (S (a:is))

belongs :: Eq a => a -> Set a -> Bool
belongs item set = estaEn item set

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _ = 0

uniqueElemsCount :: [a] -> Int
uniqueElemsCount (x:xs) = (estaEnL x xs) + uniqueElemsCount xs

sizeS :: Eq a => Set a -> Int
sizeS (S l) = uniqueElemsCount l

removeL :: Eq a => a -> [a] -> [a]
--Precondicion el elemento debe existir en la lista
removeL el (x:xs) = if x == el then removeL el xs else x : removeL el xs

removeS :: Eq a => a -> Set a -> Set a
-- Precondicion: el item debe existir en el set
removeS i (S ls) = (S (removeL i ls))

estaEnL :: Eq a => a -> [a] -> Bool
estaEnL _ [] = False
estaEnL a (x:xs) = x == a || estaEnL a xs

addManyL :: Eq a => [a] -> [a] -> [a]
addManyL [] l = l
addManyL (x:xs) l = if not (estaEnL x l) then x : addManyL xs l else addManyL xs l

addManyS :: Eq a => Set a -> Set a -> Set a
addManyS (S l1) (S l2) = let new_list = addManyL l1 l2 in (S new_list)

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S l1) (S l2) = (S (l1 ++ l2))

setToList :: Eq a => Set a -> [a]
setToList (S l) = l

