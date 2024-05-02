module Set
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
    where

data Set a = S [a] Int deriving Show
--                  longitud de set
{-
    INV.REP: 
        - No debe tener elementos repetidos
        - La longitud del set debe ser igual a la cantidad de elementos
-}

estaEn :: Eq a => a -> Set a -> Bool
estaEn _ (S [] _) = False
estaEn i (S (x:xs) l) = i == x || estaEn i (S xs (l-1))


emptyS :: Set a
emptyS = (S [] 0)

setHead :: Set a -> a
setHead (S (x:xs) _) = x

setTail :: Set a -> Set a
setTail (S (x:xs) int) = (S (xs) (int-1))

appendItem :: a -> Set a -> Set a
appendItem a (S is l) = (S (a:is) (l+1))

addS :: Eq a => a -> Set a -> Set a
addS el set = if not (estaEn el set) then appendItem el set else set

belongs :: Eq a => a -> Set a -> Bool
belongs item set = estaEn item set

sizeS :: Eq a => Set a -> Int
sizeS (S _ l) = l

removeL :: Eq a => a -> [a] -> [a]
--Precondicion el elemento debe existir en la lista
removeL el (x:xs) = if x == el then xs else x : removeL el xs

removeS :: Eq a => a -> Set a -> Set a
-- Precondicion: el item debe existir en el set
removeS i (S ls l) = (S (removeL i ls) l)

estaEnL :: Eq a => a -> [a] -> Bool
estaEnL _ [] = False
estaEnL a (x:xs) = x == a || estaEnL a xs

addManyL :: Eq a => [a] -> [a] -> [a]
addManyL [] l = l
addManyL (x:xs) l = if not (estaEnL x l) then x : addManyL xs l else addManyL xs l

addManyS :: Eq a => Set a -> Set a -> Set a
addManyS (S l1 c) (S l2 c2) = let new_list = addManyL l1 l2 in (S new_list (length new_list))

unionS :: Eq a => Set a -> Set a -> Set a
unionS s1 s2 =  addManyS s1 s2

setToList :: Eq a => Set a -> [a]
setToList (S l _) = l

