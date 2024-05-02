--1 calculo de costos

head' :: [a] -> a
head' (x:xs) = x

-- Constante 

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- Constante

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--Lineal o(n) donde n = Int

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--lineal o(n) donde n = cantidad de elementos de la lista

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

-- Cuadrática: (n*m) donde n es el costo de factorial y m es el costo de factoriales

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- Lineal o(n)  ya que == es constante, n = a la cantidad de elementos en la lista

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = [] -- constante
sinRepetidos (x:xs) =
    if pertenece x xs --lineal
    then sinRepetidos xs    --lineal
    else x : sinRepetidos   --lineal
    
-- Cuadratica o(n*m) donde n es el costo de pertenece y m es la cantidad de elementos de la lista

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Lineal, donde n es igual a la cantidad de elemntos en la lista 

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

-- Lineal, donde n es igual a la cantidad de elemntos en la lista 

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

-- es lineal donde n es igual a el primer parametro de la funcion.

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

-- es lineal donde n es igual a el primer parametro de la funcion.

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

--Es lineal, o(n) donde n = costo de taken + costo de dropN = 2*n

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- Es lineal donde n = a la cantidad de elementos de la lista

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
    if n == x
    then xs
    else x : sacar n xs

-- es Lineal donde n = a la cantidad de elementos de la lista (en el mejor caso es constante?)

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
    let m = minimo xs
        in m : ordenar (sacar m xs)

-- es cuadratica porque minimo es lineal y se repite n veces (n*n)

emptyS :: Set a
-- Crea un conjunto vacío.
addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
belongs :: Eq a => a -> Set a -> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.

removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
