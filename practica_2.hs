-- Práctica 2

-- 1

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = n+1 : sucesores ns 

conjuncion :: [Bool] -> Bool
conjuncion [] = False
conjuncion (x:xs) = x && conjuncion xs

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs 

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

boolAInt :: Bool -> Int
boolAInt True = 1
boolAInt _ = 0

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = boolAInt (e==x) + apariciones e xs

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = if x < n then x : losMenoresA n xs else losMenoresA n xs

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n then x : lasDeLongitudMayorA n xs else  lasDeLongitudMayorA n xs

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] b = [b]
agregarAlFinal (x:xs) b = x : agregarAlFinal xs b

agregar :: [a] -> [a] -> [a]
agregar b [] = b
agregar b (x:xs) = agregar (agregarAlFinal b x) xs

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregar (reversa xs) [x]

maximo :: Int -> Int -> Int
maximo x y = if x >= y then x else y

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos _ [] = []
zipMaximos [] _ = []
zipMaximos (n:ns) (x:xs) = agregar [maximo n x] (zipMaximos ns xs)

elMinimo :: Ord a => [a] -> a
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)