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

-- 2 Recursión sobre números

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n < 1 then [] else n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros 0 _ = []
losPrimeros n (x: xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 x = x
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs


-- 3 Registros
data Persona = P String Int deriving Show

esMayorA :: Persona -> Int -> Bool
esMayorA (P _ e) x = e > x

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA e (p:ps) = if esMayorA p e then p : mayoresA e ps else mayoresA e ps

sumaEdades :: [Persona] -> Int
sumaEdades [] = 0
sumaEdades ((P _ e):ps) = e + sumaEdades ps

promedioEdad :: [Persona] -> Int
-- Precondición: la lista al menos posee una persona.
promedioEdad personas = div (sumaEdades personas) (longitud personas)

elMasViejo :: [Persona] -> [Persona]
-- Precondición: la lista al menos posee una persona.
