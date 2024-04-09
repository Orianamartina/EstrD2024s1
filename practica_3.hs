--1
boolAInt :: Bool -> Int
boolAInt True = 1
boolAInt _ = 0

data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

esColor :: Color -> Color -> Bool
esColor Azul Azul = True  
esColor Rojo Rojo = True  
esColor _ _= False

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas  colorBuscado (Bolita color celda) = boolAInt (esColor color colorBuscado) + nroBolitas colorBuscado celda

poner :: Color -> Celda -> Celda
poner color celda = (Bolita color celda)

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar colorBuscado (Bolita color celda) = if (esColor color colorBuscado) then celda else (Bolita color (sacar colorBuscado celda))

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda
ponerN cantidad colorObjetivo celda = (Bolita colorObjetivo (ponerN (cantidad - 1) colorObjetivo celda) )

--2
data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEnCofre :: [Objeto] -> Bool
hayTesoroEnCofre [] = False
hayTesoroEnCofre (x:xs) = esTesoro x || hayTesoroEnCofre xs

hayTesoro  :: Camino -> Bool 
hayTesoro Fin = False
hayTesoro (Nada camino) = hayTesoro camino
hayTesoro (Cofre objetos camino) = hayTesoroEnCofre objetos || hayTesoro camino 

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Cofre objetos Fin) = 0
pasosHastaTesoro (Nada camino) = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre objetos camino) = if hayTesoroEnCofre objetos then 0 else 1 + pasosHastaTesoro camino

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn n (Nada camino) = if n >= 0 then hayTesoroEn (n-1) camino else False
hayTesoroEn n (Cofre objetos camino) =if n >= 0 then hayTesoroEnCofre objetos || hayTesoroEn (n-1) camino else False

negar :: Bool -> Bool
negar opt = case opt of
    True -> False
    False -> True

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n Fin = negar (n>0)
alMenosNTesoros n (Nada camino) = if n > 0 then alMenosNTesoros n camino else True
alMenosNTesoros n (Cofre objetos camino) = if n> 0 then if hayTesoroEnCofre objetos then alMenosNTesoros (n-1) camino else alMenosNTesoros n camino else True

--3 
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n t1 t2) = (NodeT (2 * n) (mapDobleT t1)  (mapDobleT t2))

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT a (NodeT x t1 t2) = a == x || perteneceT a t1 || perteneceT a t2

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT a (NodeT x t1 t2) =  boolAInt (a == x) + aparicionesT a t1 + aparicionesT a t2

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT _ t1 t2) = leaves t1 ++ leaves t2

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1)  (heightT t2)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1))

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x t1 t2) = x : (toList t1 ++ toList t2)

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN n (NodeT x t1 t2) = if n == 0 then [x] else (levelN (n-1) t1) ++ (levelN (n-1) t2)

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) = [x] : listPerLevel t1 ++ listPerLevel t2

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

listaMasLarga :: [a] -> [a] -> [a]
listaMasLarga l1 l2 = if longitud(l1) > longitud(l2) then l1 else l2

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = x :( listaMasLarga (ramaMasLarga t1) (ramaMasLarga t2))

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x left right) = map (x:) (todosLosCaminos left ++ todosLosCaminos right)

--2.2

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA deriving Show

eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum e1 e2) = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Neg e1) = - (eval e1)

simplificar :: ExpA -> ExpA
simplificar (Sum (Valor 0) x) = x
simplificar (Sum x (Valor 0)) = x
simplificar (Prod (Valor 0) x) = (Valor 0)
simplificar (Prod x (Valor 0)) = (Valor 0)
simplificar (Prod (Valor 1) x ) = x
simplificar (Prod x (Valor 1)) = x
simplificar (Neg (Neg x)) = x
