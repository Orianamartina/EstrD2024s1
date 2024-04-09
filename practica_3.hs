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
sumarT (NodeT n arbol1 arbol2) = n + sumarT arbol1 + sumarT arbol2

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ arbol1 arbol2) = 1 + sizeT arbol1 + sizeT arbol2

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n arbol1 arbol2) = (NodeT (2 * n) (mapDobleT arbol1)  (mapDobleT arbol2))

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