data Pizza = Prepizza| Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ pizza) = 1 + cantidadDeCapas pizza

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = (Capa x (armarPizza xs))

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa Jamon p) =  sacarJamon p
sacarJamon (Capa ingrediente p) = (Capa ingrediente (sacarJamon p))

negar :: Bool -> Bool
negar opt = case opt of
    True -> False
    False -> True

esIngrediente ::Ingrediente -> Ingrediente -> Bool
esIngrediente Salsa Salsa = True
esIngrediente Queso Queso = True
esIngrediente Jamon Jamon = True
esIngrediente (Aceitunas _) (Aceitunas _) = True
esIngrediente _ _ = False

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ingrediente pizza) = negar (esIngrediente Jamon ingrediente) && negar (esIngrediente (Aceitunas 1) ingrediente) && tieneSoloSalsaYQueso pizza 

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa (Aceitunas n) pizza) = (Capa (Aceitunas (n*2)) (duplicarAceitunas pizza))
duplicarAceitunas (Capa ingrediente pizza) = (Capa ingrediente (duplicarAceitunas pizza))

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = ((cantidadDeCapas x), x) : cantCapasPorPizza xs

--2

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (x:xs) = esTesoro x || hayTesoroEnLista xs

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre objetos) = hayTesoroEnLista objetos

hayTesoroEnPosicionActual :: Mapa -> Bool
hayTesoroEnPosicionActual (Fin cofre) = hayTesoroEnCofre cofre
hayTesoroEnPosicionActual (Bifurcacion cofre _ _) = hayTesoroEnCofre cofre

hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre) = hayTesoroEnCofre cofre
hayTesoro (Bifurcacion cofre m1 m2) = hayTesoroEnCofre cofre || hayTesoro m1 || hayTesoro m2

esIzquierda :: Dir -> Bool
esIzquierda Izq = True
esIzquierda _ = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] mapa = hayTesoroEnPosicionActual mapa
hayTesoroEn _ (Fin cofre)  = False
hayTesoroEn (x:xs) (Bifurcacion _ m1 m2) = if esIzquierda x then hayTesoroEn xs m1 else hayTesoroEn xs m2

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre) = []
caminoAlTesoro (Bifurcacion cofre m1 m2) = if hayTesoroEnCofre cofre then [] else if hayTesoro m1 then Izq : caminoAlTesoro m1 else Der : caminoAlTesoro m2 

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

listaMasLarga :: [a] -> [a] -> [a]
listaMasLarga l1 l2 = if longitud(l1) > longitud(l2) then l1 else l2


caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) =  listaMasLarga (Izq : caminoDeLaRamaMasLarga m1) (Der : caminoDeLaRamaMasLarga m2)


-- caminoDeLaRamaMasLarga (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra, Chatarra])) (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra])) (Bifurcacion (Cofre [Tesoro]) (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra, Chatarra])) (Fin (Cofre [Chatarra, Chatarra]))) (Fin (Cofre [Chatarra, Chatarra])))))


--3 
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String 
type Tripulante = String 
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

sectoresDentroDeSectores :: Tree Sector -> [SectorId]
sectoresDentroDeSectores EmptyT = []
sectoresDentroDeSectores (NodeT (S id _ _) t1 t2) = id : sectoresDentroDeSectores t1 ++ sectoresDentroDeSectores t2

sectores :: Nave -> [SectorId]
sectores (N (EmptyT)) = []
sectores (N (NodeT (S id _ _) t1 t2)) = id : (sectoresDentroDeSectores t1) ++ (sectoresDentroDeSectores t2)

esComponente :: Componente -> Componente -> Bool
esComponente (Motor _) (Motor _) = True
esComponente LanzaTorpedos LanzaTorpedos = True
esComponente (Almacen _) (Almacen _) = True
esComponente _ _ = False

motoresEnComponentes :: [Componente] -> [Componente]
motoresEnComponentes [] = []
motoresEnComponentes (x:xs) = if esComponente x (Motor 1) then x : motoresEnComponentes xs else motoresEnComponentes xs

componentes :: Tree Sector -> [Componente]
componentes EmptyT = []
componentes (NodeT (S _ c _) t1 t2) = c ++ componentes t1 ++ componentes t2

motoresDe :: Nave -> [Componente]
motoresDe (N (EmptyT)) = []
motoresDe (N (NodeT (S _ c _ ) t1 t2)) = motoresEnComponentes c ++ motoresEnComponentes (componentes t1) ++ motoresEnComponentes (componentes t2)

sumaDePropulsion :: [Componente] -> Int
-- Los componentes deben ser motores
sumaDePropulsion [] = 0
sumaDePropulsion ((Motor n):xs) = n + sumaDePropulsion xs 

poderDePropulsion :: Nave -> Int
poderDePropulsion (N (EmptyT)) = 0
poderDePropulsion nave = sumaDePropulsion (motoresDe nave)


barrilesEnAlmacen :: Componente -> [Barril]
-- El componente debe ser un almacen
barrilesEnAlmacen (Almacen lista) = lista

barrilesEn :: [Componente] -> [Barril]
barrilesEn [] = []
barrilesEn (x:xs) = if esComponente x (Almacen []) then barrilesEnAlmacen x ++ barrilesEn xs else barrilesEn xs

barrilesEnSector :: Sector -> [Barril]
barrilesEnSector (S _ [] _) = []
barrilesEnSector (S _ l _) = barrilesEn l  

barrilesEnSectores :: Tree Sector -> [Barril]
barrilesEnSectores (EmptyT) = []
barrilesEnSectores (NodeT s t1 t2) =  barrilesEnSector s ++ barrilesEnSectores t1 ++ barrilesEnSectores t2

barriles :: Nave -> [Barril]
barriles (N (EmptyT)) = []
barriles (N tree) = barrilesEnSectores tree

agregarASectorEn :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorEn _ _ EmptyT = EmptyT
agregarASectorEn l id (NodeT (S sid c t) t1 t2) = if id == sid then (NodeT (S sid (c++l) t) t1 t2) else (NodeT (S sid c t) (agregarASectorEn l id t1) (agregarASectorEn l id t2))

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector l id (N tree) = (N (agregarASectorEn l id tree))

sacar :: Eq a => a -> [a] -> [a]
-- Saca la primer coincidencia
sacar _ [] = []
sacar i (x:xs) = if x == i then xs else (x : (sacar i xs))

estaEn :: Eq a => a -> [a] -> Bool
estaEn _ [] = False
estaEn i (x:xs) = i == x || estaEn i xs


agregarTripulanteEn :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
agregarTripulanteEn _ _ EmptyT = EmptyT
agregarTripulanteEn _ [] tree = tree
agregarTripulanteEn t ids (NodeT (S sid c tr) t1 t2) = 
    if estaEn sid ids then (NodeT (S sid c (t:tr)) (agregarTripulanteEn t (sacar sid ids) t1) (agregarTripulanteEn t (sacar sid ids) t2)) 
    else (NodeT (S sid c tr) (agregarTripulanteEn t (sacar sid ids) t1)(agregarTripulanteEn t (sacar sid ids) t2))

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA t l (N tree) = (N (agregarTripulanteEn t l tree))

sectoresAsignadosEn :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosEn _ EmptyT = []
sectoresAsignadosEn t (NodeT (S sid _ tr) t1 t2) = if  estaEn t tr then sid : sectoresAsignadosEn t t1 ++ sectoresAsignadosEn t t2 else sectoresAsignadosEn t t1 ++ sectoresAsignadosEn t t2 

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N tree) = sectoresAsignadosEn t tree

agregarSiNoEstan :: Eq a => [a] -> [a] -> [a]
agregarSiNoEstan [] l = l
agregarSiNoEstan (x:xs) l = if not(estaEn x l) then (x: agregarSiNoEstan xs l) else agregarSiNoEstan xs l 

tripulantesEn :: Tree Sector -> [Tripulante]
tripulantesEn EmptyT  = []
tripulantesEn (NodeT (S _ _ tr) t1 t2)  = agregarSiNoEstan tr (tripulantesEn t1  ++ tripulantesEn t2 )

tripulantes :: Nave -> [Tripulante]
tripulantes (N tree) = tripulantesEn tree

--4


-- agregarASector l id (N (NodeT (S sid c t))) = if id == sid then (N (NodeT (S sid (c++l) t))) 
-- (N (NodeT (S "1" [LanzaTorpedos, (Motor 6), (LanzaTorpedos), (Almacen [Comida, Torpedo])] []) (NodeT (S "2" [LanzaTorpedos, (Motor 10)] []) (EmptyT) (EmptyT)) (NodeT (S "3" [(Motor 1), LanzaTorpedos, (Almacen [Comida, Oxigeno, Combustible])] []) (EmptyT) (EmptyT))))