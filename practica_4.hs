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
data Objeto = Tesoro | Chatarra deriving Show
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

tesorosDe :: [Objeto] -> [Objeto]
tesorosDe [] = []
tesorosDe (x:xs) = if esTesoro x then x : tesorosDe xs else tesorosDe xs

tesorosDeCofre :: Cofre -> [Objeto]
tesorosDeCofre (Cofre []) = []
tesorosDeCofre (Cofre lista) = tesorosDe lista

zipL :: [[a]] -> [[a]] -> [[a]]
zipL [] l = l
zipL l [] = l
zipL (x:xs) (y:ys) = (x ++ y) : zipL xs ys

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cofre) = [tesorosDeCofre cofre]
tesorosPorNivel (Bifurcacion cofre m1 m2) = tesorosDeCofre cofre : (zipL (tesorosPorNivel m1) (tesorosPorNivel m2))

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ m1 m2) = (agregarAlPrincipio Izq (todosLosCaminos m1)) ++ (agregarAlPrincipio Der (todosLosCaminos m2))

agregarAlPrincipio :: a -> [[a]] -> [[a]]
agregarAlPrincipio x [] = []
agregarAlPrincipio x (y:ys) = (x:y) : agregarAlPrincipio x ys

-- ej: (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra, Chatarra])) (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra])) (Bifurcacion (Cofre [Tesoro]) (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra, Chatarra])) (Fin (Cofre [Chatarra, Chatarra]))) (Fin (Cofre [Chatarra, Chatarra])))))

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

-- ej (N (NodeT (S "1" [LanzaTorpedos, (Motor 6), (LanzaTorpedos), (Almacen [Comida, Torpedo])] []) (NodeT (S "2" [LanzaTorpedos, (Motor 10)] []) (EmptyT) (EmptyT)) (NodeT (S "3" [(Motor 1), LanzaTorpedos, (Almacen [Comida, Oxigeno, Combustible])] []) (EmptyT) (EmptyT))))

--4
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre deriving Show
data Manada = M Lobo deriving Show

manada :: Manada
manada = (M (Cazador "lobo_cazador" ["presa_1", "presa_2", "Presa_3"] (Explorador "lobo_explorador_1" ["territorio_1", "territorio_2", "territorio_3"] (Cria "cria_1")(Cria "cria_2"))(Explorador "lobo_explorar_2" ["territorio_4", "territorio_5"] (Cria "cria_3")(Cria "cria_4"))(Cria "cria_5")))

manada_2 :: Manada
manada_2 = (M (Cazador "lobo_cazador" ["presa_1", "presa_2", "Presa_3", "presa_4", "presa_5"] (Explorador "lobo_explorador_1" ["territorio_1", "territorio_2", "territorio_3"] (Cria "cria_1")(Cria "cria_2"))(Explorador "lobo_explorar_2" ["territorio_4", "territorio_5"] (Cria "cria_3")(Cria "cria_4"))(Cazador "cazador_2" ["presa_6", "presa_7", "presa_8", "presa_9"] (Cria "cria_6") (Cria "cria_7") (Cria "cria_8"))))

lobo_1 :: Lobo
lobo_1 = (Cazador "lobo_cazador" ["presa_1", "presa_2", "Presa_3", "presa_4", "presa_5"] (Explorador "lobo_explorador_1" ["territorio_1", "territorio_2", "territorio_3", "territorio_4", "territorio_5"] (Cria "cria_1")(Cria "cria_2"))(Explorador "lobo_explorar_2" ["territorio_4", "territorio_5"] (Cria "cria_3")(Cria "cria_4"))(Cazador "cazador_2" ["presa_6", "presa_7", "presa_8", "presa_9"] (Cria "cria_6") (Cria "cria_7") (Cria "cria_8")))

buenaCazaDeLobos :: Lobo -> Int
buenaCazaDeLobos (Cria _) = -1
buenaCazaDeLobos (Explorador _  _ l1 l2) = buenaCazaDeLobos l1 + buenaCazaDeLobos l2
buenaCazaDeLobos (Cazador _ p l1 l2 l3) = longitud p + buenaCazaDeLobos l1 + buenaCazaDeLobos l2 + buenaCazaDeLobos l3

buenaCaza :: Manada -> Bool
buenaCaza (M lobo) = buenaCazaDeLobos lobo > 0

elQueMasCazo :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elQueMasCazo (n1, c1) (n2, c2) = if c1 > c2 then (n1, c1) else (n2, c2) 

elAlfaDeLobos :: Lobo -> (Nombre, Int)
elAlfaDeLobos (Cria n) = (n, 0)
elAlfaDeLobos (Explorador n _ l1 l2) = elQueMasCazo (n, 0) (elQueMasCazo (elAlfaDeLobos l1) (elAlfaDeLobos l2))
elAlfaDeLobos (Cazador n p l1 l2 l3) = elQueMasCazo (n, longitud p) (elQueMasCazo (elAlfaDeLobos l1) (elQueMasCazo (elAlfaDeLobos l2) (elAlfaDeLobos l3)))

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaDeLobos lobo

lobosQueExploraron :: Territorio -> Lobo -> [Nombre]
lobosQueExploraron _ (Cria _) = []
lobosQueExploraron t (Explorador n tr l1 l2) = if estaEn t tr then n : lobosQueExploraron t l1 ++ lobosQueExploraron t l2 else lobosQueExploraron t l1 ++ lobosQueExploraron t l2 
lobosQueExploraron t (Cazador _ _ l1 l2 l3) = lobosQueExploraron t l1 ++ lobosQueExploraron t l2 ++ lobosQueExploraron t l3

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M lobo) = lobosQueExploraron t lobo

territorios :: Lobo -> [Territorio]
territorios (Cria _) = []
territorios (Cazador _ _ l1 l2 l3) = agregarSiNoEstan (territorios l1) (territorios l2 ++ territorios l3)
territorios (Explorador _ tr l1 l2) = agregarSiNoEstan tr (territorios l1 ++ territorios l2)


lobosPorTerritorio :: [Territorio] -> Lobo -> [[Nombre]]
lobosPorTerritorio [] _ = []
lobosPorTerritorio (x:xs) lobo = lobosQueExploraron x lobo : lobosPorTerritorio xs lobo

exploradoresPorTerritorioDeLobos :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioDeLobos lobo = zip (territorios lobo) (lobosPorTerritorio (territorios lobo) lobo)

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = exploradoresPorTerritorioDeLobos lobo

estaLoboEn :: Nombre -> Lobo -> Bool
estaLoboEn n (Cria nl) = n == nl
estaLoboEn n (Cazador nl _ l1 l2 l3) = n == nl || estaLoboEn n l1 || estaLoboEn n l2 || estaLoboEn n l3
estaLoboEn n (Explorador nl _ l1 l2) = n == nl || estaLoboEn n l1 || estaLoboEn n l2

lobosHasta :: Nombre -> Lobo -> [Nombre]
lobosHasta n (Cria nl) = []
lobosHasta n (Explorador nl _ l1 l2) = if estaLoboEn n l1 then lobosHasta n l1 else lobosHasta n l2
lobosHasta n (Cazador nl l l1 l2 l3) =  if n == nl then [] else if estaLoboEn n l1 then nl:  lobosHasta n l1 else if estaLoboEn n l2 then nl: lobosHasta n l2 else nl :lobosHasta n l3

superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M lobo) = lobosHasta n lobo
