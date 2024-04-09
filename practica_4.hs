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

ramaMasLarga :: Mapa -> [a]
ramaMasLarga (Fin _) = []
ramaMasLarga (Bifurcacion _ m1 m2) = x :(listaMasLarga (ramaMasLarga m1) (ramaMasLarga m2))

-- caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- caminoDeLaRamaMasLarga (Fin _) = []
-- caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = if (rama)

-- hayTesoroEn (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra, Chatarra])) (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra])) (Fin (Cofre [Tesoro]))))
