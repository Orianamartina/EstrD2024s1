
-- 2. Números enteros
-- 1:
-- a
sucesor  :: Int -> Int
sucesor n = n+1
-- b
sumar :: Int -> Int -> Int
sumar n m = n + m
-- c
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n 0 = error "El divisor no puede ser 0"
divisionYResto n m = (division, resto)
    where
        division = div n m
        resto = mod n m
-- d
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if (n > m) then n
            else m

-- 2
-- Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))

-- maxDelPar (divisionYResto (sucesor 99) (sumar 9 1))
-- maxDelPar (divisionYResto (sumar (sucesor 119) 12) (sucesor 11))
-- maxDelPar (divisionYResto (sumar (sucesor 24) 25) (sucesor 4))
-- maxDelPar (divisionYResto (sumar 50 51) (sucesor 9))

--3
data Dir = Norte | Este | Sur | Oeste deriving Show

opuesto :: Dir -> Dir
opuesto d = 
   case d of
    Norte     -> Sur
    Este      -> Oeste
    Sur       -> Norte
    Oeste     -> Este


iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales Este Este = True


siguiente :: Dir -> Dir
siguiente d = 
    case d of 
        Norte -> Este
        Sur -> Oeste
        Este -> Sur
        Oeste -> error "No existe siguiente direccion a Oeste"

-- Es una funcion parcial por que no esta definida para todas las posibilidades de dir

--3.2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

obtenerPrimeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
obtenerPrimeroYUltimoDia = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes
ultimoDia :: DiaDeSemana
ultimoDia = Domingo

empiezaConM :: DiaDeSemana -> Bool
empiezaConM dia = case dia of
    Martes -> True
    Miercoles -> True
    _ -> False

numeroDeDiaDeSemana :: DiaDeSemana -> Int
numeroDeDiaDeSemana dia = case dia of
    Lunes -> 1
    Martes -> 2
    Miercoles -> 3
    Jueves -> 4
    Viernes -> 5
    Sabado -> 6
    Domingo -> 7

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = (numeroDeDiaDeSemana dia1) > (numeroDeDiaDeSemana dia2)

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio dia = (numeroDeDiaDeSemana dia) > 1 && (numeroDeDiaDeSemana dia) < 7

--3.3
negar :: Bool -> Bool
negar opt = case opt of
    True -> False
    False -> True

valorDe :: Bool -> Int
valorDe False = -1
valorDe True = 1

implica :: Bool -> Bool -> Bool
implica a True = True
implica a _ = negar a

yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien False _ = False

oBien :: Bool -> Bool -> Bool
oBien True b = True
oBien False b = b 


--4
data Persona = P String Int deriving Show

nombre :: Persona -> String
nombre (P n _) = n

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombre (P n e) = P nombre e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2 then p1 else p2

--4.2

data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Pok TipoDePokemon Int
data Entrenador = Ent String Pokemon Pokemon

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMismoTipo Agua Agua = True
esDelMismoTipo Fuego Fuego = True
esDelMismoTipo Planta Planta = True
esDelMismoTipo _ _ = False

esDelTipo :: TipoDePokemon -> Pokemon -> Bool
esDelTipo Agua pok = esDelMismoTipo Agua (tipoDe pok)
esDelTipo Fuego pok = esDelMismoTipo Fuego (tipoDe pok)
esDelTipo Planta pok = esDelMismoTipo Planta (tipoDe pok)

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (Pok t _) = t

superaA :: Pokemon -> Pokemon -> Bool
superaA pok1 (Pok Agua _) = esDelTipo Planta pok1
superaA pok1 (Pok Fuego _) = esDelTipo Agua pok1
superaA pok1 (Pok Planta _) = esDelTipo Fuego pok1

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (Ent _ pok1 pok2) = if yTambien (esDelTipo tipo pok1)  (esDelTipo tipo pok2) then 2 
                                            else if oBien (esDelTipo tipo pok1) (esDelTipo tipo pok2) then 1
                                            else 0

listaDePokemonDe :: Entrenador -> [Pokemon]
listaDePokemonDe (Ent _ pok1 pok2) = [pok1, pok2]

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (ent1, ent2) = listaDePokemonDe ent1 ++ listaDePokemonDe ent2


-- 5
loMismo :: a -> a 
loMismo x = x

siempreSiete :: a -> Int
siempreSiete _ = 7

swap :: (a,b) -> (b, a)
swap (x , y) = (y, x)

-- 6

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (x:xs) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs

splitHead :: [a] -> (a, [a])
splitHead x = (elPrimero x, sinElPrimero x)