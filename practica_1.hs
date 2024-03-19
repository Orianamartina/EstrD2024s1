
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
vieneDespues Lunes Domingo = True
vieneDespues dia1 dia2 = (numeroDeDiaDeSemana dia1) == (numeroDeDiaDeSemana dia2)+1

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio dia = (numeroDeDiaDeSemana dia) > 1 && (numeroDeDiaDeSemana dia) < 7

--3.3
negar :: Bool -> Bool
negar opt = case opt of
    True -> False
    False -> True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True
oBien _ _ = False

--4
data Persona = P String Int 

nombre :: Persona -> String
nombre (P n _) = n

edad :: Persona -> Int
nombre (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) -> P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombre (P n e) = P nombre edad

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2 then p1 else p2

--4.2

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = Pok TipoDePokemon Int

data Entrenador = Ent String Pokemon Pokemon

tipoSuperaA :: TipoDePokemon -> TipoDePokemon
tipoSuperaA Agua = Planta
tipoSuperaA Fuego = Agua
tipoSuperaA Planta = Fuego

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMismoTipo Agua Agua = True
esDelMismoTipo Fuego Fuego = True
esDelMismoTipo Planta Planta = True
esDelMismoTipo _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (Pok t _) (Pok t2 _) = esDelMismoTipo (tipoSuperaA t t2) t 

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (Ent _ (Pok t1 _) (Pok t2 _)) = 
