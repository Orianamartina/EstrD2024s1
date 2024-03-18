
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

--4

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