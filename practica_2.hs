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

edad :: Persona -> Int
edad (P _ e) = e

esMayorA :: Persona -> Int -> Bool
esMayorA persona x = (edad persona) > x

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA e (p:ps) = if esMayorA p e then p : mayoresA e ps else mayoresA e ps

sumaEdades :: [Persona] -> Int
sumaEdades [] = 0
sumaEdades (p:ps) = edad p  + sumaEdades ps

promedioEdad :: [Persona] -> Int
-- Precondición: la lista al menos posee una persona.
promedioEdad personas = div (sumaEdades personas) (longitud personas)

elMasViejo :: [Persona] -> Persona
-- Precondición: la lista al menos posee una persona.
elMasViejo (x:xs) = if longitud (mayoresA (edad x) xs) > 0 then elMasViejo xs else x

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t _) = t

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMismoTipo Agua Agua = True
esDelMismoTipo Fuego Fuego = True
esDelMismoTipo Planta Planta = True
esDelMismoTipo _ _ = False

esDelTipo :: TipoDePokemon -> Pokemon -> Bool
esDelTipo Agua pok = esDelMismoTipo Agua (tipoDe pok)
esDelTipo Fuego pok = esDelMismoTipo Fuego (tipoDe pok)
esDelTipo Planta pok = esDelMismoTipo Planta (tipoDe pok)

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ pokemones) = longitud pokemones

cantPokemonEnListaDe :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonEnListaDe _ [] = 0
cantPokemonEnListaDe tipo (x:xs) = if esDelTipo tipo x then 1 + cantPokemonEnListaDe tipo xs else cantPokemonEnListaDe tipo xs

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe _ (ConsEntrenador _ []) = 0
cantPokemonDe tipo (ConsEntrenador _ listaDePokemon) = cantPokemonEnListaDe tipo listaDePokemon

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (ConsEntrenador _ listaDePokemon) = listaDePokemon


leGanaA :: TipoDePokemon -> TipoDePokemon -> Bool
leGanaA Fuego Planta = True
leGanaA Agua Fuego = True
leGanaA Planta Agua = True
leGanaA _ _ = False

leGanaATodos :: TipoDePokemon -> [Pokemon] -> Bool
leGanaATodos _ [] = True
leGanaATodos tipo (x:xs) = leGanaA tipo (tipoDe x) && leGanaATodos tipo xs

cuantosDeTipo_LeGananATodos :: TipoDePokemon ->[Pokemon] -> [Pokemon] -> Int
cuantosDeTipo_LeGananATodos tipo [] lista2 = 0
cuantosDeTipo_LeGananATodos tipo (x:xs) lista2  =  if esDelTipo tipo x then boolAInt (leGanaATodos (tipoDe x) lista2) + cuantosDeTipo_LeGananATodos tipo xs lista2 else 0 + cuantosDeTipo_LeGananATodos tipo xs lista2

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ _ (ConsEntrenador _ []) _ = 0
cuantosDeTipo_De_LeGananATodosLosDe_ tipo ent1 ent2 = cuantosDeTipo_LeGananATodos tipo (pokemonesDe ent1) (pokemonesDe ent2)

hayDelTipo :: TipoDePokemon -> [Pokemon] -> Bool
hayDelTipo _ [] = False
hayDelTipo tipo (x:xs) = esDelTipo tipo x || hayDelTipo tipo xs

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ listaDePokemon) = hayDelTipo Fuego listaDePokemon && hayDelTipo Agua listaDePokemon && hayDelTipo Planta listaDePokemon

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String  deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

proyecto :: Rol -> Proyecto
proyecto (Developer _ pry) = pry
proyecto (Management _ pry) = pry

nombreDeProyecto :: Proyecto -> String
nombreDeProyecto (ConsProyecto nombre) = nombre

proyectoEstaEnRoles :: Proyecto -> [Rol] -> Bool
proyectoEstaEnRoles _ [] = False
proyectoEstaEnRoles proy (x:xs) = nombreDeProyecto (proyecto x) == nombreDeProyecto proy || proyectoEstaEnRoles proy xs

proyectosDe :: [Rol] -> [Proyecto]
proyectosDe [] = []
proyectosDe (x:xs) = if proyectoEstaEnRoles (proyecto x) xs then proyectosDe xs else proyecto x : proyectosDe xs

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa listaDeRoles) = proyectosDe listaDeRoles

tienenElMismoNombre :: Proyecto -> Proyecto -> Bool
tienenElMismoNombre pro1 pro2 = nombreDeProyecto pro1 == nombreDeProyecto pro2

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ proyecto) = proyecto
proyectoDeRol (Management _ proyecto) = proyecto

tieneProyecto :: Rol -> Proyecto -> Bool
tieneProyecto rol proyecto = tienenElMismoNombre (proyectoDeRol rol) proyecto
tieneProyecto rol proyecto = tienenElMismoNombre (proyectoDeRol rol) proyecto

esSenior :: Rol -> Bool
esSenior (Developer Senior _) = True
esSenior (Management Senior _) = True
esSenior _ = False

tienenProyectoYEsSenior :: [Rol] -> Proyecto -> Int
tienenProyectoYEsSenior [] _ = 0
tienenProyectoYEsSenior (x:xs) proyecto = boolAInt (tieneProyecto x proyecto && esSenior x) + tienenProyectoYEsSenior xs proyecto

rolesDe :: Empresa -> [Rol]
rolesDe (ConsEmpresa roles) = roles

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior empresa [] = 0
losDevSenior empresa (x:xs) = tienenProyectoYEsSenior (rolesDe empresa) x + losDevSenior empresa xs

tienenProyecto :: [Rol] -> Proyecto -> Int
tienenProyecto [] _ = 0
tienenProyecto (x:xs) proyecto = boolAInt (tieneProyecto x proyecto) + tienenProyecto xs proyecto

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn (x:xs) empresa = tienenProyecto (rolesDe empresa) x + cantQueTrabajanEn xs empresa

cantPorProyecto :: [Proyecto] -> Empresa -> [Int]
cantPorProyecto [] _ = []
cantPorProyecto (x:xs) empresa = cantQueTrabajanEn [x] empresa : cantPorProyecto xs empresa

zipL :: [a] -> [b] -> [(a, b)]
zipL [] _ = []
zipL _ [] = []
zipL (x:xs) (y:ys) = (x, y) : zip xs ys

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa []) = []
asignadosPorProyecto empresa = zipL (proyectos empresa) (cantPorProyecto (proyectos empresa) empresa) 

