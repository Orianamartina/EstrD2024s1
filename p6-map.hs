-- 1. valuesM :: Eq k => Map k v -> [Maybe v]
-- Propósito: obtiene los valores asociados a cada clave del map.
-- 2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- Propósito: indica si en el map se encuentran todas las claves dadas.
-- 3. listToMap :: Eq k => [(k, v)] -> Map k v
-- Propósito: convierte una lista de pares clave valor en un map.
-- 4. mapToList :: Eq k => Map k v -> [(k, v)]
-- Propósito: convierte un map en una lista de pares clave valor.
-- 5. agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
-- la misma clave.

import Map

values :: [k] -> Map k v -> [v] 
values [] _ = []
values (x:xs) map = lookupM x map : values xs


valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = values (keys map) map
