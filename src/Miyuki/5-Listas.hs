module Listas_Miyuki where
import PdePreludat
import Library

algunosTuits :: [a] -> [a]
algunosTuits lista = take 3 lista -- Funcion comun de lista para haskell a partir de un N y una lista devuelvo los "N´primeros" elementos de la lista.

textos :: [(String,String)] -> [String]
textos lista = map snd lista -- Al usar map la funcion debe ser un param no hay que componer

-- Definí una función recortar, que tome una lista de tuits y trunque a sus contenidos a dicha longitud. Explicitá su tipo. La función debe devolver una lista de tuits
recortar :: [(String,String)] -> [(String,String)]
recortar tuitsExceso = map recortarTuit tuitsExceso

recortarTuit :: (String,String) -> (String,String) -- Con esta funcion aux, puedo a cada elemento recortar su tuit
recortarTuit tuit = (fst tuit, ((take 15).snd) tuit) -- Solo modificamos el segundo elemento

tuitCorto :: (String,String) -> Bool
tuitCorto tuit = ((<10).length.snd) tuit

-- Normal
cantidadTuitsCortos :: [(String,String)] -> Int
cantidadTuitsCortos tuit = (length.(filter tuitCorto))  tuit 

-- Con el operador $ le pasamos a tuitCorto el parametro antes que al resto de funciones
cantidadTuitsCortos :: [(String,String)] -> Int
cantidadTuitsCortos tuit = (length.(filter tuitCorto)) $ tuit 

-- Con parametro implicito
cantidadTuitsCortos :: [(String,String)] -> Int
cantidadTuitsCortos = (length.(filter tuitCorto)) 

{- 
    Definí la función resumir, que dado un conjunto de tuits, los recorte, se quede con sus textos, y los junte todos en un sólo string separado por comas.
    Asumí que contás con las siguientes funciones:
        - csv: toma una lista de strings y los combina, separandolos por comas
        - textos: la función que definimos antes
        - recortar: la otra función que definimos antes


-}
resumir :: [(String,String)] -> String
resumir tuits = (csv.textos.recortar) tuits -- Estas fuciones YA USAN EL MAP

csv :: [String] -> String
csv listaTextos = intercalate "," listaTextos -- Intercalate es una funcion en haskell que toma un elemento y lo va intercalando con los elementos de la lista

{- PRACTICAS -}

-- Definí la función sumarSegun, que dada una función y una lista de elementos devuelve la suma de aplicar la función a cada uno de los elementos. 
sumarSegun :: (a -> Int) -> [a] ->  Int
sumarSegun func lista = (sum.map func) lista

alguno :: (a -> Bool) -> [a] -> Bool
alguno func lista = (not.null) (filter func lista)

-- Usando concat y reverse, definí la función esCapicua, que dada una lista de listas, me devuelve si la concatenación de las sublistas es una lista capicúa.
esCapicua :: Eq a => [[a]] -> Bool
esCapicua lista = (concatReverse lista) == (concat lista)

concatReverse :: [[a]] -> [a]
concatReverse = reverse.concat

esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno numero lista = any (esMultiploDe numero) lista -- Usamos any, funcion la cual verifica que en una lista al menos un elemento cumpla la condicion.

-- Llamadas

-- Version corta con Pattern matching
cuandoHabloMas :: ([Number],[Number]) -> String
cuandoHabloMas (normal,reducido)
    | sum normal > sum reducido = "Normal"
    | sum normal < sum reducido = "Reducido"
    | otherwise = "Normal"

-- Version con composicion 
cuandoHabloMas2 :: ([Number],[Number]) -> String
cuandoHabloMas2 hitorialLlamadas
    | (sum.fst) hitorialLlamadas > (sum.snd) hitorialLlamadas = "Normal"
    | (sum.fst) hitorialLlamadas < (sum.snd) hitorialLlamadas = "Reducido"
    | otherwise = "Normal"

-- Más llamadas
funcionesLlamadas :: ([Int] -> Int) -> ([Int],[Int]) -> String
funcionesLlamadas funcion hitorialLlamadas
    | (funcion.fst) hitorialLlamadas > (funcion) hitorialLlamadas = "normal"
    | (funcion.fst) hitorialLlamadas < (funcion.snd) hitorialLlamadas = "reducido"
    | otherwise = "normal"

cuandoHizoMasLlamadas :: ([Int],[Int]) -> String
cuandoHizoMasLlamadas hitorialLlamadas
    | (length.fst) hitorialLlamadas > (length.snd) hitorialLlamadas = "normal"
    | (length.fst) hitorialLlamadas < (length.snd) hitorialLlamadas = "reducido"
    | otherwise = "normal"


cuandoHizoLaLlamadaMasLarga :: ([Int],[Int]) -> String
cuandoHizoLaLlamadaMasLarga hitorialLlamadas
    | (maximum.fst) hitorialLlamadas > (maximum.snd) hitorialLlamadas = "normal"
    | (maximum.fst) hitorialLlamadas < (maximum.snd) hitorialLlamadas = "reducido"
    | otherwise = "normal"

cuandoHizoMasLlamadasBreves :: ([Int],[Int]) -> String
cuandoHizoMasLlamadasBreves hitorialLlamadas
    | cantLlamadasBreves.fst hitorialLlamadas > cantLlamadasBreves.snd hitorialLlamadas = "normal"
    | cantLlamadasBreves.fst hitorialLlamadas < cantLlamadasBreves.snd hitorialLlamadas = "reducido"
    | otherwise = "normal"

cantLlamadasBreves :: [Int] -> Int
cantLlamadasBreves llamadas = (length.(filter =<2)) llamadas


-- Promedios 
promedios :: [[Float]] -> [Float]
promedios listasNumeros = 
      map average listasNumeros 

-- promediosSinAplazos
promediosSinAplazos :: [[Float]] -> [Float]
promediosSinAplazos numeros  = 
        map (average.filter (>=4)) numeros

mejoresNotas :: [[Int]] -> [Int]
mejoresNotas listaNumeros = map maximum listaNumeros

aprobo :: [Int] -> Bool
aprobo notas = all (>=4) notas

-- Definí la función quienesAprobaron, que dada la información de un curso devuelve la información de los alumnos que aprobaron.
quienesAprobaron :: [[Int]] -> [[Int]]
quienesAprobaron notasAlumnos =  filter aprobo notasAlumnos

-- Definí la función hayAlgunNegativo, que dada una lista de números nos dice si hay algún negativo.
hayAlgunNegativo :: [Int] -> Bool
hayAlgunNegativo numeros = any (<0) numeros

-- Definí la función sumaPorFunciones, que dadas una lista de funciones y un número, devuelve la suma del resultado de aplicar las funciones al número. 
sumaPorFunciones :: [(Int->Int)] -> Int -> Int
sumaPorFunciones listaFunciones numero = (sum.aplicarFunciones listaFunciones ) numero

-- Escribí, usando composición, una función cuantosCumplen que dada una condición y una lista, diga cuantos elementos la cumplen.
cuantosCumplen :: ( a-> Bool) -> [a] -> Int
cuantosCumplen condicion lista = 
       ((length).(filter condicion)) lista

-- Escribí una función rechazar, que dada una condición y una lista, devuelva una lista con los elementos que no la cumplen. 
rechazar :: (a-> Bool) -> [a] -> [a]
rechazar condicion lista = 
       filter (not.condicion) lista

-- Definí la función contiene que dado un elemento y una lista, nos diga si la lista contiene al elemento.
contiene :: Eq a => a -> [a] -> Bool
contiene elemento lista =   (not.null.filter (==elemento)) lista

-- Escribí y explicitá el tipo de una función rotar, que tome una lista y ponga su cabeza (es decir, su primer elemento) al final.
rotar :: [a] -> [a]
rotar lista  = tail lista ++ [head lista]

-- Escribí y explicitá el tipo de una función iniciales, que tome un string formado por varias palabras, y devuelva otro formado por las iniciales.
iniciales :: String -> String
iniciales = map head .filter((>1).length).words 

-- Definí la función pam, que es... como el map pero al revés : le pasás una lista de funciones y un valor y te devuelve los resultados de aplicar cada una al valor.
pam :: [a -> b] -> a -> [b]
pam funciones valor = map ($ valor) funciones   