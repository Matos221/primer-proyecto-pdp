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