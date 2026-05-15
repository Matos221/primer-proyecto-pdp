module Listas_Miyuki where
import PdePreludat
import Library

sumarSegun  :: (Num b) =>  (a -> b) -> [a] -> b
sumarSegun op lista = foldl (+) 0 (map op lista)
-- mi semilla es el 0
{-
    sumarSegun id [1,3,5] => 9
-}

esMultiplo :: Number -> Number -> Bool
esMultiplo elem num = ((==0).rem elem) num 

esMultiploDeAlguno :: Number -> [Number] -> Bool
esMultiploDeAlguno num lista = foldl (flip ((||) . esMultiplo num )) False lista
-- esMultiploDeAlguno 15 [2,3,4] => True 
{-
Con flip (El adaptador):
    El flip garantiza que el foldl no le trate de pasar un Booleano a esMultiplo.

    El flip actúa como un adaptador de corriente. Recibe los cables en un orden y los entrega en otro:
    Entrada del foldl: (Acumulador: False, Elemento: 10)
    Transformación de flip: Los invierte -> (10, False)
    Ejecución: 
        Primero, el 10 entra en esMultiplo num. Resultado: True (porque 10 es múltiplo de 5, por ejemplo).
        Segundo, ese True se une mediante el || con el acumulador False.
        Resultado final del paso: True.
-}

-- Para mejoresNotas, creo una funcion auxiliar para tomar la mejor nota de cada lista. 
tomarMejorNota :: [Number] -> Number
tomarMejorNota notas = foldr max 0 notas

mejoresNotas :: [[Number]] -> [Number]
mejoresNotas variasNotas = map tomarMejorNota variasNotas

-- Necesario el "And y la semilla True", porque sino al comparar todas las notas si mi semilla es False, haria: nota1>=4 && nota2>=4 && False (semilla) => FALSE
-- Lo que buscamos es: nota1>=4 && nota2>=4 && True (semilla) => TRUE
aproboTodo :: Int -> Bool
aproboTodo nota = nota >= 4

aprobo :: [Int] -> Bool
aprobo notas = foldl (flip ((&&).aproboTodo)) True notas

-- Para esta funcion, primero era lista de funciones que podian cambiar o no el tipo de dato del param. Luego debia devolver una LISTA, por lo tanto el resultadon consecutivo debia integrarse en una lista con ":"
pam :: [(a -> b)] -> a -> [b]
pam funciones param = foldr (\func listFunc -> func param : listFunc) [] funciones
-- El lambda funciona de esta forma: funcion que recibe una funcion (de la lista pasada por param) y la ResultList, devuelve la funcion con el param aplicado. Luego lo mete dentro de la ResultList

-- Para este caso se aplica una funcion "lambda", la cual hace lo siguiente:
-- foldl1 toma como acumulador o primer_elemento, el primero de la lista, el lambda, retorna el acumulador, como a este no le realizamos cambios el siguiente seguira siendo el primer elemento.
cabeza :: [a] -> a
cabeza lista = foldl1 (\elem nextElem -> elem) lista

{-  En este caso se tuvo que derivar a una funcion auxiliar, la cual tome:
        - comparador 
        - elemento
        - acumulador
    El foldr toma el elemento y lo manda a la funcion de "agregarALista", con el comparador, si cumple, lo agrega a mi lista "acumulador" (vendria a ser mi resultado). Luego pasa al siguiente elemento y usa de nuevo el "acumulador"
-} 
agregarALista :: (a -> Bool) -> a -> [a] -> [a]
agregarALista comparador elemento acumulador 
  | comparador elemento = elemento : acumulador
  | otherwise = acumulador

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar comparador lista = foldr (agregarALista comparador) [] lista

-- Esta funcion similar a la anterior, segun un criterio toma el que es maximo de la lista. Compara el resultado del acumulador con el resultado del siguiente elemento. Se queda con el mayor, y sigue. 
mayorSegun :: (Ord b) => (a -> b) -> a -> a -> a -- Se necesito agregar el typeclass "Ord", dado que al aplicar nuestra "op - Operacion", obtenemos un nuevo tipo de dato y debemos aclarar a Haskell que este es comparable. 
mayorSegun op elem nextElem
  | (op elem) > (op nextElem) = elem 
  | otherwise = nextElem

maximoSegun :: (Ord b) => (a -> b) -> [a] -> a
maximoSegun op lista = foldl1 (mayorSegun op) lista 

-- Version 1
aplicarOp :: (a -> a -> a) -> (a , a) -> [a] -> [a]
aplicarOp op (x,y) acumulador = op x y : acumulador

aparearCon :: (a -> a -> a) -> [a] -> [a] -> [a]
aparearCon op lista1 lista2 = foldr (aplicarOp op) [] (zip lista1 lista2) -- zip toma dos listas y las pone en una lista de tuplas, donde cada tupla es un par de elem_a y elem_b, zip [a] [b] => [(a, b)]

-- Version 2
aparearCon2 :: (a -> a -> a) -> [a] -> [a] -> [a]
aparearCon2 op lista1 lista2 = foldr ( (:) . uncurry op ) [] (zip lista1 lista2) -- "uncurry :: (a -> b -> c) -> (a, b) -> c", toma una tupla y retorna el resultado de usar el operador entre a y b

addOpuesto :: Int -> [Int]
addOpuesto x = [x, negate x]

positivosYNegativos :: [Int] -> [Int]
positivosYNegativos lista = concatMap addOpuesto lista -- "concatMap :: (a -> [b]) -> [a] -> [b]" , toma un elemento de una lista le aplica un operacion y agrega el resultado en una neuva lista