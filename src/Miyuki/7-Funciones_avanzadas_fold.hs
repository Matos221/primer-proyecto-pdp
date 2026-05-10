module Listas_Miyuki where
import PdePreludat
import Library

sumarSegun  :: (Num b) =>  (a -> b) -> [a] -> b
sumarSegun op lista = foldl (+) 0 (map op lista)
-- mi semilla es el 0
{-
    sumarSegun id [1,3,5] => 9
-}

esMultiplo :: Int -> Int -> Bool
esMultiplo elem num = ((==0).rem elem) num 


esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno num lista = foldl (flip ((||) . esMultiplo num )) False lista
-- El flip garantiza que el foldl no le intente pasar un Booleano a esMultiplo.
{-
    esMultiploDeAlguno 15 [2,3,4] => True
-}

{-
Con flip (El adaptador):
    El flip actúa como un adaptador de corriente. Recibe los cables en un orden y los entrega en otro:
    Entrada del foldl: (Acumulador: False, Elemento: 10)
    Transformación de flip: Los invierte -> (10, False)
    Ejecución: 
        Primero, el 10 entra en esMultiplo num. Resultado: True (porque 10 es múltiplo de 5, por ejemplo).
        Segundo, ese True se une mediante el || con el acumulador False.
        Resultado final del paso: True.
-}