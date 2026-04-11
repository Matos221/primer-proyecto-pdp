import PdePreludat
import Prelude

{- VALORES -}

marioTieneGato :: Bool
peliculaFavoritaAna :: String

marioTieneGato = False
peliculaFavoritaAna = "Gladiador"

{- FUNCIONES (Las funciones en haskell tambien son un TIPO DE DATO, tal como el int o float) -}

anterior :: Int -> Int
anterior numero = numero - 1

mostrarDias :: Integer -> Integer
mostrarDias cantidadDias = cantidadDias

doble :: Int -> Int
doble numero = numero * 2

cuadrado :: Int -> Int
cuadrado numero = numero * numero

esNegativo :: Int -> Bool
esNegativo numero = numero < 0

areaRectangulo :: Int -> Int -> Int
areaRectangulo lado altura = lado * altura

{- Funcion llamando otras funciones -} 
dobleDelCuadrado :: Int -> Int
dobleDelCuadrado numero = doble (cuadrado numero) 

{- Composicion de funciones -}
tripleDelAnterior :: Int -> Int 
tripleDelAnterior = triple . anterior -- Como en matematicas la composicion de funciones, primero se hace la funcion de la derecha con el parametro ingresado, luego la de la izquierda.
-- En este caso no hace falta poner la variable "numero", o el parametro pasado, Haskell ya sabe que si en la definicion le pasamos un Int este recibira un int

masDos :: Int -> Int
masDos = siguiente.siguiente
-- Componemos una funcion consigo misma

{- Operadores son funciones tambien (los llamamos de forma prefija) -}
doble :: Int -> Int
doble numero = (*)2 numero

{- Funciones y Strings -}
cuantoMidenJuntos :: String ->  String -> Int 
cuantoMidenJuntos palabra1 palabra2 = length(palabra1 ++ palabra2)

{- PRACTICAS -}

mitad :: Float -> Float 
mitad numero = numero / 2

inversa :: Float -> Float
inversa numero = 1 / numero

esNumeroPositivo :: Float -> Bool
esNumeroPositivo numero = numero > 0

comienzaConA :: String -> Bool
comienzaConA palabra = head(palabra) == 'a'

esMultiploDeTres :: Int -> Bool
esMultiploDeTres numero = esMultiploDe numero 3

esMultiploDe :: Int -> Int -> Bool
esMultiploDe num2 num1 = (rem num2 num1) == 0

esMasLargoQue :: Int -> String -> Bool
esMasLargoQue num palabra = num < length(palabra)

-- Mover parentesis y menos parentesis
esParElMayor numero otroNumero = esPar (max numero otroNumero)
esSaludo palabra = (palabra == "hola" || palabra == "chau")
laInicialEstaIncluida unaPalabra otraPalabra = elem (head unaPalabra) otraPalabra