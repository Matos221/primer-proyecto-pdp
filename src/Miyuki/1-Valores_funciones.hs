module Funciones_Miyuki where
import PdePreludat
import Library

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

perimetroTriangulo :: Int -> Int -> Int -> Int
perimetroTriangulo l1 l2 l3 = l1 +l2 +l3

{- Funcion llamando otras funciones -} 
dobleDelCuadrado :: Int -> Int
dobleDelCuadrado numero = doble (cuadrado numero) 

-- Aplicacion parcial y Ord Sup
dobleDelCuadrado :: Int -> Int
dobleDelCuadrado = doble.cuadrado 


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

-- esBisiesto 
esBisiesto :: Int -> Bool
esBisiesto x = ( (esMultiploDe x 4 && not(esMultiploDe x 10) )  || esMultiploDe x 400)

-- deCelsiusAFarenheit
deCelsiusAFarenheit  :: Float -> Float 
deCelsiusAFarenheit x = x * 1.8 + 32

-- deFarenheitACelsius
deFarenheitACelsius  :: Float -> Float
deFarenheitACelsius x = (x - 32) / 1.8 

-- Hace frio ??
haceFrioCelsius :: Float -> Bool
haceFrioCelsius x = x < 8

haceFrioFarenheit :: Float -> Bool
haceFrioFarenheit x = (haceFrioCelsius.deFarenheitACelsius) x 

-- Dispersion

-- Con guardas
maximoEntreTres :: Int ->Int ->Int ->Int
maximoEntreTres x y z
  | x > y && x > z = x
  | y > x && y > z = y
  | z > x && z > y = z
  
minimoEntreTres :: Int ->Int ->Int ->Int
minimoEntreTres x y z
  | x < y && x < z = x
  | y < x && y < z = y
  | z < x && z < y = z

-- Mas corto 
maximoEntreTres :: Int ->Int ->Int ->Int
maximoEntreTres x y z = max (max x y) z

minimoEntreTres :: Int ->Int ->Int ->Int
minimoEntreTres x y z = min (min x y) z

dispersion :: Int -> Int -> Int -> Int
dispersion x y z = (maximoEntreTres x y z) - (minimoEntreTres x y z)

-- Pasan los dias
diasParejos :: Int ->Int ->Int ->Bool
diasParejos x y z = (dispersion x y z ) < 30

diasLocos :: Int ->Int ->Int ->Bool
diasLocos x y z = (dispersion x y z ) > 100

diasNormales :: Int ->Int ->Int ->Bool
diasNormales x y z = not(diasParejos x y z) && not(diasLocos x y z)

-- Fabrica de muebles con pinos

metrosACentimetros :: Int -> Int
metrosACentimetros metros = metros * 100

-- Funciones para obtener los pesos de la base (que es hasta 3 metros) y el tronco (altura pasada los 3 metros)
alturaBase :: Int -> Int
alturaBase altura = min altura 3 -- (Si la altura pasa los 3 metros se queda con los primeros 3, los restantes se usaran en la siguiente funcion)

alturaTronco :: Int -> Int
alturaTronco altura = max (altura - 3 ) 0 -- (Si se paso los 3 metros de alto, tomo los restantes y comparo si son mas que 0 los uso, sino significa que no paso los 3)

-- Tomo las medidas del peso de la base y tronco del pino
pesoBasePino ::  Int -> Int 
pesoBasePino altura = (metrosACentimetros . alturaBase) altura * 3 -- (Calculo el peso de la base del pino, es decir, hasta los 3 metros)

pesoTroncoPino :: Int -> Int
pesoTroncoPino altura = (metrosACentimetros . alturaTronco) altura * 2 -- (Calculo el peso del tronco del pino, es decir, pasado los 3 metros)

pesoPino :: Int -> Int
pesoPino altura = pesoBasePino altura + pesoTroncoPino altura -- (Sumo los pesos de todo el pino)

esPesoUtil :: Int -> Bool
esPesoUtil x = x > 400 && x < 1000

sirvePino :: Int -> Bool
sirvePino x = (esPesoUtil . pesoPino) x