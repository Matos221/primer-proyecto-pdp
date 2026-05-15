module Listas_Miyuki where
import PdePreludat
import Library

-- Primero modelemos unas tuplas:
autoPepe :: (String, Int, Int)
autoPepe = ("ABW100", 0 , 55)

autoMara :: (String, Int, Int)
autoMara = ("GIR982", 10  , 65)

patente :: (String, Int, Int) -> String
patente (x,y,z) = x

nivelNafta :: (String, Int, Int) -> Int
nivelNafta (x,y,z) = y

tamanioTanque :: (String, Int, Int) -> Int
tamanioTanque (x,y,z) = z

cargarTanque :: Int -> (String,Int,Int) ->(String,Int,Int)
cargarTanque carga (pat,nafta,capac) = (pat, ((min capac).(+carga) $ nafta),capac)  

-- Sinónimos de tipos:
type Auto = (String, Int, Int)

cargarTanque :: Int -> Auto -> Auto
cargarTanque nivel auto = .....

vaciarTanque :: Auto -> Auto
vaciarTanque (pat,nafta,capac) = (pat,0,capac)

estaVacio :: Auto -> Bool
estaVacio dataAuto = ((==0).nivelNafta) $ dataAuto

estaLleno :: Auto -> Bool
estaLleno dataAuto = tamanioTanque dataAuto == nivelNafta dataAuto

-- Datas
data Alumno = Alumno String Float deriving (Show, Eq)

data Auto = Auto String Int Int deriving (Show, Eq)
{-
    El constructor, además de servir para construir un auto, ¡también sirve para desarmarlo! Eso significa que sirve para trabajar con pattern matching.
    Gracias al agregado opcional de deriving (Show,Eq) es que adquiere ciertas propiedades convenientes que son poder mostrarse y ser comparado con otro auto. 
    Tené en cuenta que esto sólo es posible cuando todos sus componentes pueden también mostrarse y compararse.
-}
autoPepe = Auto "ABW100" 0 55
autoMara = Auto "GIR982" 10 65

cargarTanqueConData :: Int -> Auto -> Auto
cargarTanqueConData carga (Auto pat nafta capac) = Auto pat ((min capac).(+carga) $ nafta) capac  
--  Como en este caso "Auto", no tiene definido nombre para sus elementos, de querer descomponerlo, debmos aclarar el data 

estaVacioConData :: Auto -> Bool
estaVacioConData (Auto pat nafta capac) = ((==0).nivelNafta) $ (Auto pat nafta capac)

estaLlenoConData :: Auto -> Bool
estaLlenoConData (Auto pat nafta capac) = tamanioTanque (Auto pat nafta capac) == nivelNafta (Auto pat nafta capac)


cuantoLePuedeDar :: Auto -> Auto -> Int
cuantoLePuedeDar dador receptor = min (nivelNafta dador) (tamanioTanque receptor - nivelNafta receptor)

transferir :: Auto -> Auto -> (Auto, Auto)
transferir dador receptor = (cargarTanque (-(cuantoLePuedeDar dador receptor))dador , cargarTanque (cuantoLePuedeDar dador receptor) receptor) 

-- Primer Data
data Auto = Auto {
    patente :: String,
    nivelNafta :: Int,
    tamanioTanque :: Int
} deriving (Show,Eq)

autoPepe = Auto "ABW100" 0 55
autoMara = Auto {patente = "GIR982", nivelNafta = 10 , tamanioTanque = 65 } 

-- Defino tipos para funciones
type ServicioAutomotor = (Auto -> Auto)

vaciarTanque :: ServicioAutomotor

enchularPatente :: String -> ServicioAutomotor 
enchularPatente letras auto = auto {patente = take 3 letras ++ drop (length (patente auto)- 3) (patente auto)} 
   
hacerService :: Auto -> [ServicioAutomotor] -> Auto
hacerService auto servicios = 
    foldl (\auto servicio -> servicio auto) auto servicios