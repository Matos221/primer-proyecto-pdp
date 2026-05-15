module Listas_Miyuki where
import PdePreludat
import Library

{-  IMPORTANTE !!! => Si quiero generalizar mis funciones usando "a, b, c", etc es importante marcar su TYPECLASS, en caso de usar INT o STRING ya se tiene claro que funciones puedo aplicarles y cumpliran -}

juntarStrings :: String -> [String] -> String
juntarStrings sep cadenas = 
  foldl1 (\cadenaAcum nextElem -> cadenaAcum++sep++nextElem) cadenas

-- Para este caso, sin el "lambda", deberia crear una funcion aux que revise si, x_list1 esta en list2.
intersectar :: (Eq a) => [a] -> [a] -> [a]
intersectar list1 list2 = filter (\x -> elem x list2) list1

-- Importante para este caso y el anterior, aclarar que "a" es de la typeclass "Eq", de esa forma puedo usar "==" o "/="
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia list1 list2 = filter (\x -> ((not).elem x) list2 ) list1

hayAlgunMultiploDe :: Int -> [Int] -> Bool
hayAlgunMultiploDe num lista = any (\x -> ((==0).rem x) num) lista

totalKilosProductos :: [(String , Int)] -> Int
totalKilosProductos lista =(sum . foldl (\ac nextElem -> snd nextElem : ac) []) $ lista 

totalKilosProductosx2 :: [(String,Int)] -> Int
totalKilosProductosx2 lista =
  (sum . foldl (\ac nextElem -> (\(x,y) -> y) nextElem : ac) []) $ lista 

totalKilosProductosConMap :: [(String,Int)] -> Int
totalKilosProductosConMap lista = (sum . map (\(x,y) -> y)) $ lista