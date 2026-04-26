module Guardas_Pattern_Matching_Miyuki where
import PdePreludat
import Library

minimoEntre :: Int -> Int -> Int
minimoEntre x1 x2
  | x1 < x2 = x1
  | otherwise = x2

cantidadDePochoclosParaMinutosDeCine :: Float -> Float
cantidadDePochoclosParaMinutosDeCine min
  | min < 40 = 2
  | min > 200 = 10
  | otherwise = (min/20) 


horasDuerme persona
  | estudiaIngenieria persona = 4 
  | programa persona = 6

esCero :: Float -> Bool -- Funcion simple la cual verifica el patron de "numero" == 0.
esCero 0  = True
esCero _ = False

estadoDeAnimo :: String ->  String
estadoDeAnimo "Viernes" = "¡Estoy enamorado!"
estadoDeAnimo _ =  "Meh :S"                           

puntosParaSetenta :: Float -> Float
puntosParaSetenta 1 = 5.5
puntosParaSetenta 10 = 0.5
puntosParaSetenta 11 = 0.5
puntosParaSetenta 12 = 0.5
puntosParaSetenta n = n

distanciaAlOrigen :: (Float,Float) -> Float
distanciaAlOrigen par = sqrt (  -- La funcion de Raiz solo funciona con datos del tipo Float
              ((snd par)^2) + ((fst par)^2)
              )

-- Pattern Matching con tuplas
poderSoldado :: (String , Int, Int) -> Int
poderSoldado (_ , f, d) = f*d -- Debe ser una tupla de 3 elementos

{- PRACTICAS -}

-- Funciones que copian las funciones de First y Second de Tuplas->(x,y) pero para Tuplas->(x,y,z)
fst3 :: (a,b,c) -> a
fst3 (e1, _, _) = e1 

snd3 :: (a,b,c) -> b
snd3 (_, e2, _) = e2 

trd3 :: (a,b,c) -> c
trd3 (_, _, e3) = e3 
-- Notese el uso de Pattern Matching para verificar que tenemos los elementos necesarios para mostrar

-- "aplicar", que recibe una tupla de 2 funciones, y un entero, y devuelve como resultado una tupla con el resultado de aplicar el elemento a cada una de la funciones.
aplicar :: ( (a -> a) , (a -> a) ) -> a -> (a,a)
aplicar (f1,f2) num = ( f1 num , f2 num)

{-  cuentaBizarra:
     - si el primer elemento es mayor al segundo devuelve la suma
     - si el segundo le lleva más de 10 al primero devuelve la resta 2do – 1ro
     - en otro caso, devuelve el producto.
-}
cuentaBizarra :: (Int , Int) -> Int
cuentaBizarra (num1 , num2)
  | num1 > num2 = num1 + num2
  | (num2-num1) > 10 = num2 - num1
  | otherwise = num1*num2

{-
  - Definir la función esNotaBochazo, recibe un número y dice si no llega a 4. Hacerlo sin usar guardas.
  - Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo.
  - Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 14 y además haberse sacado al menos 6 en cada parcial.
-}
esNotaBochazo :: Int -> Bool
esNotaBochazo nota = nota < 4

aprobo :: (Int , Int ) -> Bool
aprobo (nota1,nota2) = 
          not((esNotaBochazo nota1) || (esNotaBochazo nota2)) 
          -- Para este caso se penso, la funcion "esNotaBochazo" retorna false si aprobo, entonces caso bueno es False && False pero necesito True como resultado, niego todo y queda Not(False) || Not (False)

promociono :: (Int , Int) -> Bool
promociono (nota1,nota2) = 
         ((nota1+nota2) >= 14 ) && (nota1 >=6 && nota2 >= 6) 
  


notasFinales :: ( (Int,Int) , (Int,Int) ) -> (Int,Int)
notasFinales notas  = (
  max (fst (fst notas)) (fst (snd notas)),
  max (snd (fst notas)) (snd (snd notas)) 
  )
      
recuperoDeGusto :: ( (Int,Int) , (Int,Int) ) -> Bool
recuperoDeGusto alumno = 
            promociono (fst (fst alumno) , snd (fst alumno))     -- Le paso a promociono las primeras notas de parciales
            && 
            rindioRecu (snd alumno) -- Le paso a rindioRecu la tupla con los recus
            
rindioRecu :: (Int , Int) -> Bool
rindioRecu recus = ( fst recus /= -1) || (snd recus /= -1)

esMayorDeEdad :: (String , Int) -> Bool
esMayorDeEdad persona = ((>=21).snd) persona

-- Entender bien la logica de poder derivar cosas a otras funciones para alcanzar una funcion de mayor nivel 
calcular :: (Int,Int) -> (Int,Int)
calcular (n1,n2) = 
    (duplicarSiPar n1, sumarSiImpar n2)
    
duplicarSiPar :: Int -> Int
duplicarSiPar numero 
    | even numero = numero * 2
    | otherwise = numero
    
sumarSiImpar :: Int -> Int
sumarSiImpar numero 
    | odd numero = numero + 1
    | otherwise = numero