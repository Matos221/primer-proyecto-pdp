import PdePreludat
import Library

-- EJERCICIO 1: Dadas dos palabras, devolver la longitud de la palabra mas corta
minimoEntreDosPalabras :: String -> String -> Number
minimoEntreDosPalabras palabra1 palabra2 = min (length palabra1) (length palabra2)

-- EJERCICIO 2: Decir si la palabra inicia con A
-- Recibe un String = [...char] array de caracteres
palabraEmpiezaConA :: String -> Bool
palabraEmpiezaConA palabra = head palabra == 'A'

-- EJERCICIO 3: Definir la funcion composicion
composicion :: (b -> c) -> (a -> b) -> a -> c 
composicion f g x = f (g x)

-- EJERCICIO 4: Es numero Positivo
esNumeroPositivo :: Number -> Bool
esNumeroPositivo numero = numero > 0

-- EJERCICIO Plantacion de pinos:
{-
    En una plantacion de pinos, de cada arbol se conoce la altura en metros.
    El peso de un pino se puede calcular a partir de la altura asi:
     - 3 kg por cada centrimetro hasta 3 metros de altura. LA BASE
     - 2 kg por cada centimetro arriba de los 3 metros de altura. EL TRONCO
    (La base sera hasta los 3 metros donde el peso sera de 3kg por cada centimetro, mientras que la altura sera de los 3 metros para arriba)




    Los pinos se usa para la fabrica de muebles, a la que le sirben arboles de entre 400 , 1000 kilos, un pino fuera del rango NO sirve.
    - Definir la funcion pesoPino, recibe la altura de un pino en metros y devuelve su peso.
    - Definir la funcion pesoUtil, recibe un peso en kg y responde si el pino puede usarse.
    - Definir la funcion sirvePino, recibe la altura de un pino y responde si el pino de ese peso sirve.

-}

pesoPino :: Number -> Number
pesoPino altura = pesoBasePino altura + pesoTroncoPino altura

metrosACentimetros :: Number -> Number
metrosACentimetros metros = metros * 100

pesoBasePino ::  Number -> Number 
pesoBasePino altura = (metrosACentimetros . alturaBase) altura * 3

pesoTroncoPino :: Number -> Number
pesoTroncoPino altura = (metrosACentimetros . alturaTronco) altura * 3

alturaBase :: Number -> Number
alturaBase altura = min altura 3

alturaTronco :: Number -> Number
alturaTronco altura = max (altura - 3 ) 0

-- FALTA DEFINIR pesoUtil y sirvePino --