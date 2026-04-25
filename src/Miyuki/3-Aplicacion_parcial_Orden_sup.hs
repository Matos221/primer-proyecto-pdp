import PdePreludat
import Library

-- Aplicacion Parcial -> aplicar a una función con menos argumentos de los "normales", para obtener otra que reciba los faltantes.

dobleDelSiguiente :: Number -> Number
dobleDelSiguiente = (*2).siguiente
siguiente :: Number -> Number
siguiente = (+1)

-- Funciones de Orden superior aquellas que reciben como primer parametro una funcion.

saludar :: (String -> String) -> String -> String
saludar titulador nombre = "Hola " ++ titulador nombre

esMenorSegun :: Ord b => a -> a -> (a -> b) -> Bool -- Importante aclarar que b es del tipo "Ord", capaz de usar las operaciones de comparaciones "< > <= >="
esMenorSegun e1 e2 f = (f e1) < (f e2)

saludoDoble :: (String -> String) -> String -> String -> String
saludoDoble titulador uno otro = "Hola " ++ titulador uno ++ " y " ++ titulador otro  

{-
    Tipo: (Int -> Bool -> a) -> a
    Análisis: 
    - * f recibe un número (digamos Int) y un Bool.
    - El resultado final de funcionMisteriosa1 es el mismo resultado que devuelva f.
    
    Nota: Dependiendo del contexto, el 2 podría ser de tipo Num n => n, pero para simplificar solemos pensar en Int.
    Esta función toma un argumento f y lo aplica a dos valores: el número 2 y el booleano True. Por lo tanto, f debe ser una función que acepte al menos dos parámetros.
-}
funcionMisteriosa1 :: (Float -> Bool -> a) -> a 
funcionMisteriosa1 f = f 2 True

{-
    Tipo: (Int -> a) -> [b] -> a
    Análisis:
        - length toma una lista [b] y devuelve un Int.
        - g debe ser capaz de recibir ese Int que sale de length.
        - Por lo tanto, g tiene el tipo Int -> a.
        - El resultado es una nueva función que espera una lista y devuelve lo que sea que g produzca.
-}
funcionMisteriosa2 :: (Int -> a) -> [b] -> a 
funcionMisteriosa2 g = g . length

{-
    Tipo: (a -> Bool -> b) -> (c -> a) -> c -> b
    Análisis paso a paso:
        - g x: Si x es de tipo c, entonces g debe ser c -> a. El resultado de esto es algo de tipo a.
        - h (...) True: La función h recibe el resultado de g x (que es a) y luego un True (que es Bool).
        - Entonces, h debe ser una función que acepte a y Bool.
        - El resultado final b es lo que devuelve h.
-}
funcionMisteriosa3 :: (a -> Bool -> b) -> (c -> a) -> c -> b -- 
funcionMisteriosa3 h g x = h (g x) True 

esNoVacio :: String -> Bool
esNoVacio = not.(==0).length

sumaDeDoblesSegun ::  a -> a -> (a -> Int) -> Int
sumaDeDoblesSegun val1 val2 f = ((*2).f) val1 + ((*2).f) val2 -- Importante que al plantear composicion debo encerrarla en parentesis para mejor comprension, no debo incluir en el parentensis al parametro. Este DEBE IR AFUERA.
-- (fun1.fun2) val BIEN
-- (fun1.fun2 val) MAL


{- PRACTICAS -}

esMultiploDe ::  Int -> Int -> Bool
esMultiploDe num = (==0).rem num -- planteo una funcion "rem" nueva, la cual es "rem num" la cual ya tiene integrada un argumento, esperando al siguiente. De esa forma puedo componer con (==0).

esBisiesto :: Int -> Bool
esBisiesto anio = esMultiploDe anio 400 || 
                  (esMultiploDe anio 4 && 
                  (not.esMultiploDe anio )100)

dobleDelLargo :: String -> Int
dobleDelLargo = ((*2).length)

sumarNumeroAlTriple :: Int -> Int -> Int 
sumarNumeroAlTriple = (+).(*3) -- Primero quiero que multiplique al primer parametro por 3 y luego los sume al segundo con el triple del primero

sumaEsPar :: Int -> Int -> Bool
sumaEsPar num = (even).(+num)

algunoCumple :: (a -> Bool) ->a ->a ->a -> Bool
algunoCumple comparador a1 a2 a3 = comparador a1 || comparador a2 || comparador a3

-- Se aplica el tipo de clase "Ord", dado que "Max" necesita solo comparar con menor o mayor los valores. Ahora la funcion puede aplicarse con cualquier funcion como parametro y tipo de dato perteneciente a la clase "Ord"
mejor :: Ord a => (a -> a) -> (a -> a) -> a -> a 
mejor f1 f2 x = max (f1 x) (f2 x)

-- Aplicamos ahora tuplas
esUnClasico :: (String,Int) -> Bool
esUnClasico = (<1959).snd

-- Otra forma sin param implicito
esUnClasico :: (String,Int) -> Bool
esUnClasico pelicula= ((<1959).snd) pelicula

-- Interesante ver como puedo crear una tupla a partir de solo poner el resultado de las funciones en (,)
aplicarPar :: (a -> b) -> a -> a -> (b,b)
aplicarPar func num1 num2 = (func num1 , func num2)   

tuplaDeFunciones :: ( a -> b) -> (a -> c) -> a -> (b,c)
tuplaDeFunciones f1 f2 valor = (f1 valor, f2 valor)

-- Esta funcion se llama "flip"
darVuelta  :: (b -> a -> c) -> a -> b -> c
darVuelta func campo1 campo2 = func campo2 campo1

-- Definición de flip
flip :: (a -> b -> c) -> b -> a -> c

-- Claro ejemplo que la composicion no es mas que funciones de Ord.Sup por recibir funciones como parametros
componer :: (b -> c) -> (a -> b) -> a -> c -- Ver que si o si para componer el resultado de la primera funcion con el parametro no es mas que el argumento de la segunda funcion
componer f2 f1 x =  f2 (f1 x)
-- Para haskell la logica es  componer :: f -> g -> x -> y => y = f[g(x)]
