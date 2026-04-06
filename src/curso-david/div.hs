import Prelude
{-  Tipos de funciones en Haskell
        1) Prefijas: como la operacion "menos - ", debido a que se escribe antes del operando/argumento. Ejemplo: -(-2)
        2) Infijas: tienen mas de 2 argumentos y un ejemplo es la "suma + ". Ejemplo: 2 + 2
        3) Posfija: despues del operando como si fuera ejemplo 2^2 (elevado al cuadrado - potencias)
-}

division :: Int -> Int -> Int
{-
    division = nombre_funcion
    :: = nos indica que la funcion va a ser del tipo indicado despues
    "Int -> Int -> Int" = parte de un entero, recibe un entero y retorna un entero. El ultimo siempre es el tipo de dato a retornar.
-}
division x y = x `div` y -- para float, cambio div => /
{-
    Aqui definimos que hara la funcion division:
    1) x y =  son los parametros, notese no se necesitan parentesis en HASKELL, como indicamos en la declaracion recibe Int -> Int (parametros enteros)
    2) x `div` y =  es lo que retornara la funcion  
    3) div es la funcion division pero propia de haskell, puede escribirse de estas formas: { x `div` y } (mas claro) ó { div x y }. Solo para enteros, en caso de division con Float usar /. 
-}