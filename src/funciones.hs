{- 
   - Module funcion_1 where => podemos definir un archivo.hs como "module <nombre_funcion> where", donde detallamos que esto sera una funcion la que luego podemos recurrir.
    como lo es el "Prelude" siendo este el module de fuciones basicas para haskell.
    
    - Con el comando ":t div" podemos ver que funciones estan definidas en el Prelude.   

-}
import Prelude
-- Podemos desde consola gracias al Prelude, usar las operaciones agregandolas por consola, como "2+2", "5-6" o "12 / 5". Se necesita estar dentro de la terminal para "ghci"

{-
    Para probar nuestro programa .hs, en consola pondremos => 
        1) :r
        2) :ls src/<nombre_archivo>.hs
        3) <nombre_funcion> ... (parametros)
-}

-- Definimos la funcion que recibe (int, int) retorna int
g :: Int -> Int
g y =  y * 2

f :: Int -> Int -> Int
f g x = g + x   


-- f x (g y) =>  Esto NO da error, ejecuta f con parametros (x, g(y))
-- (f x g) y =>  Esto SI da error, ejecuta f con parametros (x, g()), da error de tipo