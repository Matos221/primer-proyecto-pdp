module Listas_Miyuki where
import PdePreludat
import Library

-- Tipos de funciones junto a sus tipos de datos
f1 :: (b -> a) -> b -> a
f1 a b = a b

f1 ::  (a -> Bool) -> a -> a -> Bool
f1 x y z = x y || x z

f1 :: (a -> b) -> a -> a -> ( b -> b -> c) -> c
f1 m x y z = z (m x) (m y)

f1 :: (c -> d) ->(b -> c) -> (a -> b) -> a -> d
f1 x y m = x . y. m

f1 :: (a -> a -> b) -> a -> a -> b
f1 f x y  = f y x

f1 :: (a , a) -> (a -> b -> c) -> (b , b) -> (c , c)
f1 (x, y) f m = (f x (fst m), f y (snd m))

f1 :: (a -> Bool) -> [a] -> a 
f1 f = head.filter f

f1 f x m = (f m).x
f1 :: (d -> (b -> c)) -> (a -> b) -> d -> (a -> c)
{- 
    notar que como hacemos (f m), aplicando parcialmente "m", necesitamos que el primer param sea una funcion que reciba "m" y retorne una funcion para componer con "x"
-}

-- Funcion const, devuelve el primer parametro
const :: a -> b -> a
const x y = x

-- Para esta funcion se utiliza los "==", necesitamos aclarar que nuestro tipo "b" (nuestro resultado) pertenece a la clase de comparadores "Eq" ( == 0 !==) 
f1 :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
f1 a1 a2 a3 = a1 a3 == a2 a3

-- Para esta funcion se usaran operadores numericos, "+, ==, < o >", por lo tanto si o si "a", debe ser de la clase de tipos de datos "Num". Capaces de cumplir esas funciones.
-- Si un parametro pertenece a una "clase de tipo de dato", este se ve obligado a cumplir con todas las operaciones definidas para su clase.
f1 :: (Eq b, Num a) => (a -> b) -> (a -> b) -> a -> Bool
f1 a1 a2 a3 = a1 (a3 + 1) == a2 (a3 + 1)

f1 :: a -> a -> (a -> Bool) -> a
f1 x y c | c x = y
         | otherwise = x

f1 :: (Num b, Ord b) => a -> (a -> b) -> Bool
f1 x m = ((>0).m) x

f1 :: (Ord b) => a -> a -> (a -> b) -> Bool
f1 x y f  =  f x > f y

f1 :: (Show a) => a -> String
f1 x = "¡" ++ show x ++ "!"

f1 :: (Num b) => (a -> b) -> (a -> Bool) -> [a] -> b 
f1 f g = sum.map f.filter g

fCondicional  :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b 
fCondicional f1 f2 f3 x
    | f1 x = f2 x
    | otherwise = f3 x
    where  transformarCond f1 f2 f3 l = map (fCondicional f1 f2 f3) l
    
f1 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f1 :: (c -> d) -> ((a -> (b -> c)) -> (a -> (b -> d)))
f1 = (.)(.)(.)