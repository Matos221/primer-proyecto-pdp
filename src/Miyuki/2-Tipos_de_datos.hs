import Library

{-  Repaso de composicion de funciones, con esto podemos acortar el codigo. 

    La idea de inferencia bajo la composición usando la función . no tiene misterios: unaFuncionNueva es una funcion que toma lo que tome otraFuncion, y devuelve lo que devuelva unaFuncion.

    unaFuncionNueva x = unaFuncion (otraFuncion numero) -- Sin utilizar la función composición (.)
    unaFuncionNueva = unaFuncion.otraFuncion -- Utilizando la función composición (.)
-}
largoEsPar :: String -> Bool -- La funcion largoEsPar recibe como primer parametro el tipo de dato que recibiria la funcion "length", y luego retornaria el tipo de dato que devolveria la funcion "even"
largoEsPar = even.length


edadDeAgus :: Int
edadDeAgus = 15

nombreDeAgus :: String
nombreDeAgus = "Agustín"

estaHartoAgus :: Bool
estaHartoAgus = False

inicialDeAgus :: Char
inicialDeAgus = 'a'

nombreCompleto :: String ->String ->String ->String
nombreCompleto nombre segundoNombre apellido = nombre ++" "++ segundoNombre ++" "++ apellido

-- Podemos definir la clase de tipo de dato que espera mi funcion. Lo que tenemos de la "=>" para izq, son reestricciones, y a derecha los tipos:
doble :: Num a => a -> a -- La clase "Num", incluye int y floats. 
doble numero = numero * 2

-- Hay funciones que aceptan un tipo de dato de alguna clase como lo es, "rem" solo con enteros ó "/" solo con flotantes.
rem :: Int -> Int -> Int
(/) :: Float -> Float -> Float

-- A esta funcion se puede definir que su clase es "Num" por usar los operadores numericos "+ y *"
funcionMisteriosa1 :: Num a => a -> a 
funcionMisteriosa1 x = x * x + x

-- La siguiente funcion recibe un parametro del tipo imprimible en pantalla, como "string" y luego un Int.
funcionMisteriosa2 ::  Show a => a -> Int
funcionMisteriosa2 = length.show

-- Funcion que recibe argumentos de clases != 
funcionLoca :: (Ord a, Show b) => a -> a -> b -> Bool -- Los argumentos "a" son "Ord" y para los "b" seran "Show"
funcionLoca x y z = x > y || show z == "hola"

-- Funcion con una misma clase, capaz de comparar con <= o >=
estaEntre :: Ord a => a ->a ->a -> Bool
estaEntre valor menor mayor = valor >= menor && valor <= mayor

-- Funcion con dos clases !=, una para usar "/= o ==" y otra para reUsar la funcion de arriba
sonIgualesOEstaEntre :: (Eq a, Ord b) => a ->a ->b -> b -> b -> Bool
sonIgualesOEstaEntre unValor otroValor valorEntre menor mayor = unValor == otroValor || estaEntre valorEntre menor mayor

-- Ultimo ejemplo, con las clases podemos definir como va a comportarse la funcion
ignoraElPrimero :: Show b => a -> b -> b
ignoraElPrimero primero segundo = segundo -- Al darle a "b" la clase "Show", permitiendo mostrarlo en consola, se puede ignorar en el resultado el primer argumento

sumarTres :: Num a => a -> a -> a -> a
sumarTres uno otro otroMas = uno + otro + otroMas

compararSi :: Eq a => Bool -> a -> a -> Bool
compararSi condicion uno otro = not condicion || uno == otro -- Nuestra funcion recibe un booleano al cual se niega y luego dos argumentos de la clase "Eq", permitiendo compararlos.


{- PRACTICAS -}

esParO :: Int -> Bool ->  Bool
esParO numero condicion = even numero || condicion

sumarDos :: Num a => a -> a -- Como solo es operadores numericos usamos la clase "Num"
sumarDos numero = numero + 2

multiplicar :: Num a => a -> a -> a 
multiplicar numero otroNumero = numero * otroNumero

-- fueraDeRango: tome tres valores y nos diga si el primero está fuera del rango que forman los otro dos. En otras palabras, fueraDeRango es verdadero si el primer parámetro es mas chico que el segundo o más grande que el tercero.
fueraDeRango :: Ord a => a -> a -> a -> Bool -- Tengo la clase "Ord", la cual es para la mayoria de tipos de datos, y permite usar las comparaciones "< > >= <=".
fueraDeRango numero1 numero2 numero3 = numero1 < numero2 || numero1 > numero3

-- largoDelShow
largoDelShow :: Show a => a -> Int -- Con la clase "Show" puedo indicar que todo lo que reciba se pueda mostrar en pantalla.
largoDelShow = length.show 

-- sumaEsPar
sumaEsPar :: Integral a => a -> a -> Bool -- Unicamente puedo usar la clase Integral o el tipo Int, debido a que "even", tiene reestriccion para argumentos no enteros.
sumaEsPar numero1 numero2 = even (numero1 + numero2)

-- Maximos y Minimos
maximoEntreTres :: Ord a => a -> a -> a -> a 
maximoEntreTres uno otro otroMas = 
  max (max uno otro) otroMas
  
minimoEntreTres :: Ord a => a -> a -> a -> a 
minimoEntreTres uno otro otroMas = 
  min (min uno otro) otroMas

-- ignorarSegundo (Mas funcion const)
soloElPrimero :: Show a => a -> b -> a -- la funcion "const", haria lo mismo, ya que devuelve el primer argumento. 
soloElPrimero primero segundo  = primero

-- noTanDistintos 
noTanDistintos :: (Eq a, Eq b) => a -> a -> b -> b-> Bool -- Los primeros dos argumentos de noTanDistintos tienen que ser del mismo tipo, y los últimos dos, también. ¡Pero no tienen por qué ser los cuatro iguales!
noTanDistintos valor1 valor2 valor3 valor4 = 
  valor1 == valor2 || valor3 == valor4