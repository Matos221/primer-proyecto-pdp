import GHC.Base
import PdePreludat (putStrLn, getLine)

saludar :: String -> String
saludar nombre  = "Hola " ++ nombre ++ "!"

main :: IO ()
main = do
    putStrLn "Ingrese su nombre"
    nombre <- getLine
    let saludo = saludar nombre
    putStrLn saludo 