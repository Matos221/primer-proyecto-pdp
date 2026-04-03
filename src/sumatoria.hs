import GHC.Base
import PdePreludat

main :: IO () -- Con esta linea indicamos que el "main" va realizar una operación del tipo "IO" (Input y Output).
main = print(sum[1..10]) -- El "main" retornara con la func. print() para imprimir en pantalla, la sumatoria sum[..] de los numeros indicados.