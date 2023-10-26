import Oraculo
import Data.Maybe

main :: IO()
main = do
    let texto = crearOraculo "texto"
    let html = crearOraculo "html"
    let jv = crearOraculo "java"
    let hs = crearOraculo "haskell"
    let lng = ramificar ["imperativo", "funcional"] [jv, hs] "Que paradigma?"
    let islng = ramificar ["si", "no"] [lng, html] "Es un lenguaje?"
    let prog = ramificar ["si", "no"] [islng, texto] "Es algun tipo de codigo?"
    print $ obtenerEstadisticas prog