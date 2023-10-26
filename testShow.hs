import Oraculo
import qualified Data.Map as Map

main::IO()
main = do
    putStrLn "\ntest 1"
    let test1 = Prediccion "Prediccion directa"
    print test1
    print $ show test1

    putStrLn "\ntest 2"
    let test2 = Pregunta "Estas bien?" (Map.fromList [("No", Pregunta "Estas enferma?" (Map.fromList [("Si", Prediccion "Comiste mal"), ("No", Prediccion "Comiste bien")]))])
    print test2
    print $ show test2

    putStrLn "\ntest 3"
    let test3 = Pregunta "a" (Map.fromList [("b", Pregunta "c" (Map.fromList [("d", Prediccion "e")])),
                          ("f", Pregunta "g" (Map.fromList [("h", Prediccion "i")]))])
    print test3
    print $ show test3

    putStrLn "\ntest 4"
    let test4 = Pregunta "Es lenguaje?" (Map.fromList [("Si", Prediccion "Haskell"), ("No", Prediccion "Cancion")])
    print test4
    print $ show test4