import Show1
import qualified Data.Map as Map

main::IO()
main = do

    putStrLn $ show (Prediccion "hola bebe")

    let oraculoFast = Pregunta "hola?" (Map.fromList [("No", Pregunta "Ayer?" (Map.fromList [("Si", Prediccion "uwu"), ("No", Prediccion "tomate")]))])
    putStrLn $ show oraculoFast
  
    let otro = Pregunta "a" (Map.fromList [("b", Pregunta "c" (Map.fromList [("d", Prediccion "e")])), 
                          ("f", Pregunta "g" (Map.fromList [("h", Prediccion "i")]))])
    putStrLn $ show otro

    let otro1 = Pregunta "hola?" (Map.fromList [("No", Pregunta "Ayer?" (Map.fromList [("Si", Prediccion "Paracetamol"), ("No", Prediccion "tomate")]))])
    putStrLn $ show otro1

    let otro2 = Pregunta "hola?" (Map.fromList [("So", Prediccion "uwu"), ("No", Prediccion "tomate")])
    putStrLn $ show otro2

    print $ Map.keys (Map.fromList [("Si", Prediccion "uwu"), ("No", Prediccion "tomate")])