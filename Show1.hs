module Show1 where
    import qualified Data.Map as Map
    import Control.Monad ( forM )
    import Data.Maybe ( fromJust )

    identation:: Int -> String 
    identation n = "\n" ++ concat (replicate n "\t") ++ "- "

    getKeys:: Opciones -> [String]
    getKeys = map fst . Map.toList

    showData :: Oraculo -> Int -> String
    showData (Prediccion str) _ = str
    showData (Pregunta str op) n = str ++ concatMap (\a -> do
            identation n ++ a ++ ": "
            ++ showData (fromJust $ Map.lookup a op) (n + 1)
        ) (getKeys op)
        
    data Oraculo = Prediccion String | Pregunta String Opciones
    instance Show Oraculo where
        show :: Oraculo -> String
        show (Prediccion str) = showData (Prediccion str) 1
        show (Pregunta str op) = showData (Pregunta str op) 1

    type Opciones = Map.Map String Oraculo