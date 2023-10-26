module Oraculo (
    Oraculo (..),
    Opciones,
    crearOraculo,
    ramificar,
    prediccion,
    pregunta,
    opciones,
    respuesta,
    obtenerCadena
) where

-----------------------------  IMPORTACION DE MODULOS -----------------------------
    import qualified Data.Map as Map
    import Control.Monad (forM)
    import Data.Maybe (fromJust)
    import Data.List.Split (splitOn)
    import Data.List (isInfixOf)

--------------------------------  TIPOS DE DATOS  ---------------------------------

    {-  Tipo de Dato Oraculo que esta particionado en dos versiones
        Prediccion de tipo string y Pregunta de tipo String mas 
        opciones. -}
    data Oraculo = Prediccion String | Pregunta String Opciones

    {-  El alias Opciones permite simular mediante un mapa de llaves
        de tipo String a Oraculos la ramificacion del conocimiento de
        Haskinator. -}
    type Opciones = Map.Map String Oraculo

----------------------------------  INSTANCIAS  -----------------------------------

    -- Redefinicion de instancia Show para el tipo de dato Oraculo.
    instance Show Oraculo where
        show :: Oraculo -> String
        show (Prediccion str) = str
        show (Pregunta str op) = showData (Pregunta str op) 1

    {-  Funcion que permite realizar identacion mediante tabulaciones
        de cantidad n dado por argumento. -}
    identation:: Int -> String
    identation n = "\n" ++ concat (replicate n "\t") ++ "- "

    -- Funcion que dado un mapa de opciones extraemos para cada elemento la llave.
    getKeys:: Opciones -> [String]
    getKeys = map fst . Map.toList

    -- Funcion que permite mostrar el arbol de conocimiento de Haskinator.
    showData :: Oraculo -> Int -> String
    showData (Prediccion str) _ = str
    showData (Pregunta str op) n = str ++ concatMap (\a -> do
            identation n ++ a ++ ": "
            ++ showData (fromJust $ Map.lookup a op) (n + 1)
        ) (getKeys op)

    -----------------------------------------------------------------------------------
    -- Redefinicion de instancia Read para el tipo de dato Oraculo.
    instance Read Oraculo where
      readsPrec :: Int -> String -> [(Oraculo, String)]
      readsPrec _ input = [(readData [input] 1, "")]

    {-  Funcion que permite leer para el formato definido de Haskinator 
        a la estructura principal Oraculo que almacenara el "conocimiento".-}
    readData :: [String] -> Int -> Oraculo
    readData input n = do
        if length input == 1 && not ("\t" `isInfixOf` concat input) then do

            if ":" `isInfixOf` concat input then
              Prediccion $ splitOn ": " (head input) !! 1
              else
                Prediccion $ head input
            else do
                let splitedAux = concatMap (splitOn (identation n)) input
                let elements = tail splitedAux
                let list1 = map (takeWhile (/= ':')) elements
                let list2 = map (drop 2 . dropWhile (/= ':')) elements
                let list3 = map (\x -> readData [x] (n+1)) list2
                let list4 = zip list1 list3
                Pregunta (head splitedAux) $ Map.fromList list4

-----------------------------------------------------------------------------------
-----------------------------  FUNCIONES DE ACCESO  -------------------------------

    prediccion :: Oraculo -> String
    prediccion (Prediccion pred) = pred
    prediccion (Pregunta _ _) = error "No se puede obtener algo de tipo 'Prediccion' de algo tipo 'Pregunta'"

    pregunta :: Oraculo -> String
    pregunta (Pregunta preg _) = preg
    pregunta (Prediccion _) =  error "No se puede obtener algo de tipo 'Pregunta' de algo tipo 'Prediccion'"

    opciones :: Oraculo -> Opciones
    opciones (Pregunta _ op) = op
    opciones (Prediccion _ ) = error "No se pueden obterner opciones a partir de algo tipo 'Prediccion'"

    respuesta :: Oraculo -> String -> Oraculo
    respuesta (Pregunta _ option) answer = fromJust $ Map.lookup answer option
    respuesta (Prediccion _) _ = error "No se pueden obterner respuestas a partir de algo tipo 'Prediccion'"

-----------------------------------------------------------------------------------
----------------------------  FUNCIONES DE INSPECCION  ----------------------------

    obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]
    obtenerCadena (Prediccion _) _ = Nothing
    obtenerCadena orac pred
        | pred `notElem` obtenerPredicciones orac = Nothing
        | otherwise = Just $ paso orac pred

    obtenerPredicciones :: Oraculo -> [String]
    obtenerPredicciones (Prediccion pred) = [pred]
    obtenerPredicciones (Pregunta _ op) = [pred | (k, v) <- Map.toList op, pred <- obtenerPredicciones v]

    paso :: Oraculo -> String -> [(String, String)]
    paso (Prediccion _) _ = []
    paso (Pregunta preg op) pred
        | pred `notElem` obtenerPredicciones (Pregunta preg op) = []
        | otherwise = [(preg, key) | (key, value) <- Map.toList op, pred `elem` obtenerPredicciones value]
        ++ concatMap (`paso` pred) (Map.elems op)

    --obtenerEstadisticas
    obtenerEstadisticas :: Oraculo -> (Float, Float, Float)
    obtenerEstadisticas orac = (
        minimum depths,
        maximum depths,
        sum depths / fromIntegral (length depths)
        ) where
            depths = fromIntegral <$> profundidades orac

    profundidades :: Oraculo -> [Int]
    profundidades (Prediccion _) = [0]
    profundidades (Pregunta preg op) = map (obtenerProfundidad (Pregunta preg op)) (obtenerPredicciones (Pregunta preg op))
    
    obtenerProfundidad :: Oraculo -> String -> Int
    obtenerProfundidad (Prediccion _) _ = 0
    obtenerProfundidad orac pred
        | pred `notElem` obtenerPredicciones orac = 0
        | otherwise = 1 + maximum (map (`obtenerProfundidad` pred) (Map.elems $ opciones orac))
    


-----------------------------------------------------------------------------------
---------------------------  FUNCIONES DE CONSTRUCCION  ---------------------------

    crearOraculo :: String -> Oraculo
    crearOraculo = Prediccion

    ramificar :: [String] -> [Oraculo] -> String -> Oraculo
    ramificar [] [] _ = error "No se puede ramificar un oraculo sin opciones"
    ramificar [] _ _ = error "No se puede ramificar un oraculo sin opciones"
    ramificar _ [] _ = error "No se puede ramificar un oraculo sin opciones"
    ramificar ops oracls preg = Pregunta preg $ Map.fromList $ zip ops oracls
