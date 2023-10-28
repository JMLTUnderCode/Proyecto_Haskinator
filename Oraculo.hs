module Oraculo (
    Oraculo (..),
    Opciones,
    crearOraculo,
    ramificar,
    prediccion,
    pregunta,
    opciones,
    respuesta,
    obtenerCadena,
    obtenerEstadisticas,
    preguntaCrucial
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
        show (Prediccion str) = showData (Prediccion str) 1
        show (Pregunta str op) = showData (Pregunta str op) 1
    
    {-  Funcion que permite realizar identacion mediante tabulaciones
        de cantidad n dado por argumento. -}
    identation:: Int -> String 
    identation n = "\n" ++ concat (replicate n "\t") ++ "- "
    
    -- Funcion que permite mostrar el arbol de conocimiento de Haskinator.
    showData :: Oraculo -> Int -> String
    showData (Prediccion str) _ = str
    showData (Pregunta str op) n = str ++ concatMap (\a -> do
            identation n ++ a ++ ": "
            ++ showData (fromJust $ Map.lookup a op) (n + 1)
        ) (Map.keys op)

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
            Prediccion $ head input
            else do
                -- Extraccion de nodo pregunta y ramas asociadas.
                let splitedTree = concatMap (splitOn (identation n)) input

                -- Elementos restantes del split realizado.
                let elements = tail splitedTree

                -- Lista de respuestas para cada elemento restante.
                let answerList = map (takeWhile (/= ':')) elements 

                -- Lista de oraculos a traducir por cada elemento restante. 
                let oracleList = map (drop 2 . dropWhile (/= ':')) elements

                -- Lista de oraculos traducidos de las cuales se realizan llamadas recursivas
                -- por cada subrama dada en la lista de oraculos disponibles.
                let translatedOracles = map (\x -> readData [x] (n+1)) oracleList 

                -- Emparejamiento de respuestas a la pregunta nodo principal con los oraculos traducidos.
                let tuples = zip answerList translatedOracles

                -- Creacion de subarboles de datos.
                Pregunta (head splitedTree) $ Map.fromList tuples

-----------------------------------------------------------------------------------
-----------------------------  FUNCIONES DE ACCESO  -------------------------------

    prediccion :: Oraculo -> String
    prediccion (Prediccion prediction) = prediction
    prediccion (Pregunta _ _) = error "No se puede obtener algo de tipo 'Prediccion' de algo tipo 'Pregunta'." 

    pregunta :: Oraculo -> String
    pregunta (Pregunta question _) = question
    pregunta (Prediccion _) =  error "No se puede obtener algo de tipo 'Pregunta' de algo tipo 'Prediccion'."

    opciones :: Oraculo -> Opciones
    opciones (Pregunta _ options) = options
    opciones (Prediccion _ ) = error "No se pueden obterner opciones a partir de algo tipo 'Prediccion'."

    respuesta :: Oraculo -> String -> Oraculo
    respuesta (Pregunta _ options) answer = case
        Map.lookup answer options of
            Just newOp -> newOp
            Nothing -> error $ "No existen opciones asociadas a la respuesta '" ++ answer ++ "'."
    respuesta (Prediccion _) _ = error "No se pueden obterner respuestas a partir de algo tipo 'Prediccion'."

-----------------------------------------------------------------------------------
----------------------------  FUNCIONES DE INSPECCION  ----------------------------

    obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]
    obtenerCadena (Prediccion _) _ = Nothing
    obtenerCadena orac pred
        | pred `notElem` obtenerPredicciones orac = Nothing
        | otherwise = Just $ camino orac pred

    obtenerPredicciones :: Oraculo -> [String]
    obtenerPredicciones (Prediccion pred) = [pred]
    obtenerPredicciones (Pregunta _ op) = [pred | (k, v) <- Map.toList op, pred <- obtenerPredicciones v]

    camino :: Oraculo -> String -> [(String, String)]
    camino (Prediccion _) _ = []
    camino (Pregunta preg op) pred
        | pred `notElem` obtenerPredicciones (Pregunta preg op) = []
        | otherwise = head [(preg, key) | (key, value) <- Map.toList op, pred `elem` obtenerPredicciones value]
        : camino (head [o | o <- Map.elems op, pred `elem` obtenerPredicciones o]) pred

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



-----------------------------------------------------------------------------------
-----------------------------  FUNCIONES ADICIONALES  -----------------------------
    -- Funcion que dado un oraculo y dos strings retorna el ancestro comun mas 
    -- bajo en el arbol informacion descrito por Oraculo.
    preguntaCrucial :: Oraculo -> String -> String -> Maybe (String, String, String)
    preguntaCrucial oraculo pred1 pred2 = do
        c1 <- obtenerCadena oraculo pred1
        c2 <- obtenerCadena oraculo pred2
        if null c1 || null c2 then Nothing
        else do
            let cC = caminoComun c1 c2
            let l = length cC
            let c1Diff = drop l c1
            let c2Diff = drop l c2
            Just (fst $ head c1Diff, snd $ head c1Diff, snd $ head c2Diff)

    caminoComun :: [(String, String)] -> [(String, String)] -> [(String, String)]
    caminoComun [] _ = []
    caminoComun _ [] = []
    caminoComun (xh : xt) (yh : yt) 
        | xh == yh =  xh : caminoComun xt yt
        | otherwise = []
