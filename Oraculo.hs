module Oraculo where

-----------------------------  IMPORTACION DE MODULOS -----------------------------
    import qualified Data.Map as Map
    import Control.Monad (forM)
    import Data.Maybe (fromJust)
    import Data.List.Split
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

    -- obtenerCadena

    --obtenerEstadisticas

-----------------------------------------------------------------------------------
---------------------------  FUNCIONES DE CONSTRUCCION  ---------------------------

    crearOraculo :: String -> Oraculo
    crearOraculo = Prediccion

    -- ramificar :: [String] -> [Oraculo] -> String -> Oraculo
