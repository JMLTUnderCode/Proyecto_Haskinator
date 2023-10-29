module Haskinator (main) where
    
-- Importacion de modulo Oraculo y modulos generales para el funcionaimento del programa.
import Oraculo
import System.IO
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isInfixOf, nub)
import Data.Maybe (fromJust, isJust)
import Data.Either
import Data.Char
import System.Directory

---------------------------------------------------------------------------------------
-----------------------------  FUNCIONES PRINCIPALES ----------------------------------
main :: IO ()
main = do
    header
    cliente (Prediccion "Yo solo se que no se nada chaval. d:v")

cliente :: Oraculo -> IO ()
cliente oracleData = do
    putStrLn "-------------------------------------------------------------------------------"
    putStrLn "                        ***  Selecciona una opción: ***"
    putStrLn "           1. Crear un oráculo nuevo     5. Consultar pregunta crucial"
    putStrLn "           2. Predecir                   6. Estadísticas"
    putStrLn "           3. Persistir                  7. Salir"
    putStrLn "           4. Cargar"
    putStr "   >> "

    opcion <- getLine
    case opcion of
        "1" -> do -- Crear oráculo
            putStr "   Ingresa una predicción: "
            inputUser <- getLine

            if not ("\t" `isInfixOf` inputUser) then do
                let newPrediccion = crearOraculo inputUser
                cliente newPrediccion
                else do
                    putStrLn "   Error: Formato de prediccion incorrecta."
                    cliente oracleData

        "2" -> do -- Predecir
            bigHead
            newOraculo <- predecir oracleData oracleData ""
            cliente newOraculo

        "3" -> do -- Persistir
            putStr "   Indica el nombre del archivo: "
            inputUser <- getLine
            let fileName = if ".txt" `isInfixOf` inputUser then inputUser else inputUser++".txt"
            persistirOraculo oracleData fileName

            putStrLn $ "   Información almacenada en el archivo " ++ fileName
            cliente oracleData 

        "4" -> do -- Cargar
            putStr "   Indica el nombre del archivo: "
            inputUser <- getLine

            -- Agregamos la extension .txt en caso de que el archivo no lo tenga.
            let fileName = if ".txt" `isInfixOf` inputUser then inputUser else inputUser++".txt"

            -- Verificando que el archivo exista.
            if not (unsafePerformIO (doesFileExist $ "./" ++ fileName)) then do
                putStrLn $ "   Error: El nombre '" ++ fileName ++ "' no existe o no esta asociado a un archivo .txt."
                cliente oracleData 
                else do
                    let newOracle = cargarOraculo fileName
                    -- Verificamos que no existen predicciones repetidas.
                    if hasRepeatPredictions newOracle then do
                        putStrLn "   Error: Este arbol de conocimiento posee predicciones repetidas."
                        putStrLn "          Asegurese de que solo existe una prediccion una unica vez."
                        cliente oracleData
                        else do
                            putStrLn $ "   Información cargada desde el archivo " ++ fileName
                            cliente newOracle

        "5" -> do -- Pregunta crucial
            putStr "   Indica la primera cadena: "
            pred1 <- getLine
            putStr "   Indica la segunda cadena: "
            pred2 <- getLine

            case preguntaCrucial oracleData pred1 pred2 of
                Just (pregunta, opcion1, opcion2) -> do
                    putStrLn $ "   Pregunta: '" ++ pregunta ++ "'"
                    putStrLn $ "   La opción '" ++ opcion1 ++ "' lleva a '" ++ pred1 ++ "'"
                    putStrLn $ "   La opción '" ++ opcion2 ++ "' lleva a '" ++ pred2 ++ "'"
                Nothing -> putStrLn "   Error: No se encontró una pregunta crucial."

            cliente oracleData

        "6" -> do -- Estadísticas
            let (oMin, oMax, oAvg) = obtenerEstadisticas oracleData
            putStrLn $ "   min: " ++ show oMin ++ "       max: " ++ show oMax ++ "      avg: " ++ show oAvg

            cliente oracleData

        "7" -> do -- Salir
            putStrLn "   Haskinator Españolete. Version 1.0. "

            return ()

        _   -> do -- Error
            putStrLn "   Error: Opción inválida."

            cliente oracleData

    hFlush stdout

-- Funcion que se encarga de realizar el proceso de prediccion. Muestra las interacciones con
-- el usuario, solicita datos y realiza llamados a funciones de actualizacion de oraculos.
predecir :: Oraculo -> Oraculo -> String -> IO Oraculo

-- En caso de recibir Prediccion se realiza la comprobacion.
predecir (Prediccion prediction) originalOracle lastOption = do
    hFlush stdout
    -- Verificamos aceptacion de prediccion.
    putStr $ "   Haskinator : Prediccion: " ++ prediction ++ "\n                 Si / No" ++ "\n   Usuario    : "
    inputUser <- getLine
    -- En caso de que la prediccion sea aceptada.
    if map toUpper inputUser == "SI" then do
        putStrLn "   ¡Habeis flipao' colores tio! Soy la ostia a que si?"
        return originalOracle
        -- En caso de que la prediccion sea negada.
        else do 
            if map toUpper inputUser == "NO" then do
                -- Solicitamos todos los datos pertinentes para una nueva prediccion.
                putStr "   Haskinator : ¡Me cago en la ostia tio! Cual es la puñetera respuesta?\n   Usuario    : "
                expectedAnswer <- getLine

                if isJust (obtenerCadena originalOracle expectedAnswer) then do
                    putStrLn $  "   Haskinator : ¡Manda cojones chavalillo! La predicción '" ++ expectedAnswer ++ "' ya existe."
                    return originalOracle
                else do
                    putStr $ "   Haskinator : Que pregunta distingue a '"++ expectedAnswer ++"' de las otras opciones?\n   Usuario    : "
                    distinguishedQuestion <- getLine

                    let questionPrompt = "   Haskinator : Cual es la respuesta a '"++ distinguishedQuestion ++"' para '"
                    putStr $ questionPrompt ++ expectedAnswer ++ "'?\n   Usuario    : "
                    optionForExpectedAnswer <- getLine
                    
                    putStr $ questionPrompt ++ prediction
                    putStr "'?\n   Usuario    : "
                    optionForGivenPrediction <- getLine

                    -- Actualizamos el Oraculo con los nuevos datos y predeccion dada.
                    let newQuestion = Pregunta distinguishedQuestion (Map.fromList [(optionForExpectedAnswer, Prediccion expectedAnswer), (optionForGivenPrediction, Prediccion prediction)])
                    let updatedOraculo = actualizarOraculo (lastOption, Prediccion prediction) newQuestion originalOracle
                    putStrLn "   Graciah chaval! Pero sobre todo agradecido con el de arriba Papa Dio'"
                    return updatedOraculo
            else do
                putStrLn $ "   Haskinator : Que dices chaval! La opcion '" ++ inputUser ++ "' no es valida."
                predecir (Prediccion prediction) originalOracle lastOption

-- En caso de recibir Oraculo de tipo Pregunta se realizan confirmaciones de aceptaciones de
-- opciones como tambien realizar el proceso de recorrido del arbol.
predecir oracle originalOracle lastOption = do
    let options = opciones oracle
    let answer = pregunta oracle
    let availableOptions = Map.keys options
    putStr $ "   Haskinator : " ++ answer ++ "\n                 " ++ concatMap ( ++ " / ") availableOptions ++ "Ninguna\n   Usuario    : "
    inputUser <- getLine
    -- Verificamos que la opcion sea valida.
    if inputUser `isInfixOf` concat availableOptions then do
        let newOracle = respuesta oracle inputUser  
        predecir newOracle originalOracle inputUser        
        else do
            -- En caso de marcar NINGUNA luego de mostrar las opciones disponibles.
            if map toUpper inputUser == "NINGUNA" then do
                putStr "   Haskinator : ¡Me cago en la ostia tio! Cual es la puñetera respuesta?\n   Usuario    : "
                expectedAnswer <- getLine
                if isJust (obtenerCadena originalOracle expectedAnswer) then do
                    putStrLn $  "   Haskinator : ¡Manda cojones chavalillo! La predicción '" ++ expectedAnswer ++ "' ya existe."
                    return originalOracle
                    else do
                        putStr $ "   Haskinator : " ++ answer ++ "\n   Usuario    : "
                        optionForExpectedAnswer <- getLine
                        let newPrediction = Prediccion expectedAnswer
                        let updatedOraculo = actualizarOraculo' (optionForExpectedAnswer, newPrediction) answer originalOracle
                        putStrLn "   Graciah chaval! Pero sobre todo agradecido con el de arriba Papa Dio'"
                        return updatedOraculo
                else do 
                    putStrLn $ "   La opcion '" ++ inputUser ++ "' no existe."
                    predecir oracle originalOracle lastOption

-- Funcion que dado un archivo y la estructura de datos Oraculo plasma en el dicho archivo el arbol 
-- de informacion descrito por Oraculo.
persistirOraculo:: Oraculo -> String -> IO () 
persistirOraculo oraculo nameFile = writeFile nameFile (show oraculo)

-- Funcion encargada de dado un string leido por archivo de texto generar la estructura Oraculo
cargarOraculo:: String -> Oraculo
cargarOraculo fileName = do
    let availableContent = unsafePerformIO (readFile fileName)
    read availableContent :: Oraculo

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

---------------------------------------------------------------------------------------
-----------------------------  FUNCIONES AUXILIARES -----------------------------------

-- Funcion que dado un Oraculo determina si hay o no predicciones repetidas.
hasRepeatPredictions :: Oraculo -> Bool
hasRepeatPredictions (Prediccion _) = False
hasRepeatPredictions orac = hasDuplicates $ obtenerPredicciones orac

-- Funcion generica que determina si hay elementos repetidos en una lista de elementos comparables.
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates lista = length (nub lista) /= length lista

-- Funcion que dado un Oraculo retorna una lista de strings con las predicciones que contiene.
obtenerPredicciones :: Oraculo -> [String]
obtenerPredicciones (Prediccion pred) = [pred]
obtenerPredicciones (Pregunta _ op) = [pred | (k, v) <- Map.toList op, pred <- obtenerPredicciones v]

-- Funcion que determinar si un Oraculo es de tipo Pregunta o es Prediccion.
esPregunta :: Oraculo -> Bool
esPregunta (Pregunta _ _) = True
esPregunta (Prediccion _) = False

-- Funcion encargada de actualizar el oraculo principal para el caso de que el usuario
-- diga que una prediccion dada es incorrecta.
actualizarOraculo :: (String, Oraculo) -> Oraculo -> Oraculo -> Oraculo
actualizarOraculo tuple toBeUpdate oracle = do
    if esPregunta oracle then do
        let mapToList = Map.toList $ opciones oracle
        if tuple `elem` mapToList then do -- Caso cuando encontramos el nivel a actualizar.
            let updatingOptions =  Map.insert (fst tuple) toBeUpdate $ opciones oracle
            let mainQuestion = pregunta oracle
            Pregunta mainQuestion updatingOptions
            else do -- Realizando proceso de busqueda de en los niveles del arbol.
                -- Elementos del oraculo actual.
                let valuesFromOptions = Map.elems $ opciones oracle
                -- Actualizacion recursiva para cada elementos de tipo Pregunta Oraculo
                let updatingOraculo = map (\x -> if esPregunta x then actualizarOraculo tuple toBeUpdate x else x) valuesFromOptions
                -- Extrayendo las llaves del mapa actual asociado al oraculo.
                let updatedOptions = Map.keys $ opciones oracle
                -- Haciendo el match de llaves con elementos ya actualizado.
                let newOracleOptions = zip updatedOptions updatingOraculo
                -- Extraccion de la pregunta nodo principal.
                let firstQuestion = pregunta oracle
                -- Creacion y actualizacion del oraculo para la nueva rama agregada.
                Pregunta firstQuestion $ Map.fromList newOracleOptions
        else
            toBeUpdate

-- Funcion encargada de actualizar el oraculo principal para el caso de que el usuario
-- indique "Ninguna" de las opciones propuestas a una pregunta.
actualizarOraculo' :: (String, Oraculo) -> String -> Oraculo -> Oraculo
actualizarOraculo' toBeUpdate question oracle = do
    let mapToList = Map.toList $ opciones oracle
    if question == pregunta oracle then do -- Caso cuando tenemos la pregunta inicial.
        let newOracle = uncurry Map.insert toBeUpdate $ opciones oracle
        Pregunta question newOracle
        else do -- Buscamos recursivamente.
            -- Buscamos los pares ordenados cuya segunda posicion sea Oraculo Pregunta.
            let elementTarget = filter (\(y, x) -> esPregunta x && question == pregunta x) mapToList
            if not (null elementTarget) then do
                -- Realizamos la actualicion del nuevo oraculo.
                let optionUpdate = opciones $ snd (head elementTarget)
                let newInnerOracle = uncurry Map.insert toBeUpdate optionUpdate
                let newOracle = Map.insert (fst (head elementTarget)) (Pregunta question newInnerOracle) $ opciones oracle
                let mainQuestion = pregunta oracle
                Pregunta mainQuestion newOracle
                else do
                    -- Realizamos la llamada recursiva para actualizas posteriormente el oraculo.
                    let elementsOracle = Map.elems $ opciones oracle
                    let updatingOracle = map (\x -> if esPregunta x then actualizarOraculo' toBeUpdate question x else x) elementsOracle
                    let updatedOptions = Map.keys $ opciones oracle
                    let newOracleOptions = zip updatedOptions updatingOracle
                    let firstQuestion = pregunta oracle
                    Pregunta firstQuestion $ Map.fromList newOracleOptions

-- Funcion que dadas dos cadenas (listas de preguntas hasta una predicción), devuelve 
-- el segmento común entre ellas partiendo desde el inicio de ambas listas
caminoComun :: [(String, String)] -> [(String, String)] -> [(String, String)]
caminoComun [] _ = []
caminoComun _ [] = []
caminoComun (xh : xt) (yh : yt) 
    | xh == yh =  xh : caminoComun xt yt
    | otherwise = []

---------------------------------------------------------------------------------------
--------------------------- IMPRESIONES PARA EL CLIENTE -------------------------------
header :: IO ()
header = do
    putStrLn "\n"
    putStrLn "██╗  ██╗ █████╗ ███████╗██╗  ██╗██╗███╗   ██╗ █████╗ ████████╗ ██████╗ ██████╗ "
    putStrLn "██║  ██║██╔══██╗██╔════╝██║ ██╔╝██║████╗  ██║██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗"
    putStrLn "███████║███████║███████╗█████╔╝ ██║██╔██╗ ██║███████║   ██║   ██║   ██║██████╔╝"
    putStrLn "██╔══██║██╔══██║╚════██║██╔═██╗ ██║██║╚██╗██║██╔══██║   ██║   ██║   ██║██╔══██╗"
    putStrLn "██║  ██║██║  ██║███████║██║  ██╗██║██║ ╚████║██║  ██║   ██║   ╚██████╔╝██║  ██║"
    putStrLn "╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝"
    putStrLn "-------------------------------------------------------------------------------"
    putStrLn "                                    AUTORES"
    putStrLn "-------------------------------------------------------------------------------"
    putStrLn "   16-10371 | Daniel Figueroa"
    putStrLn "   17-10303 | Junior Lara"
    putStrLn "   18-10938 | Astrid Alvarado"
    putStrLn "   15-11377 | Carlos Sivira"

bigHead :: IO ()
bigHead = do
    putStrLn "                                                                   ;.      "
    putStrLn "                                                                   ;x.     "
    putStrLn "                                                                   ;X+.    "
    putStrLn "                                                                 .;XXX;    "
    putStrLn "                                . . . . .                    .:+XXXXXX:    "
    putStrLn "                     .:;;;;;::::...              .:;      .+XXXXXXXX;.     "
    putStrLn "              .:;;;;;:::::::::                      ::.  .+xXXXXX+:.       "
    putStrLn "             :::::::::;;:::::                         :;:.+xXX+:           "
    putStrLn "            ;:::::;::::::;;::::                         .;++X;             "
    putStrLn "           :::::;::::::::::;+;:::..                       .;+:             "
    putStrLn "          .;:::;::::::::::.  :++;::::;;;;;;;;;;::::::::..    :;:           "
    putStrLn "          ::::;:::::::;;;;;:.  :++;:::::                ...:;;;;;:.        "
    putStrLn "         .;::;::::::+;:.:..:;    ;;;;:::.                         :;.      "
    putStrLn "         .:::::::::;;:.:;;:.::    :+:;:::.                         .:;.    "
    putStrLn "         :::;::::::;;:::;:..;:     .;;:;:::.                         .;:   "
    putStrLn "         :::::::::::+;;;;+::;        :;:;;::                           ::  "
    putStrLn "         ::;::::::::::;:::;:          :;::+::.                          :: "
    putStrLn "         .;;::::::...........:.::::... :;::;;:.                          ;."
    putStrLn "          .;::::::::::;;;;;;;;;;::::::;;+++;+;:::...                     :."
    putStrLn "           .;::;;+;;::;::;;;;..............::;;++;::::.                  ;."
    putStrLn "            .+X;;................................:;;;:::.               .;."
    putStrLn "             .x+:..:;++++;;;;;;;;;;;;:...........:; :;:::.              :;."
    putStrLn "              .x:................................;+: .:;::.             ;;."
    putStrLn "               ;:::;+x+:.......:+XXx++++X:.........;:. .::.            .;: "
    putStrLn "              .+;+X$x;..:......:...................;: .  ..:::.        .;: "
    putStrLn "              .X++x$XX+........+Xx+&&X+;::.........;;.     .+..::;;;;::;;  "
    putStrLn "              :;....++.........:X+.+Xxx+;;.........:+;. :+x;:;.  :;:::;;.  "
    putStrLn "              ;:......;;.........;:..................:x+:....:;  :;  .;:   "
    putStrLn "              ;:.....:+.......................................; .;. .;:    "
    putStrLn "              ;:.....+:...............................::++:...;:;. .:.     "
    putStrLn "              ;:....:+...................................:;...;; .;:       "
    putStrLn "              ;:....x.......:;;..........................;:..;++;:         "
    putStrLn "              :+....;++;:;;;;+:+...............:......;++;..;:.            "
    putStrLn "               +...:;..........:;..............+:....;;...:;:              "
    putStrLn "               ;:..:;...........:;............+::...;x+X$;.                "
    putStrLn "               :;..:;............::.....................;:                 "
    putStrLn "                +..:+++++++++++++:......................::                 "
    putStrLn "                .;.................:............++...:..:;                 "
    putStrLn "                 :;....;:......................X;...+;..+                  "
    putStrLn "                  :;..:+:+;:.................:+....:::;+.                  "
    putStrLn "                   ::..;$$$X:...............;+...::;;:                     "
    putStrLn "                    ;;:X$$$$;............::X:.;+.                          "
    putStrLn "                     .x$$$$$X.........:;+;:..                              "
    putStrLn "                      +$Xxx+;;+;;::.                                       "
    putStrLn "                      :$$;                                                 "
    putStrLn "                   :$$$$$X:                                                "
    putStrLn "                   ;$::::.                                                 "
    putStrLn "                   :X:                                                     "
    putStrLn "                     ;.                                                    "