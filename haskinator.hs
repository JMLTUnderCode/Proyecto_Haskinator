-- Importacion de modulo Oraculo y modulos generales para el funcionaimento del programa.
import Oraculo
import System.IO
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isInfixOf, nub)
import Data.Maybe (fromJust)
import Data.Either
import Data.Char
import System.Directory

hasRepeatPredictions :: Oraculo -> Bool
hasRepeatPredictions (Prediccion _) = False
hasRepeatPredictions orac = hasDuplicates $ obtenerPredicciones orac

-- Funcion generica que determina si hay elementos repetidos en una lista de elementos comparables.
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates lista = length (nub lista) /= length lista

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
        if tuple `elem` mapToList then do
            let updatingOptions =  Map.insert (fst tuple) toBeUpdate $ opciones oracle
            let mainQuestion = pregunta oracle
            Pregunta mainQuestion updatingOptions
            else do
                let valuesFromOptions = Map.elems $ opciones oracle
                let updatingOraculo = map (\x -> if esPregunta x then actualizarOraculo tuple toBeUpdate x else x) valuesFromOptions
                let updatedOptions = Map.keys $ opciones oracle
                let newOracleOptions = zip updatedOptions updatingOraculo
                let firstQuestion = pregunta oracle
                Pregunta firstQuestion $ Map.fromList newOracleOptions
        else
            toBeUpdate

-- Funcion encargada de actualizar el oraculo principal para el caso de que el usuario
-- indique "Ninguna" de las opciones propuestas a una pregunta.
actualizarOraculo' :: (String, Oraculo) -> String -> Oraculo -> Oraculo
actualizarOraculo' toBeUpdate question oracle = do
    let mapToList = Map.toList $ opciones oracle
    if question == pregunta oracle then do
        let newOracle = uncurry Map.insert toBeUpdate $ opciones oracle
        Pregunta question newOracle
        else do
            let elementTarget = filter (\(y, x) -> esPregunta x && question == pregunta x) mapToList
            if not (null elementTarget) then do
                let optionUpdate = opciones $ snd (head elementTarget)
                let newInnerOracle = uncurry Map.insert toBeUpdate optionUpdate
                let newOracle = Map.insert (fst (head elementTarget)) (Pregunta question newInnerOracle) $ opciones oracle
                let mainQuestion = pregunta oracle
                Pregunta mainQuestion newOracle
                else do
                    let elementsOracle = Map.elems $ opciones oracle
                    let updatingOracle = map (\x -> if esPregunta x then actualizarOraculo' toBeUpdate question x else x) elementsOracle
                    let updatedOptions = Map.keys $ opciones oracle
                    let newOracleOptions = zip updatedOptions updatingOracle
                    let firstQuestion = pregunta oracle
                    Pregunta firstQuestion $ Map.fromList newOracleOptions

-- Funcion que se encarga de realizar el proceso de prediccion. Muestra las interacciones con
-- el usuario, solicita datos y realiza llamados a funciones de actualizacion de oraculos.
predecir :: Oraculo -> Oraculo -> String -> IO Oraculo
predecir (Prediccion prediction) originalOracle lastOption = do
    hFlush stdout
    putStr $ "   Haskinator : Prediccion: " ++ prediction ++ "\n                 Si / No" ++ "\n   Usuario    : "
    inputUser <- getLine
    if map toUpper inputUser == "SI" then do
        putStrLn "   ¡Habeis flipao' colores tio! Soy la ostia a que si?"
        return originalOracle
        else do 
            if map toUpper inputUser == "NO" then do
                putStr "   Haskinator : ¡Me cago en la ostia tio! Cual es la puñetera respuesta?\n   Usuario    : "
                expectedAnswer <- getLine

                putStr $ "   Haskinator : Que pregunta distingue a '"++ expectedAnswer ++"' de las otras opciones?\n   Usuario    : "
                distinguishedQuestion <- getLine

                let questionPrompt = "   Haskinator : Cual es la respuesta a '"++ distinguishedQuestion ++"' para '"
                putStr $ questionPrompt ++ expectedAnswer ++ "'?\n   Usuario    : "
                optionForExpectedAnswer <- getLine
                
                -- MAL PRINTEO (SOLVENTAR)
                putStr $ questionPrompt ++ prediction
                putStr "'?\n   Usuario    : "
                optionForGivenPrediction <- getLine

                let newQuestion = Pregunta distinguishedQuestion (Map.fromList [(optionForExpectedAnswer, Prediccion expectedAnswer), (optionForGivenPrediction, Prediccion prediction)])
                let updatedOraculo = actualizarOraculo (lastOption, Prediccion prediction) newQuestion originalOracle
                putStrLn "   Graciah chaval! Pero sobre todo agradecido con el de arriba Papa Dio'"
                return updatedOraculo
                else do
                    putStrLn $ "   Haskinator : Que dices chaval! La opcion '" ++ inputUser ++ "' no es valida."
                    predecir (Prediccion prediction) originalOracle lastOption

-- Mostrar un Oraculo en version Pregunta, se muestra la misma y las opciones que conllevan esa pregunta.
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
                putStr $ "   Haskinator : " ++ answer ++ "\n   Usuario    : "
                optionForExpectedAnswer <- getLine
                let newPrediction = Prediccion expectedAnswer
                let updatedOraculo = actualizarOraculo' (optionForExpectedAnswer, newPrediction) answer originalOracle
                putStrLn "   Graciah chaval! Pero sobre todo agradecido con el de arriba Papa Dio'"
                return updatedOraculo
                else do 
                    putStrLn $ "   La opcion '" ++ inputUser ++ "' no existe."
                    predecir oracle originalOracle lastOption

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

-- Funcion que dado un archivo y la estructura de datos Oraculo plasma en el dicho archivo el arbol 
-- de informacion descrito por Oraculo.
persistirOraculo:: Oraculo -> String -> IO () 
persistirOraculo oraculo nameFile = writeFile nameFile (show oraculo)

-- Funcion encargada de dado un string leido por archivo de texto generar la estructura Oraculo
cargarOraculo:: String -> Oraculo
cargarOraculo fileName = do
    let availableContent = unsafePerformIO (readFile fileName)
    read availableContent :: Oraculo

main :: IO ()
main = do
    header
    cliente (Prediccion "Yo solo se que no se nada chaval. d:v") [Prediccion "Yo solo se que no se nada chaval. d:v"]

cliente :: Oraculo -> [Oraculo] -> IO ()
cliente oraculo predictionList = do
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
                cliente newPrediccion [newPrediccion]
                else do
                    putStrLn "   Formato de prediccion incorrecta."
                    cliente oraculo predictionList

        "2" -> do -- Predecir
            bighead
            newOraculo <- predecir oraculo oraculo ""
            cliente newOraculo []

        "3" -> do -- Persistir
            putStr "   Indica el nombre del archivo: "
            inputUser <- getLine
            let fileName = if ".txt" `isInfixOf` inputUser then inputUser else inputUser++".txt"
            persistirOraculo oraculo fileName

            putStrLn $ "   Información almacenada en el archivo " ++ fileName
            cliente oraculo predictionList

        "4" -> do -- Cargar
            putStr "   Indica el nombre del archivo: "
            inputUser <- getLine

            -- Agregamos la extension .txt en caso de que el archivo no lo tenga.
            let fileName = if ".txt" `isInfixOf` inputUser then inputUser else inputUser++".txt"

            -- Verificando que el archivo exista.
            if not (unsafePerformIO (doesFileExist $ "./" ++ fileName)) then do
                putStrLn $ "   El nombre '" ++ fileName ++ "' no existe o no esta asociado a un archivo .txt."
                cliente oraculo predictionList
            else do
                let newOraculo = cargarOraculo fileName
                let newPredictionList = map (\x -> read x :: Oraculo) $ obtenerPredicciones newOraculo
                if hasRepeatPredictions newOraculo then do
                    putStrLn $ "El archivo "++ fileName ++ " tiene predicciones repetidas."
                    cliente oraculo predictionList
                    else do
                        putStrLn $ "   Información cargada desde el archivo " ++ fileName
                        cliente newOraculo newPredictionList
            
        "5" -> do -- Pregunta crucial
            putStr "   Indica la primera cadena: "
            pred1 <- getLine
            putStr "   Indica la segunda cadena: "
            pred2 <- getLine

            case preguntaCrucial oraculo pred1 pred2 of
                Just (pregunta, opcion1, opcion2) -> do
                    putStrLn $ "   Pregunta: '" ++ pregunta ++ "'"
                    putStrLn $ "   La opción '" ++ opcion1 ++ "' lleva a '" ++ pred1 ++ "'"
                    putStrLn $ "   La opción '" ++ opcion2 ++ "' lleva a '" ++ pred2 ++ "'"
                Nothing -> putStrLn "   No se encontró una pregunta crucial."

            cliente oraculo []

        "6" -> do -- Estadísticas
            let (oMin, oMax, oAvg) = obtenerEstadisticas oraculo
            putStrLn $ "   min: " ++ show oMin ++ "       max: " ++ show oMax ++ "      avg: " ++ show oAvg

            cliente oraculo []

        "7" -> do -- Salir
            putStrLn "   Haskinator Españolete. Version 1.0. "

            return ()

        _   -> do -- Error
            putStrLn "   Opción inválida."
            cliente oraculo []

    hFlush stdout


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


bighead :: IO ()
bighead = do
    putStr("                                                                   ;.      \n")
    putStr("                                                                   ;x.     \n")
    putStr("                                                                   ;X+.    \n")
    putStr("                                                                 .;XXX;    \n")
    putStr("                                . . . . .                    .:+XXXXXX:    \n")
    putStr("                     .:;;;;;::::...              .:;      .+XXXXXXXX;.     \n")
    putStr("              .:;;;;;:::::::::                      ::.  .+xXXXXX+:.       \n")
    putStr("             :::::::::;;:::::                         :;:.+xXX+:           \n")
    putStr("            ;:::::;::::::;;::::                         .;++X;             \n")
    putStr("           :::::;::::::::::;+;:::..                       .;+:             \n")
    putStr("          .;:::;::::::::::.  :++;::::;;;;;;;;;;::::::::..    :;:           \n")
    putStr("          ::::;:::::::;;;;;:.  :++;:::::                ...:;;;;;:.        \n")
    putStr("         .;::;::::::+;:.:..:;    ;;;;:::.                         :;.      \n")
    putStr("         .:::::::::;;:.:;;:.::    :+:;:::.                         .:;.    \n")
    putStr("         :::;::::::;;:::;:..;:     .;;:;:::.                         .;:   \n")
    putStr("         :::::::::::+;;;;+::;        :;:;;::                           ::  \n")
    putStr("         ::;::::::::::;:::;:          :;::+::.                          :: \n")
    putStr("         .;;::::::...........:.::::... :;::;;:.                          ;.\n")
    putStr("          .;::::::::::;;;;;;;;;;::::::;;+++;+;:::...                     :.\n")
    putStr("           .;::;;+;;::;::;;;;..............::;;++;::::.                  ;.\n")
    putStr("            .+X;;................................:;;;:::.               .;.\n")
    putStr("             .x+:..:;++++;;;;;;;;;;;;:...........:; :;:::.              :;.\n")
    putStr("              .x:................................;+: .:;::.             ;;.\n")
    putStr("               ;:::;+x+:.......:+XXx++++X:.........;:. .::.            .;: \n")
    putStr("              .+;+X$x;..:......:...................;: .  ..:::.        .;: \n")
    putStr("              .X++x$XX+........+Xx+&&X+;::.........;;.     .+..::;;;;::;;  \n")
    putStr("              :;....++.........:X+.+Xxx+;;.........:+;. :+x;:;.  :;:::;;.  \n")
    putStr("              ;:......;;.........;:..................:x+:....:;  :;  .;:   \n")
    putStr("              ;:.....:+.......................................; .;. .;:    \n")
    putStr("              ;:.....+:...............................::++:...;:;. .:.     \n")
    putStr("              ;:....:+...................................:;...;; .;:       \n")
    putStr("              ;:....x.......:;;..........................;:..;++;:         \n")
    putStr("              :+....;++;:;;;;+:+...............:......;++;..;:.            \n")
    putStr("               +...:;..........:;..............+:....;;...:;:              \n")
    putStr("               ;:..:;...........:;............+::...;x+X$;.                \n")
    putStr("               :;..:;............::.....................;:                 \n")
    putStr("                +..:+++++++++++++:......................::                 \n")
    putStr("                .;.................:............++...:..:;                 \n")
    putStr("                 :;....;:......................X;...+;..+                  \n")
    putStr("                  :;..:+:+;:.................:+....:::;+.                  \n")
    putStr("                   ::..;$$$X:...............;+...::;;:                     \n")
    putStr("                    ;;:X$$$$;............::X:.;+.                          \n")
    putStr("                     .x$$$$$X.........:;+;:..                              \n")
    putStr("                      +$Xxx+;;+;;::.                                       \n")
    putStr("                      :$$;                                                 \n")
    putStr("                   :$$$$$X:                                                \n")
    putStr("                   ;$::::.                                                 \n")
    putStr("                   :X:                                                     \n")
    putStr("                     ;.                                                    \n")