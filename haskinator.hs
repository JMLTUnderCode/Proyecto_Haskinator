-- Importacion de modulo Oraculo y modulos generales para el funcionaimento del programa.
import Oraculo
import System.IO
import qualified Data.Map as Map
import System.IO.Unsafe ( unsafePerformIO )
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.Either
import Data.Char
import System.Directory

-- Funcion que se encarga de realizar el proceso de prediccion.
-- actualizar :: ?? -> Oraculo

-- Mostrar el oraculo en version Prediccion.
predecir :: Oraculo -> IO ()
predecir (Prediccion prediction) = do
    putStr("Haskinator : Prediccion: " ++ prediction ++ "\n              Si / No" ++ "\nUsuario    : ")
    inputUser <- getLine
    if map toUpper inputUser == "SI" then 
        putStrLn "¡Soy demasiado genial! Haskinator lo sabe todo."
        else do 
            if map toUpper inputUser == "NO" then
                putStrLn "Haskinator : He fallado! Cual era la respuesta correcta?"
                -- DEBEMOS ACTUALIZAR
                else do
                    putStrLn $ "Haskinator : La opcion '" ++ inputUser ++ "' no es valida."
                    predecir (Prediccion prediction)

-- Mostrar un Oraculo en version Pregunta, se muestra la misma y las opciones que conllevan esa pregunta.
predecir oracle = do
    let options = opciones oracle
    let answer = pregunta oracle
    let availableOptions = Map.keys options
    putStr $ "Haskinator : " ++ answer ++ "\n              " ++ concatMap ( ++ " / ") availableOptions ++ "Ninguna\nUsuario    : "
    inputUser <- getLine
    -- Verificamos que la opcion sea valida.
    if inputUser `isInfixOf` concat availableOptions then do
        predecir $ respuesta oracle inputUser
        else do
            -- En caso de marcar NINGUNA luego de mostrar las opciones disponibles.
            if map toUpper inputUser == "NINGUNA" then do
                putStrLn "Haskinator : He fallado! Cual era la respuesta correcta?"
                -- DEBEMOS ACTUALIZAR
                else do 
                    putStrLn $ "La opcion '" ++ inputUser ++ "' no existe."
                    predecir oracle
{-
EJEMPLOS
  En caso de responder "No" a una prediccion:
    1. Pedir la RESPUESTA correcta. (Prediccion final)
    2. Pedir la PREGUNTA que distinguie a la RESPUESTA de (1)
    3. Pedir la RESPUESTA a la PREGUNTA de (2) asociada a la respuesta de (1)
    4. Pedir la RESPUESTA a la PREGUNTA de (2) asociada a la respuesta de la prediccion "errada"
    Haskinator : Prediccion: HTML
                 Si / No
    Usuario    : No
    Haskinator : He fallado! Cual era la respuesta correcta?
    Usuario    : CSS
    Haskinator : Que pregunta distingue a CSS de las otras opciones?
    Usuario    : Que tipo de lenguaje es?
    Haskinator : Cual es la respuesta a "Que tipo de lenguaje es?" para CSS?
    Usuario    : De definicion de estilo
    Haskinator : Cual es la respuesta a "Que tipo de lenguaje es?" para HTML?
    Usuario    : De marcado
-}
{-
  En caso de responder NINGUNA en el proceso de seleccion de opciones:
    1. Pedir la RESPUESTA correcta. (Prediccion final)
    2. Pedir la RESPUESTA a la PREGUNTA que no posee la respuesta (1)
  
    Haskinator : Es un lenguaje de programacion?
                 Si / No
    Usuario    : Si
    Haskinator : A que paradigma pertenece?
                 Imperativo / Funcional / Logico
    Usuario    : Imperativo
    Haskinator : A quien pertenece el lenguaje?
                 Oracle / Microsoft
    Usuario    : ninguna
    Haskinator : He fallado! Cual era la respuesta correcta?
    Usuario    : Go
    Haskinator : A quien pertenece el lenguaje?
    Usuario    : Google
-}
  
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
    cliente (Prediccion "")

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
    putStrLn "16-10371 | Daniel Figueroa"
    putStrLn "17-10303 | Junior Lara"
    putStrLn "18-10938 | Astrid Alvarado"
    putStrLn "15-11377 | Carlos Sivira"
    putStrLn "-------------------------------------------------------------------------------"

cliente :: Oraculo -> IO ()
cliente oraculo = do
    putStrLn "\nSelecciona una opción:\n"
    putStrLn "1. Crear un oráculo nuevo"
    putStrLn "2. Predecir"
    putStrLn "3. Persistir"
    putStrLn "4. Cargar"
    putStrLn "5. Consultar pregunta crucial"
    putStrLn "6. Estadísticas"
    putStrLn "7. Salir\n"

    opcion <- getLine
    case opcion of
        "1" -> do -- Crear oráculo
            putStrLn "Ingresa una predicción: "
            inputUser <- getLine

            if not ("\t" `isInfixOf` inputUser) then do
                let newPrediccion = crearOraculo inputUser
                cliente newPrediccion
                else do
                    putStrLn "Formato de prediccion incorrecta."
                    cliente oraculo

        "2" -> do -- Predecir
            predecir oraculo
            
            cliente oraculo

        "3" -> do -- Persistir
            putStrLn "Indica el nombre del archivo"
            inputUser <- getLine
            let fileName = if ".txt" `isInfixOf` inputUser then inputUser else inputUser++".txt"
            persistirOraculo oraculo fileName

            putStrLn $ "Información almacenada en el archivo " ++ fileName
            cliente oraculo

        "4" -> do -- Cargar
            putStrLn "Indica el nombre del archivo"
            inputUser <- getLine

            -- Agregamos la extension .txt en caso de que el archivo no lo tenga.
            let fileName = if ".txt" `isInfixOf` inputUser then inputUser else inputUser++".txt"

            -- Verificando que el archivo exista.
            if not (unsafePerformIO (doesFileExist $ "./" ++ fileName)) then do
                putStrLn $ "El nombre '" ++ fileName ++ "' no existe o no esta asociado a un archivo .txt."
                cliente oraculo
            else do
                let newOraculo = cargarOraculo fileName
                putStrLn $ "Información cargada desde el archivo " ++ fileName
                cliente newOraculo
            
        "5" -> do -- Pregunta crucial
            putStrLn "Indica la primera cadena"
            pred1 <- getLine
            putStrLn "Indica la segunda cadena"
            pred2 <- getLine

            case preguntaCrucial oraculo pred1 pred2 of
                Just (pregunta, opcion1, opcion2) -> do
                    putStrLn $ "Pregunta: '" ++ pregunta ++ "'"
                    putStrLn $ "La opción '" ++ opcion1 ++ "' lleva a '" ++ pred1 ++ "'"
                    putStrLn $ "La opción '" ++ opcion2 ++ "' lleva a '" ++ pred2 ++ "'"
                Nothing -> putStrLn "No se encontró una pregunta crucial."

            cliente oraculo

        "6" -> do -- Estadísticas
            let (oMin, oMax, oAvg) = obtenerEstadisticas oraculo
            putStrLn $ "min: " ++ show oMin ++ "       max: " ++ show oMax ++ "      avg: " ++ show oAvg

            cliente oraculo

        "7" -> do -- Salir
            return ()

        _   -> do -- Error
            putStrLn "Opción inválida."
            cliente oraculo

    hFlush stdout