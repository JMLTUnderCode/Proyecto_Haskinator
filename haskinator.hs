-- Importacion de modulo Oraculo y modulos generales para el funcionaimento del programa.
import Oraculo
import System.IO
import qualified Data.Map as Map
import System.IO.Unsafe ( unsafePerformIO )
import Data.List (isInfixOf)

-- Funcion encargada de dado un string leido por archivo de texto generar la estructura Oraculo
cargarOraculo:: String -> Oraculo
cargarOraculo fileName = do
    let content = unsafePerformIO (readFile fileName)
    read content :: Oraculo

-- Funcion que dado un archivo y la estructura de datos Oraculo plasma en el dicho archivo el arbol 
-- de informacion descrito por Oraculo.
persistirOraculo:: Oraculo -> String -> IO () 
persistirOraculo oraculo nameFile = writeFile nameFile (show oraculo) 

main :: IO ()
main = do
    header
    cliente (Prediccion "")

header :: IO ()
header = do
    putStrLn("\n")
    putStrLn("██╗  ██╗ █████╗ ███████╗██╗  ██╗██╗███╗   ██╗ █████╗ ████████╗ ██████╗ ██████╗ ")
    putStrLn("██║  ██║██╔══██╗██╔════╝██║ ██╔╝██║████╗  ██║██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗")
    putStrLn("███████║███████║███████╗█████╔╝ ██║██╔██╗ ██║███████║   ██║   ██║   ██║██████╔╝")
    putStrLn("██╔══██║██╔══██║╚════██║██╔═██╗ ██║██║╚██╗██║██╔══██║   ██║   ██║   ██║██╔══██╗")
    putStrLn("██║  ██║██║  ██║███████║██║  ██╗██║██║ ╚████║██║  ██║   ██║   ╚██████╔╝██║  ██║")
    putStrLn("╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝")
    putStrLn("-------------------------------------------------------------------------------")
    putStrLn("                                    AUTORES")
    putStrLn("-------------------------------------------------------------------------------")
    putStrLn("XX-XXXXX | Nombre Apellido")
    putStrLn("17-10303 | Junior Lara")
    putStrLn("18-10938 | Astrid Alvarado")
    putStrLn("15-11377 | Carlos Sivira")                                                                    
    putStrLn("-------------------------------------------------------------------------------")

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
            
            putStrLn ("Ingresa una predicción: ")
            inputUser <- getLine
            
            if not("\t" `isInfixOf` inputUser) then do
                let newPrediccion = crearOraculo inputUser 
                cliente newPrediccion
                else do
                    putStrLn "Formato de prediccion incorrecta."
                    cliente oraculo
                
        "2" -> do -- Predicción
            
            cliente oraculo
            
        "3" -> do -- Persistir
            
            putStrLn ("Indica el nombre del archivo")
            inputUser <- getLine
            let fileName = if ".txt" `isInfixOf` inputUser then inputUser else inputUser++".txt"
            persistirOraculo oraculo fileName
            
            putStrLn ("Información almacenada en el archivo " ++ fileName)
            cliente oraculo
            
        "4" -> do -- Cargar
            
            putStrLn ("Indica el nombre del archivo")
            inputUser <- getLine
            let fileName = if ".txt" `isInfixOf` inputUser then inputUser else inputUser++".txt"
            let newOraculo = cargarOraculo fileName
            
            putStrLn ("Información cargada desde el archivo " ++ fileName)
            cliente newOraculo
            
        "5" -> do -- Pregunta crucial
            
            putStrLn ("Indica la primera cadena")
            pred1 <- getLine
            putStrLn ("Indica la segunda cadena")
            pred2 <- getLine

            cliente oraculo
            
        "6" -> do -- Estadísticas
            
            putStrLn ("Mínimo: ~")
            putStrLn ("Máximo: ~")
            putStrLn ("Promedio: ~")

            cliente oraculo
            
        "7" -> do -- Salir
            return ()
            
        _   -> do -- Error
            
            putStrLn "Opción inválida."
            cliente oraculo

    hFlush stdout