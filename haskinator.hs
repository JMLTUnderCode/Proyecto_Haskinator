-- Importacion de modulo Oraculo toda las funcionalidades.
-- import Oraculo

import System.IO
import qualified Data.Map as Map

data Oraculo = Prediccion String | Pregunta String Opciones
type Opciones = Map.Map String Oraculo

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
    putStrLn("XX-XXXXX | Nombre Apellido")
    putStrLn("XX-XXXXX | Nombre Apellido")
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
        "1" -> do 
            -- Crear oráculo
            putStrLn ("Ingresa una predicción")
            p <- getLine

            cliente (Prediccion p)
        "2" -> do
            -- Predicción
            cliente oraculo
        "3" -> do
            -- Persistir
            putStrLn ("Indica el nombre del archivo")
            fileName <- getLine

            putStrLn ("Información almacenada en el archivo " ++ fileName)
            cliente oraculo
        "4" -> do
            -- Cargar
            putStrLn ("Indica el nombre del archivo")
            fileName <- getLine
            
            putStrLn ("Información cargada desde el archivo " ++ fileName)
            cliente oraculo
        "5" -> do
            -- Pregunta crucial
            putStrLn ("Indica la primera cadena")
            pred1 <- getLine
            putStrLn ("Indica la segunda cadena")
            pred2 <- getLine

            cliente oraculo
        "6" -> do
            -- Estadísticas
            putStrLn ("Mínimo: ~")
            putStrLn ("Máximo: ~")
            putStrLn ("Promedio: ~")

            cliente oraculo
        "7" -> do 
            -- Salir
            return ()
        _   -> do 
            -- Error
            putStrLn "Opción inválida."
            cliente oraculo
    
    hFlush stdout