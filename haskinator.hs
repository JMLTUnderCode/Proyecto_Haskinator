-- Importacion de modulo Oraculo toda las funcionalidades.
-- import Oraculo

import System.IO
import qualified Data.Map as Map

data Oracle = Prediction String | Question String (Map.Map String Oracle)
    deriving (Show, Read)

type Options = Map.Map String Oracle

main :: IO ()
main = do
    header
    client (Prediction "")

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

client :: Oracle -> IO ()
client oracle = do
    putStrLn "\nSelecciona una opción:\n"
    putStrLn "1. Crear un oráculo nuevo"
    putStrLn "2. Predecir"
    putStrLn "3. Persistir"
    putStrLn "4. Cargar"
    putStrLn "5. Consultar pregunta crucial"
    putStrLn "6. Estadísticas"
    putStrLn "7. Salir\n"

    option <- getLine
    case option of
        "1" -> do 
            -- Crear oráculo
            putStrLn ("Ingresa una predicción")
            p <- getLine

            client (Prediction p)
        "2" -> do
            -- Predicción
            client oracle
        "3" -> do
            -- Persistir
            putStrLn ("Indica el nombre del archivo")
            fileName <- getLine

            putStrLn ("Información almacenada en el archivo " ++ fileName)
            client oracle
        "4" -> do
            -- Cargar
            putStrLn ("Indica el nombre del archivo")
            fileName <- getLine
            
            putStrLn ("Información cargada desde el archivo " ++ fileName)
            client oracle
        "5" -> do
            -- Pregunta crucial
            putStrLn ("Indica la primera cadena")
            pred1 <- getLine
            putStrLn ("Indica la segunda cadena")
            pred2 <- getLine

            client oracle
        "6" -> do
            -- Estadísticas
            putStrLn ("Mínimo: ~")
            putStrLn ("Máximo: ~")
            putStrLn ("Promedio: ~")

            client oracle
        "7" -> do 
            -- Salir
            return ()
        _   -> do 
            -- Error
            putStrLn "Opción inválida."
            client oracle
    
    hFlush stdout