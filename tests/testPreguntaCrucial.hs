import Oraculo

main :: IO()
main = do

    {- 
        Es un lenguaje de programacion?
            - Si: A que paradigma pertenece?
                - Imperativo: A quien pertenece el lenguaje?
                    - Oracle: Java
                    - Microsoft: C#
                    - Google: Go
                - Funcional: Haskell
                - Logico: Prolog
            - No: Que tipo de lenguaje es?
                - De marcado: HTML
                - De definicion de estilo: CSS
     -}


    let java = "Java"
    let csharp = "C#"
    let go = "Go"
    let haskell = "Haskell"
    let prolog = "Prolog"
    let html = "HTML"
    let css = "CSS" 

    let oJava = crearOraculo java
    let oCsharp = crearOraculo csharp
    let oGo = crearOraculo go
    let oHaskell = crearOraculo haskell
    let oProlog = crearOraculo prolog
    let oHtml = crearOraculo html
    let oCss = crearOraculo css

    let oPertenece = ramificar ["Oracle", "Microsoft", "Google"] [oJava, oCsharp, oGo] "A quien pertenece el lenguaje?"
    let oParadigma = ramificar ["Imperativo", "Funcional", "Logico"] [oPertenece, oHaskell, oProlog] "A que paradigma pertenece?"
    let oTipo = ramificar ["De marcado", "De definicion de estilo"] [oHtml, oCss] "Que tipo de lenguaje es?"
    let oLenguaje = ramificar ["Si", "No"] [oParadigma, oTipo] "Es un lenguaje de programacion?"
    let oraculo = oLenguaje

    putStrLn "\n\nCaso 1 -> \n"
    case preguntaCrucial oraculo java haskell of
        Just (pregunta, opcion1, opcion2) -> do
            putStrLn $ "Pregunta: '" ++ pregunta ++ "'"
            putStrLn $ "La opción '" ++ opcion1 ++ "' lleva a '" ++ java ++ "'"
            putStrLn $ "La opción '" ++ opcion2 ++ "' lleva a '" ++ haskell ++ "'"
        Nothing -> putStrLn "No se encontró una pregunta crucial."

    putStrLn "\n\nCaso 2 -> \n"
    case preguntaCrucial oraculo html haskell of
        Just (pregunta, opcion1, opcion2) -> do
            putStrLn $ "Pregunta: '" ++ pregunta ++ "'"
            putStrLn $ "La opción '" ++ opcion1 ++ "' lleva a '" ++ html ++ "'"
            putStrLn $ "La opción '" ++ opcion2 ++ "' lleva a '" ++ haskell ++ "'"
        Nothing -> putStrLn "No se encontró una pregunta crucial."

    putStrLn "\n\nCaso 3 -> \n"
    case preguntaCrucial oraculo html css of
        Just (pregunta, opcion1, opcion2) -> do
            putStrLn $ "Pregunta: '" ++ pregunta ++ "'"
            putStrLn $ "La opción '" ++ opcion1 ++ "' lleva a '" ++ html ++ "'"
            putStrLn $ "La opción '" ++ opcion2 ++ "' lleva a '" ++ css ++ "'"
        Nothing -> putStrLn "No se encontró una pregunta crucial."

    putStrLn "\n\nCaso 4 -> \n"
    case preguntaCrucial oraculo csharp go of
        Just (pregunta, opcion1, opcion2) -> do
            putStrLn $ "Pregunta: '" ++ pregunta ++ "'"
            putStrLn $ "La opción '" ++ opcion1 ++ "' lleva a '" ++ csharp ++ "'"
            putStrLn $ "La opción '" ++ opcion2 ++ "' lleva a '" ++ go ++ "'"
        Nothing -> putStrLn "No se encontró una pregunta crucial."

    putStrLn "\n\nCaso 5 -> \n"
    case preguntaCrucial oraculo "any" go of
        Just (pregunta, opcion1, opcion2) -> do
            putStrLn $ "Pregunta: '" ++ pregunta ++ "'"
            putStrLn $ "La opción '" ++ opcion1 ++ "' lleva a '" ++ csharp ++ "'"
            putStrLn $ "La opción '" ++ opcion2 ++ "' lleva a '" ++ go ++ "'"
        Nothing -> putStrLn "No se encontró una pregunta crucial."