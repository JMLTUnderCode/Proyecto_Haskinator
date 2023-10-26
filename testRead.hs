import Oraculo

main :: IO()
main = do
  -- Para probar la funcion readData que usa la instancia Read
  putStrLn "\n test 1"
  putStrLn $ show $ readData ["a\n\t- b: c\n\t\t- d: e\n\t- f: g\n\t\t- h: i"] 1
  
  putStrLn "\n test 2"
  putStrLn $ show $ readData ["Estas bien?\n\t- No: Estas enferma?\n\t\t- No: Comiste bien\n\t\t- Si: Comiste mal"] 1
  
  putStrLn "\n test 3"
  putStrLn $ show $ readData ["Prediccion directa"] 1
  
  putStrLn "\n test 4"
  putStrLn $ show $ readData ["Es un lenguaje de programacion?\r\n\t- No: HTML\n\t- Si: A que paradigma pertenece?\r\n\t\t- Funcional: Haskell\r\n\t\t- Imperativo: A quien pertenece?\r\n\t\t\t- Google: Go\r\n\t\t\t- Microsoft: C#\r\n\t\t\t- Oracle: Java\r\n\t\t- Logico: Prolog\r"] 1

  -- Para probar la instancia read directamente, realizar en consola
  -- cargando el modulo al interprete.
  -- ghci Oraculo.hs
  -- >> import Oraculo
  -- >> read "a\n\t- b: c\n\t\t- d: e\n\t- f: g\n\t\t- h: i" :: Oraculo
  -- >> read "hola?\n\t- No: Ayer?\n\t\t- No: tomate\n\t\t- Si: Paracetamol" :: Oraculo
  -- >> read "hola" :: Oraculo
  -- >> read "Es un lenguaje de programacion?\r\n\t- No: HTML\n\t- Si: A que paradigma pertenece?\r\n\t\t- Funcional: Haskell\r\n\t\t- Imperativo: A quien pertenece?\r\n\t\t\t- Google: Go\r\n\t\t\t- Microsoft: C#\r\n\t\t\t- Oracle: Java\r\n\t\t- Logico: Prolog\r" :: Oraculo