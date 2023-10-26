import Oraculo

main :: IO()
main = do
  -- Para probar la funcion read que usa la instancia Read
  putStrLn "\ntest 1"
  let oraculo1 = read "a\n\t- b: c\n\t\t- d: e\n\t- f: g\n\t\t- h: i" :: Oraculo
  print oraculo1

  putStrLn "\ntest 2"
  let oraculo2 = read "Estas bien?\n\t- No: Estas enferma?\n\t\t- No: Comiste bien\n\t\t- Si: Comiste mal" :: Oraculo
  print oraculo2

  putStrLn "\ntest 3"
  let oraculo3 = read "Prediccion directa" :: Oraculo
  print oraculo3
  
  putStrLn "\ntest 4"
  let oraculo4 = read "Es un lenguaje de programacion?\r\n\t- No: HTML\n\t- Si: A que paradigma pertenece?\r\n\t\t- Funcional: Haskell\r\n\t\t- Imperativo: A quien pertenece?\r\n\t\t\t- Google: Go\r\n\t\t\t- Microsoft: C#\r\n\t\t\t- Oracle: Java\r\n\t\t- Logico: Prolog\r" :: Oraculo
  print oraculo4 

  -- Para probar la instancia read directamente, realizar en consola cargando el modulo 
  -- al interprete.
  -- ghci Oraculo.hs
  -- >> import Oraculo
  -- >> read "a\n\t- b: c\n\t\t- d: e\n\t- f: g\n\t\t- h: i" :: Oraculo
  -- >> read "hola?\n\t- No: Ayer?\n\t\t- No: tomate\n\t\t- Si: Paracetamol" :: Oraculo
  -- >> read "hola" :: Oraculo
  -- >> read "Es un lenguaje de programacion?\r\n\t- No: HTML\n\t- Si: A que paradigma pertenece?\r\n\t\t- Funcional: Haskell\r\n\t\t- Imperativo: A quien pertenece?\r\n\t\t\t- Google: Go\r\n\t\t\t- Microsoft: C#\r\n\t\t\t- Oracle: Java\r\n\t\t- Logico: Prolog\r" :: Oraculo