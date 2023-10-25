import Oraculo

main :: IO()
main = do
  -- Para probar la funcion readData que usa la instancia Read
  putStrLn "\ntest 1"
  print $ readData ["a\n\t- b: c\n\t\t- d: e\n\t- f: g\n\t\t- h: i"] 1

  putStrLn "\ntest 2"
  print $ readData ["Estas bien?\n\t- No: Estas enferma?\n\t\t- No: Comiste bien\n\t\t- Si: Comiste mal"] 1

  putStrLn "\ntest 3"
  print $ readData ["Prediccion directa"] 1

  -- Para probar la instancia read directamente, realizar en consola
  -- cargando el modulo al interprete.
  -- ghci Oraculo.hs
  -- >> import Oraculo
  -- >> read "a\n\t- b: c\n\t\t- d: e\n\t- f: g\n\t\t- h: i" :: Oraculo
  -- >> read "hola?\n\t- No: Ayer?\n\t\t- No: tomate\n\t\t- Si: Paracetamol" :: Oraculo
  -- >> read "hola" :: Oraculo