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
  let oraculo = read "Prediccion directa" :: Oraculo
  print oraculo