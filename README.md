# **Haskinator**
### CI-3641: Lenguajes de Programación I
### Septiembre - Diciembre 2023
#### Integrantes:  
Carlos Sivira, 15-11377  
Daniel Figueroa, 16-10371  
Junior Lara, 17-10303  
Astrid Alvarado, 18-10938
## **Ejecución**
Para clonar el repositorio, ejecute la siguiente línea de código
```bash
$ git clone https://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@github.com/JMLTUnderCode/Proyecto_1_LLP1_Haskinator.git
```
### Requisitos para la ejecución
#### Instalación de la librería `Data.List.Split`
Para esto, ejecute la siguiente línea de código:
```bash
$ cabal install split
``` 
#### Versión de Haskell
Para este proyecto se utilizaron las siguientes versiones
- `GHC 9.2.7`
- `GHC 9.2.8`
- `GHC 9.4.7`
- `GHC 9.6.3`  

Por lo tanto, a partir de la versión **9.2.7** de GHC este proyecto puede ser ejecutado.
### Modo de ejecución
```bash
$ mkdir -p build
$ ghc -odir build -hidir build -o build/haskinator haskinator.hs
$ ./build/haskinator
```

O en un solo comando:

```bash
$ mkdir -p build && ghc -odir build -hidir build -o build/haskinator haskinator.hs && ./build/haskinator
```
## **Detalles de Implementación**
### Oraculo.hs
- #### Tipo de datos
    - #### Oraculo  
        Este tipo de datos se define como:  
        ```data Oraculo = Prediccion String | Pregunta String Opciones```  
        Donde `Prediccion` almacena una cadena de caracteres con la predicción correspondiente, en cambio, `Pregunta` sirve para almacenar la pregunta a realizar por _Haskinator_ y las diferentes opciones disponibles para responder a dicha pregunta.
        - #### Instancias
            - #### `Show`
                Para la instancia de `Show`, se empleó la función auxiliar `showData :: Oraculo -> Int -> String`, el cual genera una cadena de caracteres a partir de un Oraculo dado y la profundidad actual del árbol. Por ejemplo, al realizar
                ```haskell
                show $ Pregunta "a?" (Map.fromList [("b", Pregunta "c?" (Map.fromList [("d", Prediccion "e")])),
                          ("f", Pregunta "g?" (Map.fromList [("h", Prediccion "i")]))])
                ```
                Se obtiene la siguiente cadena:
                ```
                a?
                    - b: c?
                        - d: e
                    - f: g?
                        - h: i
                ```
                Esto se logra al recorrer el árbol generado de Oraculo, donde la tabulación representa el nivel de profundidad alcanzado.
            - #### `Read`
                Para la instancia de `Read`, se empleó la función auxiliar `readData :: [String] -> Int -> Oraculo`, la cual a partir de una lista de cadenas de caracteres y la profundidad recorrida en el árbol, retorna un Oraculo con los datos suministrados de la cadena de caracteres. Por ejemplo, al realizar:
                ```haskell
                read "a?\n\t- b: c?\n\t\t- d: e\n\t- f: g?\n\t\t- h: i" :: Oraculo
                ```
                Se obtiene el siguiente resultado
                ```haskell
                Pregunta "a?" (Map.fromList [("b", Pregunta "c?" (Map.fromList [("d", Prediccion "e")])),
                          ("f", Pregunta "g?" (Map.fromList [("h", Prediccion "i")]))])
                ```
                Esto se logra a través de la cantidad de tabulaciones, donde la tabulación representa el nivel de profundidad alcanzado y por ende su ubicación correspondiente.
            - #### `Eq`
                Para poder realizar comparaciones en el tipo `Oraculo`, se realizó una derivación de la instacia `Eq`
    - #### Opciones
        Este tipo de datos se define como:  
        ```type Opciones = Map.Map String Oraculo```  
        Esto es, un diccionario cuyas claves son del tipo `String`, las cuales representa las diferentes opciones a contestar. Los valores asociados a las claves son del tipo `Oraculo`, por lo tanto para una opción seleccionada se tiene que puede llevar a una prediccion o una nueva pregunta.
- #### Funciones de Construcción
    - `crearOraculo :: String -> Oraculo`  
        Función que dado una cadena de caracteres, genera un Oraculo de tipo `Prediccion`
    - `ramificar :: [String] -> [Oraculo] -> String -> Oraculo`  
        Función implementada mediante el uso de las funciones `zip` y `map`, permitiendo construir los elementos que conforman un Oráculo de tipo `Pregunta` con la lista de cadenas de caracteres (opciones) dados con la lista de Oráculos (bien sea `Pregunta` o `Prediccion`).
- #### Funciones de Acceso
    - `prediccion :: Oraculo -> String`  
        Dado un `Oraculo`, retorna la cadena de caracteres asociada al tipo `Prediccion`, en caso de recibir el tipo `Pregunta`, se arroja un error
    - `pregunta :: Oraculo -> String`  
        Dado un `Oraculo`, retorna la cadena de caracteres asociada al tipo `Pregunta`, en caso de recibir el tipo `Prediccion`, se arroja un error
    - `opciones :: Oraculo -> Opciones`  
        Dado un `Oraculo`, retorna las opciones asociadas al tipo `Pregunta`, en caso de recibir el tipo `Prediccion`, se arroja un error
    - `respuesta :: Oraculo -> String -> Oraculo`  
        Función implementada mediante el uso de la instrucción `case` para la extracción del Oraculo, donde si se encuentra `Just op`, se retorna `op` y en cambio, si se encuentra `Nothing` se reporta error, permitiendo realizar la extracción de las opciones vinculadas a una pregunta. En caso de recibir una `Prediccion`, se arroja error
- #### Funciones de Inspección
    - `obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]`  
       Función que permite obtener la cadena de preguntas y respuestas que llevan a una prediccion. Esta función es implementada con dos funciones auxiliares
        - `obtenerPredicciones :: Oraculo -> [String]`  
            Función que permite obtener las predicciones de un `Oraculo` como una lista de cadena de caracteres. Esto facilita la búsqueda de una predicción en el `Oraculo`.
        - `camino :: Oraculo -> String -> [(String, String)]`  
            Función que dado un `Oraculo` y una prediccion (como una cadena de caracteres), devuelve la cadena de preguntas y respuestas que llevan a dicha prediccion
    - `obtenerEstadisticas :: Oraculo -> (Float, Float, Float)`  
        Función que permite obtener las estadisticas de un `Oraculo` en forma de la tripleta `(mínimo, máximo, promedio)`
        - `profundidades :: Oraculo -> [Int]`  
            Función que dado un `Oraculo`, devuelve una lista con las profundidades de todas las predicciones.
        - `obtenerProfundidad :: Oraculo -> String -> Int`  
            Función que dado un `Oraculo` y una predicción (como una cadena de caracteres), devuelve la profundidad de dicha predicción
### Haskinator.hs
- #### Crear nuevo oraculo `(1)`
    Para esta funcionalidad se utilizó la función `crearOraculo` del módulo `Oraculo`. Esta opción es activada al ingresar el número `1`
- #### Predecir `(2)`
    Para esta funcionalidad se utilizó la función
    - `predecir :: Oraculo -> Oraculo -> String -> IO Oraculo`  
        Funcion que recorre el Oraculo para realizar la predicción, del mismo modo, se encarga de la actualización del mismo en caso de que no exista la opción o la predicción no sea acertada. Para esto se utilizan las siguientes funciones auxiliares
        - `actualizarOraculo :: (String, Oraculo) -> Oraculo -> Oraculo -> Oraculo`  
            Función para actualizar el Oraculo dado cuando la predicción no sea la acertada. En esta, una vez se encuentra la predicción a modificar, se agrega la nueva ramificación con las preguntas suministradas al usuario.
        - `actualizarOraculo' :: (String, Oraculo) -> String -> Oraculo -> Oraculo`
            Función para actualizar el Oraculo dado cuando la opción pensada por el usuario no se encuentra en las disponibles. En esta, una vez se encuentra la pregunta a modificar, se agrega la nueva opción con la predicción suministrada por el usuario.
        - `esPregunta :: Oraculo -> Bool`
            Función que indica si un Oraculo es de tipo `Pregunta`
    
    Esta opción es activada al ingresar el número `2`
- #### Persistir `(3)`
    - `persistirOraculo :: Oraculo -> String -> IO ()`  
        Función que, dado un Oraculo y el nombre del archivo, genera un archivo `.txt` con el oraculo actual. Para esta función, se hizo uso de la instancia `Show` creada y la herramienta correspondiente para la escritura de archivos, por lo tanto el formato del archivo generado es el siguiente:
        ```
        a?
            - b: c?
                - d: e
            - f: g?
                - h: i
        ```
            
    Para hacer uso de esta opción, se debe ingresar el número `3`

- #### Cargar `(4)`
    - `cargarOraculo :: String -> Oraculo`
        Función que, dado el nombre del archivo `.txt`, genera un oraculo con la información del archivo. Para esta función, se hizo uso de la instancia `Read` creada y la herramienta correspondiente para la lectura de archivos, por lo tanto el formato del archivo a ingresar es el siguiente:
        ```
        a?
            - b: c?
                - d: e
            - f: g?
                - h: i
        ```
        Es de suma importancia asegurarse de que el archivo esté bien identado, con un espacio justo después de guion (-) y los dos puntos (:) de cada opción.  
        Para esta función se utilizó las siguientes funciones auxiliares  
        - `hasRepeatPredictions :: Oraculo -> Bool`  
            Función que dado un Oraculo determina si hay o no predicciones repetidas. Esta función hace uso de
            - `hasDuplicates :: Eq a => [a] -> Bool`  
                Función genérica que determina si hay elementos repetidos en una lista de elementos comparables. Para esto utiliza la función `nub` que elimina elementos repetidos de una lista y verifica que la lista sea de distinto tamaño a la original.

    Para hacer uso de esta opción, se debe ingresar el número `4`
- #### Pregunta Crucial `(5)` 
    - `preguntaCrucial :: Oraculo -> String -> String -> Maybe (String, String, String)`  
        Función que dado un Oraculo y dos strings retorna el LCA (Lowest Common Ancestor) en el arbol informacion descrito por Oraculo. Esta función hace uso de
        - `caminoComun :: [(String, String)] -> [(String, String)] -> [(String, String)]`  
            Funcion que dadas dos cadenas (listas de preguntas hasta una predicción), devuelve el segmento común entre ellas partiendo desde el inicio de ambas listas.
    
    Para usar esta opción, se debe ingresar el número `5`
- #### Estadística `(6)`
    Para esta funcionalidad se utilizó la función `obtenerEstadisticas` del módulo `Oraculo`. Si se quiere hacer uso de la misma, se debe ingresar el número `6`
- #### Salir `(7)`
    Fin del programa. Para activar esta función, se debe ingresar el número `7`
