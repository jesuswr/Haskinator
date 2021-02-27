module Haskinator (main) where

import System.Directory
import Oraculo
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Char

-- 
-- Función Main. Despliega el logo y llama al ciclo principal
-- 
main :: IO()
main = do
  putStrLn "Bienvenido a"
  putStrLn haskinator_logo
  cicloPrincipal Nothing

--
-- Ciclo principal. Encargado de orquestrar las distintas funcionalidades. 
--
cicloPrincipal :: Maybe Oraculo -> IO()
cicloPrincipal oraculo = do
  imprimirOpciones
  opcion <- getLine
  case opcion of
    "1" -> nuevoOraculo
    "2" -> predecir oraculo
    "3" -> guardarOraculo oraculo
    "4" -> cargarOraculo oraculo
    "5" -> consultarPreguntaCrucial oraculo
    "6" -> return()
    _   -> do
      putStrLn "Opción inválida."
      cicloPrincipal oraculo 

--
-- Función que despliega las opciones de funcionalidades que pueden ser 
-- ejecutadas.
--
imprimirOpciones :: IO()
imprimirOpciones = do
  putStrLn "\n¿Qué desea hacer?"
  putStrLn "1) Crear un oráculo nuevo."
  putStrLn "2) Predecir."
  putStrLn "3) Persistir."
  putStrLn "4) Cargar."
  putStrLn "5) Consultar pregunta crucial."
  putStrLn "6) Salir."

--
-- Función para generar la prediccion inicial. 
--
nuevoOraculo :: IO()
nuevoOraculo = do
  putStrLn "Ingrese su predicción."
  pred <- getLine
  cicloPrincipal $ Just (crearOraculo pred)

--
-- Función para guardar el estado actual del oráculo en un archivo 
-- especificado.
--
guardarOraculo :: Maybe Oraculo -> IO()
guardarOraculo Nothing = do
  putStrLn "El oráculo actual está vacío."
  cicloPrincipal Nothing
guardarOraculo (Just oraculo) = do
  putStrLn "Ingrese el nombre del archivo para guardar."
  nombre <- getLine
  writeFile nombre (show oraculo)
  cicloPrincipal (Just oraculo)

--
-- Función para cargar el estado de un oráculo desde un archivo especificado.
--
cargarOraculo :: Maybe Oraculo -> IO()
cargarOraculo oraculo = do
  putStrLn "Ingrese el nombre del archivo a cargar."
  nombre <- getLine
  existe <- doesFileExist nombre
  if existe then do
    contenido <- readFile nombre
    cicloPrincipal $ Just $ read contenido
  else do
    putStrLn "El archivo especificado no existe."
    cicloPrincipal oraculo

--
-- Función que implemente el flujo principal de predicción
--
predecir :: Maybe Oraculo -> IO ()
predecir oraculo = case oraculo of
  Nothing -> do
    -- Oráculo no creado	    
    putStrLn "Consulta inválida: oráculo vacío. Cree un oráculo nuevo."
    cicloPrincipal oraculo
  Just orac -> case orac of 
    -- Raíz del oráculo es una pregunta
    Pregunta _ _ -> do
      resultado <- resolverPregunta orac
      case resultado of
        Just nuevoOraculo -> cicloPrincipal $ Just nuevoOraculo
        Nothing           -> cicloPrincipal $ Just orac
    -- Raíz del oráculo es una predicción
    Prediccion s -> do
      resultado <- resolverPrediccion orac
      case resultado of
        Just nuevoOraculo -> cicloPrincipal $ Just nuevoOraculo
        Nothing           -> cicloPrincipal $ Just orac

--
-- Flujo principal de predicción cuando la raíz del árbol que se está 
-- considerando actualmente es una pregunta. 
--
resolverPregunta :: Oraculo -> IO (Maybe Oraculo)
resolverPregunta oraculo = do
  let preg = pregunta oraculo
  let ops  = opciones oraculo
  putStrLn $ preg ++ "\n" ++ (List.intercalate " / " $ Map.keys ops)
  resp <- getLine
  if (Map.member resp ops) then do
    case respuesta oraculo resp of
      Pregunta _ _    -> do 
        resultado <- resolverPregunta $ respuesta oraculo resp
        case resultado of
          Just nuevoOrac ->
            return $ Just $ Pregunta preg (Map.insert resp nuevoOrac ops)
          Nothing        ->
            return Nothing
      Prediccion pred -> do
        resultado <- resolverPrediccion $ Prediccion pred
        case resultado of
          Just nuevoOrac ->
            return $ Just $ Pregunta preg $ Map.insert resp nuevoOrac ops
          Nothing        -> 
            return Nothing
  else if map toLower resp == "ninguna" then do
    putStrLn "¡He fallado! ¿Cuál era la respuesta correcta?"
    nuevaPred <- getLine
    putStrLn preg
    nuevaOpcion <- getLine
    return $ Just $ Pregunta preg $ Map.insert nuevaOpcion (crearOraculo nuevaPred) ops
  else do
    putStrLn "Entrada inválida"
    resolverPregunta oraculo

--
-- Función que será llamada en caso que una predicción sea rechazada. La función
-- pedirá al usuario los datos de ramificación y retornará el oráculo pregunta
-- que corresponde a esta.
--
resolverPrediccion ::  Oraculo -> IO (Maybe Oraculo)
resolverPrediccion orac@(Prediccion s) = do
  putStrLn $ s ++ "\nSi / No"
  resp <- getLine
  case map toLower resp of
    "si" -> return Nothing
    "no" -> do
      putStrLn "¡He fallado! ¿Cuál era la respuesta correcta?"
      nuevaOpcion <- getLine
      putStrLn $ "¿Qué pregunta distingue a " ++ nuevaOpcion ++ " de las otras opciones?"
      nuevaPreg <- getLine
      putStrLn $ "¿Cuál es la respuesta a \"" ++ nuevaPreg ++ "\" para " ++ nuevaOpcion ++ "?"
      respuesta1 <- getLine
      putStrLn $ "¿Cuál es la respuesta a \"" ++ nuevaPreg ++ "\" para " ++ s ++ "?"
      respuesta2 <- getLine
      return $ Just $ ramificar [respuesta1, respuesta2] [crearOraculo nuevaOpcion, orac] nuevaPreg
    _    -> do
      putStrLn "Entrada inválida"
      resolverPrediccion orac


--
-- Implementación de la pregunta crucial.
--

--
-- Obteniendo entrada del usuario y llamando a la función principal
--
consultarPreguntaCrucial :: Maybe Oraculo -> IO ()
consultarPreguntaCrucial orac = do
  putStrLn "Ingrese una predicción presente en el oráculo actual:"
  prediccion1 <- getLine
  putStrLn "Ingrese otra predicción presente en el oráculo actual:"
  prediccion2 <- getLine
  preguntaCrucial orac prediccion1 prediccion2
  cicloPrincipal orac


--
-- Tipo de dato para representar el resultado obtenido al recorrer el arbol de oráculo
-- en búsqueda de las predicciones.
-- 
-- El valor "Primero" representa que solo se encontró la primera predicción
-- El valor "Segundo" representa que solo se encontró la segunda predicción
-- El valor "Ambos str" representa que se encontraron ambas predicciones y la pregunta
-- crucial (el ancestro común más bajo) es el string str
-- El valor "Ninguno" representa que no se encontró ninguna de las dos predicciones.
--
data Resultado = Primero | Segundo | Ambos String | Ninguno

--
-- Función para combinar dos resultados
--
combinar :: String -> Resultado -> Resultado -> Resultado 
combinar _ r1@(Ambos s) _  = r1
combinar _ _ r2@(Ambos s)  = r2
combinar _ r1 Ninguno      = r1
combinar _ Ninguno r2      = r2
combinar s Primero Segundo = Ambos s
combinar s Segundo Primero = Ambos s


--
-- Recibe las predicciones a buscar, evalua que sea una consulta válida
-- y llama a la función dfsLca para buscar un resultado. En caso de encontrarlo,
-- imprime la pregunta crucial.
--
preguntaCrucial :: Maybe Oraculo -> String -> String -> IO ()
preguntaCrucial Nothing _ _= 
  putStrLn "Consulta invalida: oráculo vacío. Cree un oráculo nuevo."
preguntaCrucial (Just orac) prediccion1 prediccion2
  | prediccion1 == prediccion2 = 
    putStrLn "Consulta inválida: las predicciones deben ser distintas."
  | otherwise                  = 
    case dfsLca prediccion1 prediccion2 orac of
      Ambos str -> do
        putStrLn "La pregunta crucial para las predicciones dadas es:"
        putStrLn str
      otherwise -> 
        putStrLn "Consulta inválida: ambas predicciones deben pertenecer al oráculo"


--
-- Implementación de dfs para encontrar el ancestro común más bajo (LCA)
-- entre las dos predicciones
--
dfsLca :: String -> String -> Oraculo -> Resultado
dfsLca p1 p2 (Prediccion pred)
  | p1 == pred = Primero
  | p2 == pred = Segundo
  | otherwise  = Ninguno
dfsLca p1 p2 oraculo@(Pregunta preg ops) = 
  foldl1 (combinar preg) $ map (dfsLca p1 p2) $ Map.elems ops

-------------------------------------------------------------------------------------
--
-- Logo
--
haskinator_logo :: String
haskinator_logo =
  "    __  _____   _____ __ __ _____   _____  __________  ____  __\n\
    \   / / / /   | / ___// //_//  _/ | / /   |/_  __/ __ \\/ __ \\/ /\n\
    \  / /_/ / /| | \\__ \\/ ,<   / //  |/ / /| | / / / / / / /_/ / / \n\
    \ / __  / ___ |___/ / /| |_/ // /|  / ___ |/ / / /_/ / _, _/_/  \n\
    \/_/ /_/_/  |_/____/_/ |_/___/_/ |_/_/  |_/_/  \\____/_/ |_(_)   \n"
