module Haskinator (main) where

import System.Directory
import Oraculo
--import ASCII_art
import qualified Data.Map as M
import qualified Data.List as L
import Data.Char

main :: IO()
main = do
  putStrLn "Bienvenido a Haskinator."
  runMainLoop Nothing

runMainLoop :: Maybe Oraculo -> IO()
runMainLoop orac = do
  --putStr haskinator_ascii_art_smallest
  --print orac
  printOptions
  option <- getLine
  case option of
    "1" -> nuevoOraculo
    "2" -> predecir orac
    "3" -> guardarOraculo orac
    "4" -> cargarOraculo orac
    "5" -> consultarPreguntaCrucial orac
    "6" -> return()
    _   -> do
      putStrLn "Opcion no valida."
      runMainLoop orac

printOptions :: IO()
printOptions = do
  putStrLn "¿Que desea hacer?"
  putStrLn "1) Crear un oraculo nuevo."
  putStrLn "2) Predecir."
  putStrLn "3) Persistir."
  putStrLn "4) Cargar."
  putStrLn "5) Consultar pregunta crucial."
  putStrLn "6) Salir."

nuevoOraculo :: IO()
nuevoOraculo = do
  putStrLn "Diga su prediccion."
  pred <- getLine
  runMainLoop $ Just (crearOraculo pred)

guardarOraculo :: Maybe Oraculo -> IO()
guardarOraculo (Just orac) = do
  putStrLn "Diga el nombre del archivo para guardar."
  nombre <- getLine
  writeFile nombre (show orac)
  runMainLoop (Just orac)
guardarOraculo Nothing = do
  putStrLn "El oraculo actual esta vacio."
  runMainLoop Nothing

cargarOraculo :: Maybe Oraculo -> IO()
cargarOraculo orac = do
  putStrLn "Diga el nombre del archivo a cargar."
  nombre <- getLine
  existe <- doesFileExist nombre
  if existe then do
    contenido <- readFile nombre
    runMainLoop $ Just $ read contenido
  else do
    putStrLn "El archivo dado no existe."
    runMainLoop orac


-- Flujo principal de prediccion
predecir :: Maybe Oraculo -> IO ()
predecir oraculo = case oraculo of
  Nothing -> putStrLn "Consulta invalida: oraculo vacio. Cree un oraculo nuevo."
  Just orac -> case orac of 
    Pregunta _ _ -> resolverPregunta orac
    Prediccion s -> do                          -- Unico nodo de prediccion
      putStrLn $ s ++ "\nSi / No"
      resp <- getLine
      case map toLower resp of
        "si" -> runMainLoop oraculo
        "no" -> do
          nuevaPregunta <- resolverPrediccion orac
          runMainLoop $ Just nuevaPregunta
        _    -> do
          putStrLn "Opcion incorrecta"
          predecir oraculo

resolverPregunta :: Oraculo -> IO()
resolverPregunta oraculo = do
  let preg = pregunta oraculo
  let ops  = opciones oraculo
  putStrLn $ preg ++ "\n" ++ (L.intercalate " / " $ M.keys ops)
  resp <- getLine
  let evaluarRespuesta  | M.member resp ops = case respuesta oraculo resp of
                          Pregunta _ _ -> resolverPregunta $ respuesta oraculo resp
                          Prediccion pred -> do
                            putStrLn $ pred ++ "\nSi / No"
                            resultado <- getLine
                            case map toLower resultado of
                              "si" -> return ()
                              "no" -> do                            
                                nuevaPregunta <- resolverPrediccion $ oraculo
                                return $ M.insert resp nuevaPregunta ops
                                return ()
                              _ -> do 
                                putStrLn "Opcion incorrecta"
                                resolverPregunta oraculo
                        | map toLower resp == "ninguna" = do
                            putStrLn "He fallado! Cual era la respuesta correcta?"
                            nuevaPred <- getLine
                            putStrLn preg
                            nuevaOpcion <- getLine
                            return $ M.insert nuevaOpcion (crearOraculo nuevaPred) ops
                            return ()
                        | otherwise = do
                          putStrLn "Entrada no permitida"
                          resolverPregunta oraculo
  evaluarRespuesta

resolverPrediccion ::  Oraculo -> IO Oraculo
resolverPrediccion oraculo = do
  putStrLn "He fallado! Cual era la respuesta correcta?"
  nuevaOpcion <- getLine
  putStrLn $ "Que pregunta distingue a " ++ nuevaOpcion ++ " de las otras opciones?"
  nuevaPreg <- getLine
  putStrLn $ "Cual es la respuesta a \"" ++ nuevaPreg ++ "\" para " ++ nuevaOpcion ++ "?"
  respuesta1 <- getLine
  putStrLn $ "Cual es la respuesta a \"" ++ nuevaPreg ++ "\" para " ++ (prediccion oraculo) ++ "?"
  respuesta2 <- getLine
  return $ ramificar [respuesta1, respuesta2] [crearOraculo nuevaOpcion, oraculo] nuevaPreg


-- Preunta crucial:

consultarPreguntaCrucial :: Maybe Oraculo -> IO ()
consultarPreguntaCrucial orac = do
  putStrLn "Ingrese una prediccion presente en el oraculo actual:"
  prediccion1 <- getLine
  putStrLn "Ingrese otra prediccion presente en el oraculo actual:"
  prediccion2 <- getLine
  preguntaCrucial orac prediccion1 prediccion2
  runMainLoop orac

-- LCA entre dos nodos prediccion
preguntaCrucial :: Maybe Oraculo -> String -> String -> IO ()
preguntaCrucial Nothing _ _= putStrLn "Consulta invalida: oraculo vacio. Cree un oraculo nuevo."
preguntaCrucial (Just orac) prediccion1 prediccion2
  | prediccion1 == prediccion2 = putStrLn "Consulta invalida: las predicciones deben ser distintas."
  | otherwise                  = case dfsLca prediccion1 prediccion2 orac of
    Nothing -> putStrLn "Consulta invalida: ambas predicciones deben pertenecer al oraculo."
    (Just (pregunta, _)) -> do
      putStrLn "La pregunta crucial para las predicciones dadas es:"
      putStrLn pregunta

-- Encuentra el LCA de dos nodos con dfs
dfsLca :: String -> String  -> Oraculo -> Maybe (String, Int)

dfsLca pred1 _ (Prediccion pred)
  | pred == pred1 = Just ("",0)
  | otherwise     = Nothing

dfsLca pred1 pred2 oraculo@(Pregunta preg opciones) =
  do
    let resultados = 
                take 1 $ dropWhile (==Nothing) $ map aux (M.toList opciones)

    if null resultados
      then Nothing
      else case head resultados of
        Just ((s,1),_) -> Just (s,1)
        Just ((s,0),bloq) -> if dfsAux pred2 (newOrac bloq oraculo)
          then Just (preg,1)
          else Just (s,0)

  where
    aux (ss, orac) = case dfsLca pred1 pred2 orac of
      Just (s,n) -> Just ((s,n), (ss, orac))
      otherwise  -> Nothing

    newOrac bloqueado (Pregunta s opciones) =
      (Pregunta s (M.fromList $ filter ((/=bloqueado)) (M.toList opciones)))


dfsAux :: String -> Oraculo -> Bool

dfsAux pred (Prediccion pred_)
  | pred == pred_ = True
  | otherwise     = False

dfsAux pred (Pregunta _ opciones) = 
  or $ map (dfsAux pred . snd) (M.toList opciones)
