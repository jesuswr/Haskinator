module Haskinator (main) where

import System.Directory
import Oraculo
--import ASCII_art
import qualified Data.Map as M


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
    "2" -> putStrLn "En progreso"
    "3" -> guardarOraculo orac
    "4" -> cargarOraculo orac
    "5" -> putStrLn "En progreso"
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
    runMainLoop (Just (read contenido))
  else do
    putStrLn "El archivo dado no existe."
    runMainLoop orac


-- Preunta crucial:
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