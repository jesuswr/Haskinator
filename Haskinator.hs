module Haskinator where

import System.Directory
import Oraculo




main :: IO()
main = do
  putStrLn "Bienvenido a Haskinator."
  runMainLoop Nothing

runMainLoop :: Maybe Oraculo -> IO()
runMainLoop orac = do
  print orac
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