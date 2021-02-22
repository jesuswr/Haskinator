module Haskinator where

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
    "3" -> putStrLn "En progreso"
    "4" -> putStrLn "En progreso"
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