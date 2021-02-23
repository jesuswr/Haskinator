module Oraculo (Oraculo, 
                Opciones, 
                crearOraculo,
                prediccion,
                pregunta,
                opciones,
                respuesta,
                ramificar
                ) where

import qualified Data.Map as M

data Oraculo = 
    Prediccion {getPrediccion :: String}
    | Pregunta {
                 getPregunta :: String,
                 mapOpciones :: Opciones
               }
    deriving(Read, Show)

type Opciones = M.Map String Oraculo

crearOraculo :: String -> Oraculo
crearOraculo pred = Prediccion pred

prediccion :: Oraculo ->String
prediccion = undefined

pregunta :: Oraculo ->String
pregunta = undefined

opciones :: Oraculo ->Opciones
opciones = undefined

respuesta :: Oraculo ->String ->Oraculo
respuesta = undefined

ramificar :: [String] ->[Oraculo] ->String ->Oraculo
ramificar = undefined

