module Oraculo where

import Data.Map

data Oraculo = 
    Prediccion {prediccion :: String}
    | Pregunta {
                 pregunta :: String,
                 opciones :: Opciones
               }
    deriving(Read, Show)

type Opciones = Map String Oraculo

crearOraculo :: String -> Oraculo
crearOraculo pred = Prediccion pred