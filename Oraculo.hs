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

prediccion :: Oraculo -> String
prediccion orac = case orac of Prediccion s -> s
                               Pregunta _ _ -> error "El oraculo no es una predicción"

pregunta :: Oraculo -> String
pregunta orac = case orac of Pregunta s _ -> s
                             Prediccion _ -> error "El oraculo no es una pregunta"

opciones :: Oraculo -> Opciones
opciones orac = case orac of Pregunta _ m -> m
                             Prediccion _ -> error "El oráculo no es una pregunta"
 
respuesta :: Oraculo -> String -> Oraculo
respuesta orac preg = case orac of Pregunta _ m -> m M.! preg
                                   Prediccion _ -> error "El oráculo no es una pregunta"

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar pregs oracs preg = Pregunta {getPregunta = preg
                                      ,mapOpciones = M.fromList $ zip pregs oracs}
