module Oraculo (Oraculo (..), 
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
    Prediccion { obtenerPrediccion :: String}
    | Pregunta {
                 obtenerPregunta :: String,
                 mapaOpciones :: Opciones
               }
    deriving(Read, Show, Eq)

type Opciones = M.Map String Oraculo

crearOraculo :: String -> Oraculo
crearOraculo pred = Prediccion pred

prediccion :: Oraculo -> String
prediccion oraculo = case oraculo of 
                      Prediccion s -> s
                      Pregunta _ _ -> error "El oraculo no es una predicción"

pregunta :: Oraculo -> String
pregunta oraculo = case oraculo of 
                    Pregunta s _ -> s
                    Prediccion _ -> error "El oraculo no es una pregunta"

opciones :: Oraculo -> Opciones
opciones oraculo = case oraculo of 
                    Pregunta _ m -> m
                    Prediccion _ -> error "El oráculo no es una pregunta"
 
respuesta :: Oraculo -> String -> Oraculo
respuesta oraculo preg = case oraculo of 
                          Pregunta _ m -> m M.! preg
                          Prediccion _ -> error "El oráculo no es una pregunta"

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar resps oracs preg = Pregunta {obtenerPregunta = preg
                                      ,mapaOpciones = M.fromList $ zip resps oracs}
