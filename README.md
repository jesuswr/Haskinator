# Haskinator

![](Haskinator.jpg)

Este archivo contiene una breve descripción de nuestra implementación del proyecto Haskinator.

Proyecto elaborado por:

- Jesus De Aguiar 15-10360
- Neil Villamizar 15-11523
- Jesus Wahrman   15-11540

Proyecto para CI3691 - Laboratorio de Lenguajes de Programación I

## Compilación y Ejecución

Para compilar es suficiente con ejecutar el comando `make`, el cual hará uso de ghc para realizar\
dicha función.
 
`$ make`

Para eliminar los archivos creados es suficiente ejecutar:

`$ make clean`

Este proceso generará el ejecutable `Haskinator` y un directorio con los archivos de objeto e \
interfaces que fueron generados al compilar.

## Estructura e implementación

El archivo `Oraculo.hs` contiene la estructura del tipo de dato Oraculo y del tipo de dato Opciones\
como fue planteado en el enunciado del proyecto. Ademá, contiene las funciones de construccion, \
acceso y modificación especificadas.La implementación de estas fue lograda haciendo uso \
principalmente de *Pattern Matching*, constructores y el las funciones de *Data.Map*. 

El archivo `Haskinator.hs` contiene la implementación del flujo principal del programa.

En este destacan las siguientes funciones:

La función `prediccion` es la implementacion del flujo principal de predicción. Esta función recibe\
el oráculo que representa el estado actual y realiza las interacciones con el usuario para realizar\
las predicciones o modificar el oráculo en caso de ser necesario.

Para entender el funcionamiento de este, podemos entender la representación de un oráculo como un\ 
árbol, donde las hojas son predicciones, el resto de los nodos representan preguntas y las \
transiciones entre los nodos están representadas por las opciones. 

`prediccion` funciona de la siguiente manera: en primer lugar, chequea si el oráculo actual es solo\
una predicción o una pregunta. En caso de ser una predicción, se llama a la función \
`resolverPrediccion`. Esta se encarga de interactuar con el usuario para preguntarle si la acepta o\
no. En caso de que no, interactua con el usuario para obtener más información y realizar una\
ramificación de esta predicción retornando el tipo de dato Pregunta que representa a esta bifurcación. 

En caso de ser una pregunta, se llama a la función `resolverPregunta` esta se encarga de recorrer\
el árbol desde la raíz (pregunta principal) hasta llegar a una hoja, pidiendo opciones al usuario\
para saber cuál es el siguiente nodo a considerar. En caso de que el usuario no acepte ninguna de\
las opciones, se actualiza la pregunta actual con la opción especificada por el usuario \
(actualizando su mapa de opciones) y se retorna esta pregunta actualizada. Por otro lado, si se\
alcanza una predicción, se repite el proceso mencionado anteriormente, donde se chequea si el \
usuario acepta o no la predicción y se genera una ramificación en caso de obtener una respuesta \
negativa. En cualquiera de estos dos últimos flujos donde ocurre una actualización a una pregunta,\
todos los antecesores a esta también son actualizados para que apunten a la nueva pregunta creada.

Otra de las funciones interesantes es la implementación de la pregunta crucial. Para resolver este\
problema, se implementó una búsqueda en profundidad (dfs) que calcula el ancestro común más bajo\
entre las predicciones ingresadas por el usuario.

En primer lugar, se creó el tipo de dato Resultado. Este representa el resultado de haber ejecutado\
la búsqueda a partir de un nodo determinado. La semántica de resultado es la siguiente:

- Resultado = Primero -> Se encontró la primera predicción en el árbol enraizado en el nodo actual.
- Resultado = Segundo -> Se encontró la segunda predicción en el árbol enraizado en el nodo actual.
- Resultado = Ninguno -> No se enecontró ninguna de las predicciones.
- Resultado = Ambos str -> Se encontraron ambas predicciones y la pregunta crucial es str.

De esta manera, el algoritmo empieza en un nodo determinado, llama a la misma función sobre todos\
sus hijos, y combina los resultados obtenidos. En caso de llegar a una predicción, se retornará el\
resultado como un Primero, Segundo o Ninguno, dependiendo si esta predicción es alguna o ninguna de\
las que se quieren buscar. En caso de estar en una pregunta y haber encontrado ambas predicciones\
entre sus hijos, se extrae la pregunta crucial y se retorna el resultado como un Ambos str. Esto se\
repite a lo largo de todo el árbol y al final se considera la respuesta de la raíz y se le entrega\
esta al usuario.

Por último, para las funcionalidades de cargar y persistir, simplemente se construyó el tipo\
Oraculo haciendo que este implementara las *typeclasses read* y *show*. Estas nos permiten traducir\
directamente la estructura del oráculo a un archivo y leerla sin ninguna implementación adicional.
