## ----- Librerías que se van a usar (Insertar las que sean necesarias) ----- ##
library(kohonen)


## Carga de datos del .CSV
datosCSV <- read.csv('./Datos Muestra/Wholesale customers data.csv')

## Se eliminan las variables irrelevantes para el análisis. En este caso "Channel" y "Region"
datosCSV <- select(datosCSV, -c(Channel, Region)

## Se comprueban los datos almacenados en datosCSV para ver si se han borrado las variables deseadas. Borrar cuando se haga correctamente la comprobación.
summary(datosCSV)

## Nos aseguramos que se puedan repetir resultados en el CSV. 
set.seed(50)
## Se omiten los datos que venga nulos o vacíos en el CSV
datosCSV <- na.omit(datosCSV)

## Se realiza el escalado o estandarización de los datos --> Se debe estudiar más a fondo qué se hace aquí
datosEscalados <- scale(datosCSV)

## Se genera el mapa auto-organizado con los datos que han sido estandarizados. Probamos de momento con un grid de 10x10. Esto sirve para entrenarlo
## Si existen demasiados nodos vacíos, reducir las dimensiones. En caso de que los nodos se sombreen demasiado, aumentar dimensiones. Es cuestión de probar.
## Es necesario estudiar si es necesario separar las muestras en dos grupos. Uno empleado para realizar el entrenamiento y otro para el testing.
mapaAuto <- som(data = datosEscalados, grid = somgrid(10,10,"hexagonal"))

## Esto es una alternativa. Se ha de probar cuántas iteraciones se necesitan para entrenar el grid y ver si la solución converge.
mapaAuto2 <- som(data = datosEscalados, grid = somgrid(10,10,"hexagonal"), rlen = 100)

## Se plasma en una gráfica el mapa obtenido. Probar diferentes plots para ver cual es el más adecuado al problema. Se pueden usar varios y analizarlos.
## Estudiar temas de colores para representar mejor los sombreados.
plot(mapaAuto2, type = "count")
plot(mapaAuto2, shape = "straight")
plot(mapaAuto2, type = "changes")

## Podemos comprobar cuántos elementos existen en un nodo. Quizá nos venga bien para el análisis.
elementosNodo <- table(mapaAuto$unit.classif)
print(elementosNodo)

## Podemos comprobar los nodos que se han asignado a cada registro del csv
print(mapaAuto2$unit.classif) 

## Se puede generar una gráfica de distancias. Representa la distancia euclidiana entre los vectores de cada neurona con su vecina.
plot(mapaAuto2, type="dist.neighbours", shape = "straight")



