## Se establece el directorio donde se encuentra el CSV con los datos
setwd("C:/Users/Lydia Prado Ib��ez/Documents/Universidad/M�ster/Desarrollo de Sistemas Inteligentes/TrabajoClusteringSOM")
#setwd("")
## ----- Librerias que se van a usar (Insertar las que sean necesarias) ----- ##
library(kohonen)
library(dplyr)


## Carga de datos del .CSV
datosCSV <- read.csv('./Datos Muestra/Wholesale customers data.csv')

## Se eliminan las variables irrelevantes para el analisis. En este caso "Channel" y "Region"
datosCSV <- select(datosCSV, -c(Channel, Region))

## Se comprueban los datos almacenados en datosCSV para ver si se han borrado las variables deseadas. Borrar cuando se haga correctamente la comprobacion.
summary(datosCSV)

## Se inicia de forma aleatoria. 
#set.seed(1000)
## Se omiten los datos que venga nulos o vacios en el CSV
datosCSV <- na.omit(datosCSV)

## Se realiza el escalado o estandarizacion de los datos --> Se debe estudiar mas a fondo que se hace aqui
datosEscalados <- scale(datosCSV)
summary(datosEscalados)

## Se genera el mapa auto-organizado con los datos que han sido estandarizados. Probamos de momento con un grid de 10x10. Esto sirve para entrenarlo
## Si existen demasiados nodos vacios, reducir las dimensiones. En caso de que los nodos se sombreen demasiado, aumentar dimensiones. Es cuestión de probar.
## Es necesario estudiar si es necesario separar las muestras en dos grupos. Uno empleado para realizar el entrenamiento y otro para el testing.
## Esto es una alternativa. Se ha de probar cuántas iteraciones se necesitan para entrenar el grid y ver si la solucion converge.
mapaAuto2 <- som(datosEscalados, grid = somgrid(6,6,"hexagonal"), rlen = 1000)

## Se plasma en una grafica el mapa obtenido. Probar diferentes plots para ver cual es el mas adecuado al problema. Se pueden usar varios y analizarlos.
## Estudiar temas de colores para representar mejor los sombreados.
plot(mapaAuto2, type = "count")
plot(mapaAuto2, shape = "straight")
plot(mapaAuto2, type = "changes")

## Podemos comprobar cuántos elementos existen en un nodo. Quizá nos venga bien para el análisis.
elementosNodo <- table(mapaAuto2$unit.classif)
print(elementosNodo)

## Podemos comprobar los nodos que se han asignado a cada registro del csv
print(mapaAuto2$unit.classif) 

## Se puede generar una gráfica de distancias. Representa la distancia euclidiana entre los vectores de cada neurona con su vecina.
plot(mapaAuto2, type="dist.neighbours", shape = "straight")

## CLUSTERING CON SOM

## Realizar la matriz de distancia entre los nodos
distancia <- dist(getCodes(mapaAuto2,1))

## Se realiza el agrupamiento jerarquico con las distancias y los nodos y se dibuja el gr�fico
agrupamiento <- hclust(distancia,method="ward.D2",members=elementosNodo)
plot(agrupamiento,hang=-1,labels=F)

## Se prueba a hacer diversos grupos en base al resultado de la gr�fica (se ve un buen resultado con 4 o 5 grupos)
k <- 3
rect.hclust(agrupamiento,k)

## Se muestra a qu� cluster pertenece cada grupo
grupos <- cutree(agrupamiento, k)
print(grupos)

## Se muestran los clusters en el grafico
plot(mapaAuto2,type="mapping",bgcol=c("paleturquoise","#F5D0FA","springgreen")[grupos],shape = "straight")
add.cluster.boundaries(mapaAuto2,clustering=grupos)





