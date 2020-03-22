## Se establece el directorio donde se encuentra el CSV con los datos
## setwd("C:/Users/Lydia Prado Ibáñez/Documents/Universidad/Máster/Desarrollo de Sistemas Inteligentes/TrabajoClusteringSOM")
setwd("C:/Users/ruben/Desktop/TrabajoClusteringSOM")
## ----- Librerias que se van a usar (Insertar las que sean necesarias) ----- ##
library(kohonen)
library(dplyr)


## Carga de datos del .CSV
datosOriginalesCSV <- read.csv('./Datos Muestra/Wholesale customers data.csv')


## Se eliminan las variables irrelevantes para el analisis. En este caso "Channel" y "Region"
datosCSV <- select(datosOriginalesCSV, -c(Channel, Region))

## Se comprueban los datos almacenados en datosCSV para ver si se han borrado las variables deseadas. Borrar cuando se haga correctamente la comprobacion.
summary(datosCSV)


### --- Funcionalidad para comprobar si se cumple el Principio de Pareto --- ###

## Número de filas totales que contiene el CSV
numeroFilas = nrow(datosCSV)
## Numero de filas que representan el 20% de las filas totales (clientes)
veintePorcienClientes = numeroFilas * 0.2
## Suma de cada elemento en cada fila del csv
arraySumaFilas = rowSums(datosCSV, na.rm=FALSE, dims = 1)
## Suma total de filas
sumaTotalFilas = sum(arraySumaFilas, na.rm = FALSE, dims = 1)
## Ordenación de menor a mayor de cada fila
sumaFilasOrd = sort(arraySumaFilas, decreasing = TRUE)

## Representación del 80% de ganancias,
ventePorcienTotal = sumaTotalFilas * 0.8
## Suma del 20% de clientes
sumaVeinte = sum(sumaFilasOrd[1:veintePorcienClientes])

### -- Se puede concluir que la regla no se cumple -- ###


## Se inicia de forma aleatoria. 
#set.seed(1000)
## Se omiten los datos que venga nulos o vacios en el CSV
datosCSV <- na.omit(datosCSV)

## Se realiza el escalado o estandarizacion de los datos --> Se debe estudiar mas a fondo que se hace aqui
datosEscalados <- scale(datosCSV)
summary(datosEscalados)

## Se genera el mapa auto-organizado con los datos que han sido estandarizados. Probamos de momento con un grid de 10x10. Esto sirve para entrenarlo
## Si existen demasiados nodos vacios, reducir las dimensiones. En caso de que los nodos se sombreen demasiado, aumentar dimensiones. Es cuestiÃ³n de probar.
## Es necesario estudiar si es necesario separar las muestras en dos grupos. Uno empleado para realizar el entrenamiento y otro para el testing.
## Esto es una alternativa. Se ha de probar cuÃ¡ntas iteraciones se necesitan para entrenar el grid y ver si la solucion converge.
mapaAuto2 <- som(datosEscalados, grid = somgrid(6,6,"hexagonal"), rlen = 1000)

## Se plasma en una grafica el mapa obtenido. Probar diferentes plots para ver cual es el mas adecuado al problema. Se pueden usar varios y analizarlos.
## Estudiar temas de colores para representar mejor los sombreados.
plot(mapaAuto2, type = "count")
plot(mapaAuto2, shape = "straight")
plot(mapaAuto2, type = "changes")

## Podemos comprobar cuantos elementos existen en un nodo. QuizÃ¡ nos venga bien para el anÃ¡lisis.
elementosNodo <- table(mapaAuto2$unit.classif)
print(elementosNodo)

## Podemos comprobar los nodos que se han asignado a cada registro del csv
print(mapaAuto2$unit.classif) 

## Se puede generar una grafica de distancias. Representa la distancia euclidiana entre los vectores de cada neurona con su vecina.
## Se obtienen los Outliers
plot(mapaAuto2, type="dist.neighbours", shape = "straight")

## CLUSTERING CON SOM

## Realizar la matriz de distancia entre los nodos
distancia <- dist(getCodes(mapaAuto2,1))

## Se realiza el agrupamiento jerarquico con las distancias y los nodos y se dibuja el gráfico
agrupamiento <- hclust(distancia,method="ward.D2",members=elementosNodo)
plot(agrupamiento,hang=-1,labels=F)

## Se prueba a hacer diversos grupos en base al resultado de la gráfica (se ve un buen resultado con 4 o 5 grupos)
k <- 4
rect.hclust(agrupamiento,k)

## Se muestra a qué cluster pertenece cada grupo
grupos <- cutree(agrupamiento, k)
print(grupos)

## Se muestran los clusters en el grafico
plot(mapaAuto2,type="mapping",bgcol=c("paleturquoise","#F5D0FA","springgreen")[grupos],shape = "straight")
add.cluster.boundaries(mapaAuto2,clustering=grupos)


## CLUSTERING CON SOM sin Outliers


## Carga de csv sin Outliers
datosCSVSinOutliers <- read.csv('./Datos Muestra/Wholesale customers data Sin Outliers.csv')
datosCSVSinOutliers <- select(datosCSVSinOutliers, -c(Channel, Region))
summary(datosCSVSinOutliers)

datosEscaladosOutliers <- scale(datosCSVSinOutliers)

mapaAutoOutliers <- som(datosEscaladosOutliers, grid = somgrid(6,6,"hexagonal"), rlen = 1000)
plot(mapaAutoOutliers, type = "count")
plot(mapaAutoOutliers, shape = "straight")
plot(mapaAutoOutliers, type = "changes")

elementosNodoOutliers <- table(mapaAutoOutliers$unit.classif)

## Realizar la matriz de distancia entre los nodos
distanciaOutliers <- dist(getCodes(mapaAutoOutliers,1))

## Se realiza el agrupamiento jerarquico con las distancias y los nodos y se dibuja el gráfico
agrupamientoOutliers <- hclust(distanciaOutliers,method="ward.D2",members=elementosNodoOutliers)
plot(agrupamientoOutliers,hang=-1,labels=F)

## Se prueba a hacer diversos grupos en base al resultado de la gráfica (se ve un buen resultado con 4 o 5 grupos)
k <- 1
rect.hclust(agrupamientoOutliers,k)

## Se muestra a qué cluster pertenece cada grupo
gruposOutliers <- cutree(agrupamientoOutliers, k)
print(gruposOutliers)

## Se muestran los clusters en el grafico
plot(mapaAutoOutliers,type="mapping",bgcol=c("paleturquoise","#F5D0FA","springgreen")[gruposOutliers],shape = "straight")
add.cluster.boundaries(mapaAutoOutliers,clustering=gruposOutliers)

## Cálculo de media para establecer el representante del grupo obtenido
mediaRepresentante <- mean(arraySumaFilas)
print(mediaRepresentante)

### --- DATOS POR REGION --- ###

## Filtrado de datos en funcion de las variables cualitativas de Region 
datosLisboa <- select(filter(datosOriginalesCSV, Region == 1), -c(Channel, Region))
datosOporto <- select(filter(datosOriginalesCSV, Region == 2), -c(Channel, Region))
datosOtros <- select(filter(datosOriginalesCSV, Region == 3), -c(Channel, Region))

## Ingresos totales por Region
sumaIngresosLisboa <- sum(rowSums(datosLisboa, na.rm=FALSE, dims = 1))
sumaIngresosOporto <- sum(rowSums(datosOporto, na.rm=FALSE, dims = 1))
sumaIngresosOtros <- sum(rowSums(datosOtros, na.rm=FALSE, dims = 1))

## Numero de clientes por Region
clientesLisboa <- nrow(datosLisboa)
clientesOporto <- nrow(datosOporto)
clientesOtros <- nrow(datosOtros)

## Ingresos medios por Region
ingresosMediosLisboa <- sumaIngresosLisboa/clientesLisboa
ingresosMediosOporto <- sumaIngresosOporto/clientesOporto
ingresosMediosOtros <- sumaIngresosOtros/clientesOtros


### --- DATOS POR CHANNEL --- ###

## Filtrado de datos en funcion de las variables cualitativas de Channel
datosMinorista <- select(filter(datosOriginalesCSV, Channel == 1), -c(Channel, Region))
datosHosteleria <- select(filter(datosOriginalesCSV, Channel == 2), -c(Channel, Region))

## Ingresos totales por Channel
sumaIngresosMinorista <- sum(rowSums(datosMinorista, na.rm=FALSE, dims = 1))
sumaIngresosHosteleria <- sum(rowSums(datosHosteleria, na.rm=FALSE, dims = 1))

## Numero de clientes por Channel
clientesMinorista <- nrow(datosMinorista)
clientesHosteleria <- nrow(datosHosteleria)

## Ingresos medios por Channel
ingresosMediosMinorista <- sumaIngresosMinorista/clientesMinorista
ingresosMediosHosteleria <- sumaIngresosHosteleria/clientesHosteleria



