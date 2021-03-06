## Se establece el directorio donde se encuentra el CSV con los datos
## setwd("C:/Users/Lydia Prado Ib��ez/Documents/Universidad/M�ster/Desarrollo de Sistemas Inteligentes/TrabajoClusteringSOM")
 setwd("C:/Users/ruben/Desktop/TrabajoClusteringSOM")

## ----- Librerias que se van a usar (Insertar las que sean necesarias) ----- ##
library(kohonen)
library(dplyr)


## Carga de datos del .CSV
datosOriginalesCSV <- read.csv('./Datos Muestra/Wholesale customers data.csv')


## Se eliminan las variables irrelevantes para el analisis. En este caso "Channel" y "Region"
datosCSV <- select(datosOriginalesCSV, -c(Channel, Region))


### --- Funcionalidad para comprobar si se cumple el Principio de Pareto --- ###

## N�mero de filas totales que contiene el CSV
numeroFilas = nrow(datosCSV)
## Numero de filas que representan el 20% de las filas totales (clientes)
veintePorcienClientes = numeroFilas * 0.2
## Suma de cada elemento en cada fila del csv
arraySumaFilas = rowSums(datosCSV, na.rm=FALSE, dims = 1)
## Suma total de filas
sumaTotalFilas = sum(arraySumaFilas, na.rm = FALSE, dims = 1)
## Ordenaci�n de menor a mayor de cada fila
sumaFilasOrd = sort(arraySumaFilas, decreasing = TRUE)

## Representaci�n del 80% de ganancias,
ventePorcienTotal = sumaTotalFilas * 0.8
## Suma del 20% de clientes
sumaVeinte = sum(sumaFilasOrd[1:veintePorcienClientes])

### -- Se puede concluir que la regla no se cumple -- ###


## Se omiten los datos que venga nulos o vacios en el CSV
datosCSV <- na.omit(datosCSV)

## Se realiza el escalado o estandarizacion de los datos
datosEscalados <- scale(datosCSV)

## Se genera el mapa auto-organizado con los datos que han sido estandarizados.
mapaAuto <- som(datosEscalados, grid = somgrid(6,6,"hexagonal"), rlen = 1000)

## Se plasma en una grafica el mapa obtenido. Probar diferentes plots para ver cual es el mas adecuado al problema. Se pueden usar varios y analizarlos.
## Estudiar temas de colores para representar mejor los sombreados.
plot(mapaAuto, type = "count")
plot(mapaAuto, shape = "straight")
plot(mapaAuto, type = "changes")

## Podemos comprobar cuantos elementos existen en un nodo. Quizá nos venga bien para el análisis.
elementosNodo <- table(mapaAuto$unit.classif)

## Podemos comprobar los nodos que se han asignado a cada registro del csv
print(mapaAuto$unit.classif) 

## Se puede generar una grafica de distancias. Representa la distancia euclidea entre los vectores de cada neurona con su vecina.
## Se obtienen los Outliers
plot(mapaAuto, type="dist.neighbours", shape = "straight")


## CLUSTERING CON SOM

## Realiza la matriz de distancia entre los nodos
distancia <- dist(getCodes(mapaAuto,1))

## Se realiza el agrupamiento jerarquico con las distancias y los nodos y se dibuja el grafico
agrupamiento <- hclust(distancia,method="ward.D2",members=elementosNodo)
plot(agrupamiento,hang=-1,labels=F)

## Se prueba a hacer diversos grupos en base al resultado de la gr�fica (se ve un buen resultado con 4 o 5 grupos)
k <- 4
rect.hclust(agrupamiento,k)

## Se muestra a qu� cluster pertenece cada grupo
grupos <- cutree(agrupamiento, k)

## Se muestran los clusters en el grafico
plot(mapaAuto,type="mapping",bgcol=c("paleturquoise","#F5D0FA","springgreen")[grupos],shape = "straight")
add.cluster.boundaries(mapaAuto,clustering=grupos)



## CLUSTERING CON SOM sin Outliers


## Se realiza lo mismo que en el caso anterior
## Carga de csv sin Outliers
datosCSVSinOutliers <- read.csv('./Datos Muestra/Wholesale customers data Sin Outliers.csv')
datosCSVSinOutliers <- select(datosCSVSinOutliers, -c(Channel, Region))

datosEscaladosOutliers <- scale(datosCSVSinOutliers)

mapaAutoOutliers <- som(datosEscaladosOutliers, grid = somgrid(6,6,"hexagonal"), rlen = 1000)
plot(mapaAutoOutliers, type = "count")
plot(mapaAutoOutliers, shape = "straight")
plot(mapaAutoOutliers, type = "changes")

elementosNodoOutliers <- table(mapaAutoOutliers$unit.classif)

## Realizar la matriz de distancia entre los nodos
distanciaOutliers <- dist(getCodes(mapaAutoOutliers,1))

## Se realiza el agrupamiento jerarquico con las distancias y los nodos y se dibuja el gr�fico
agrupamientoOutliers <- hclust(distanciaOutliers,method="ward.D2",members=elementosNodoOutliers)
plot(agrupamientoOutliers,hang=-1,labels=F)

## Se prueba a hacer diversos grupos en base al resultado de la gr�fica. En este caso, el mejor resultado sale con k=1
k <- 1
rect.hclust(agrupamientoOutliers,k)

## Se muestra a qu� cluster pertenece cada grupo
gruposOutliers <- cutree(agrupamientoOutliers, k)
print(gruposOutliers)

## Se muestran los clusters en el grafico
plot(mapaAutoOutliers,type="mapping",bgcol=c("paleturquoise","#F5D0FA","springgreen")[gruposOutliers],shape = "straight")
add.cluster.boundaries(mapaAutoOutliers,clustering=gruposOutliers)


## --- Generacion de grupos a partir de la combinacion de los valores tomados por Region y Channel--- ##

datosGrupo1 <- select(filter(datosOriginalesCSV, Region == 1 & Channel == 1), -c(Channel, Region))
datosGrupo2 <- select(filter(datosOriginalesCSV, Region == 2 & Channel == 1), -c(Channel, Region))
datosGrupo3 <- select(filter(datosOriginalesCSV, Region == 3 & Channel == 1), -c(Channel, Region))
datosGrupo4 <- select(filter(datosOriginalesCSV, Region == 1 & Channel == 2), -c(Channel, Region))
datosGrupo5 <- select(filter(datosOriginalesCSV, Region == 2 & Channel == 2), -c(Channel, Region))
datosGrupo6 <- select(filter(datosOriginalesCSV, Region == 3 & Channel == 2), -c(Channel, Region))



## Ingresos totales por grupo
sumaIngresosGrupo1 <- sum(rowSums(datosGrupo1, na.rm=FALSE, dims = 1))
sumaIngresosGrupo2 <- sum(rowSums(datosGrupo2, na.rm=FALSE, dims = 1))
sumaIngresosGrupo3 <- sum(rowSums(datosGrupo3, na.rm=FALSE, dims = 1))
sumaIngresosGrupo4 <- sum(rowSums(datosGrupo4, na.rm=FALSE, dims = 1))
sumaIngresosGrupo5 <- sum(rowSums(datosGrupo5, na.rm=FALSE, dims = 1))
sumaIngresosGrupo6 <- sum(rowSums(datosGrupo6, na.rm=FALSE, dims = 1))

## Numero de clientes por grupo
clientesGrupo1 <- nrow(datosGrupo1)
clientesGrupo2 <- nrow(datosGrupo2)
clientesGrupo3 <- nrow(datosGrupo3)
clientesGrupo4 <- nrow(datosGrupo4)
clientesGrupo5 <- nrow(datosGrupo5)
clientesGrupo6 <- nrow(datosGrupo6)



## Medias totales por grupo --> Se establecen los Representantes
mediaIngresosGrupo1 <- mean(rowSums(datosGrupo1))
mediaIngresosGrupo2 <- mean(rowSums(datosGrupo2))
mediaIngresosGrupo3 <- mean(rowSums(datosGrupo3))
mediaIngresosGrupo4 <- mean(rowSums(datosGrupo4))
mediaIngresosGrupo5 <- mean(rowSums(datosGrupo5))
mediaIngresosGrupo6 <- mean(rowSums(datosGrupo6))



## datos para la variable Delicassen
grupo1Delicassen <- c(select(datosGrupo1, c(Delicassen)))
grupo2Delicassen <- c(select(datosGrupo2, c(Delicassen)))
grupo3Delicassen <- c(select(datosGrupo3, c(Delicassen)))
grupo4Delicassen <- c(select(datosGrupo4, c(Delicassen)))
grupo5Delicassen <- c(select(datosGrupo5, c(Delicassen)))
grupo6Delicassen <- c(select(datosGrupo6, c(Delicassen)))

# Test de Kruskal-Wallis para los ingresos de Delicassen de cada grupo
kruskal.test(list(grupo1Delicassen, grupo2Delicassen, grupo3Delicassen, grupo4Delicassen, grupo5Delicassen, grupo6Delicassen))

# Ejemplo de Prueba de combinaciones con pares de grupos que dan hipotesis nulas para Delicassen
wilcox.test(x = as.numeric(unlist(select(datosGrupo1, c(Delicassen)))), y = as.numeric(unlist(select(datosGrupo4, c(Delicassen)))), conf.int = 0.05)



## datos para la variable Fresh
grupo1Fresh <- c(select(datosGrupo1, c(Fresh)))
grupo2Fresh <- c(select(datosGrupo2, c(Fresh)))
grupo3Fresh <- c(select(datosGrupo3, c(Fresh)))
grupo4Fresh <- c(select(datosGrupo4, c(Fresh)))
grupo5Fresh <- c(select(datosGrupo5, c(Fresh)))
grupo6Fresh <- c(select(datosGrupo6, c(Fresh)))

# Test de Kruskal-Wallis para los ingresos de Fresh de cada grupo
kruskal.test(list(grupo1Fresh, grupo2Fresh, grupo3Fresh, grupo4Fresh, grupo5Fresh, grupo6Fresh))

# Ejemplo de Prueba de combinaciones con pares de grupos que dan hipotesis nulas para Fresh
wilcox.test(x = as.numeric(unlist(select(datosGrupo3, c(Fresh)))), y = as.numeric(unlist(select(datosGrupo6, c(Fresh)))), conf.int = 0.05)



## datos para la variable Milk
grupo1Milk <- c(select(datosGrupo1, c(Milk)))
grupo2Milk <- c(select(datosGrupo2, c(Milk)))
grupo3Milk <- c(select(datosGrupo3, c(Milk)))
grupo4Milk <- c(select(datosGrupo4, c(Milk)))
grupo5Milk <- c(select(datosGrupo5, c(Milk)))
grupo6Milk <- c(select(datosGrupo6, c(Milk)))

# Test de Kruskal-Wallis para los ingresos de Milk de cada grupo
kruskal.test(list(grupo1Milk, grupo2Milk, grupo3Milk, grupo4Milk, grupo5Milk, grupo6Milk))

# Ejemplo de Prueba de combinaciones con pares de grupos que dan hipotesis nulas para Milk
wilcox.test(x = as.numeric(unlist(select(datosGrupo5, c(Milk)))), y = as.numeric(unlist(select(datosGrupo6, c(Milk)))), conf.int = 0.05)



## datos para la variable Grocery
grupo1Grocery <- c(select(datosGrupo1, c(Grocery)))
grupo2Grocery <- c(select(datosGrupo2, c(Grocery)))
grupo3Grocery <- c(select(datosGrupo3, c(Grocery)))
grupo4Grocery <- c(select(datosGrupo4, c(Grocery)))
grupo5Grocery <- c(select(datosGrupo5, c(Grocery)))
grupo6Grocery <- c(select(datosGrupo6, c(Grocery)))

# Test de Kruskal-Wallis para los ingresos de Grocery de cada grupo
kruskal.test(list(grupo1Grocery, grupo2Grocery, grupo3Grocery, grupo4Grocery, grupo5Grocery, grupo6Grocery))

# Ejemplo de Prueba de combinaciones con pares de grupos que dan hipotesis nulas para Grocery
wilcox.test(x = as.numeric(unlist(select(datosGrupo1, c(Grocery)))), y = as.numeric(unlist(select(datosGrupo6, c(Grocery)))), conf.int = 0.05)



## datos para la variable Frozen
grupo1Frozen <- c(select(datosGrupo1, c(Frozen)))
grupo2Frozen <- c(select(datosGrupo2, c(Frozen)))
grupo3Frozen <- c(select(datosGrupo3, c(Frozen)))
grupo4Frozen <- c(select(datosGrupo4, c(Frozen)))
grupo5Frozen <- c(select(datosGrupo5, c(Frozen)))
grupo6Frozen <- c(select(datosGrupo6, c(Frozen)))

# Test de Kruskal-Wallis para los ingresos de Frozen de cada grupo
kruskal.test(list(grupo1Frozen, grupo2Frozen, grupo3Frozen, grupo4Frozen, grupo5Frozen, grupo6Frozen))

# Ejemplo de Prueba de combinaciones con pares de grupos que dan hipotesis nulas para Frozen
wilcox.test(x = as.numeric(unlist(select(datosGrupo5, c(Frozen)))), y = as.numeric(unlist(select(datosGrupo6, c(Frozen)))), conf.int = 0.05)



## datos para la variable Detergents_Paper
grupo1Detergents_Paper <- c(select(datosGrupo1, c(Detergents_Paper)))
grupo2Detergents_Paper <- c(select(datosGrupo2, c(Detergents_Paper)))
grupo3Detergents_Paper <- c(select(datosGrupo3, c(Detergents_Paper)))
grupo4Detergents_Paper <- c(select(datosGrupo4, c(Detergents_Paper)))
grupo5Detergents_Paper <- c(select(datosGrupo5, c(Detergents_Paper)))
grupo6Detergents_Paper <- c(select(datosGrupo6, c(Detergents_Paper)))

# Test de Kruskal-Wallis para los ingresos de Detergents_Paper de cada grupo
kruskal.test(list(grupo1Detergents_Paper, grupo2Detergents_Paper, grupo3Detergents_Paper, grupo4Detergents_Paper, grupo5Detergents_Paper, grupo6Detergents_Paper))

# Ejemplo de prueba de combinaciones con pares de grupos que dan hipotesis nulas para Detergents_Paper
wilcox.test(x = as.numeric(unlist(select(datosGrupo1, c(Detergents_Paper)))), y = as.numeric(unlist(select(datosGrupo6, c(Detergents_Paper)))), conf.int = 0.05)



