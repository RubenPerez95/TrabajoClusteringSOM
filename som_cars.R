#Vamos a cargar los datos de coches vendidos en USA en el 1993
# https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Cars93.html

library("MASS") 
data(Cars93) # la variable Car93 almacena los datos
str(Cars93)

#Vemos que hay muchos datos, algunos numéricos así que para 
# el SOM en R no nos valdrían. Entonces lo que tenemos 
#es que seleccionar y además ver como tratarlos.

# podemos borrar aquellas variables que tienen muchos niveles,
# son más dificiles de transformar a numéricas y 
# además no nos van a aportar nada (o no queremos que nos aporte 
# por ejemplo el Modelo. 
Cars93$Model=NULL
Cars93$Make=NULL
Cars93$Man.trans.avail = NULL
Cars93$Manufacturer=NULL
# Las variables que se pueden transformar en numéricas 
# las vamos a transformar en numéricas.

# Atención a que se establece una relación ordinal 
# y se podrían cambiar los niveles y no ser automáticos
Cars93$AirBags = as.numeric(factor(Cars93$AirBags, levels = unique(Cars93$AirBags)))
Cars93$Origin = as.numeric(factor(Cars93$Origin, levels = unique(Cars93$Origin)))
Cars93$DriveTrain = as.numeric(factor(Cars93$DriveTrain, levels = unique(Cars93$DriveTrain)))

Cars93$Cylinders = as.numeric(Cars93$Cylinders)

# nos quedamos solo con los casos completos
Cars93 <- Cars93[complete.cases(Cars93),]

classes <- Cars93$Type
Cars93$Type = NULL

# normalizamos los datos que restan y 
# eliminamos casos con valores perdidos
data <- scale(Cars93)
# establecemos ad-hoc la parte de train y la de test
train.set <- data[1:50]
test.set <- data[51:82]

# construimos el mapa autorganizado.
library("kohonen")
set.seed(7) # aleatoriedad en la inicialización
# se puede experimentar con otros parámetros (rlen, alpha, etc.)
som.cars = som(as.matrix(train.set), grid = somgrid(3,3, "rectangular"))
# se grafica el som, pero como hay tanta variable el gráfico 
# no es tan significativo como si tuviese menos. 
#también puede influir el tipo de dato
plot(som.cars)
plot(som.cars, type="count")

# realizamos la predicción sobre la clase de test. 
# cada neurona reperesnetará al tipo mayoritario
som.prediction <- predict (som.cars, newdata= as.matrix(test.set), trainX = as.matrix(train.set), trainY = classvec2classmat(class[1:50]))

# sacamos la confussion matrix.
table(classes[51:82], som.prediction$prediction)

# los resultados no son buenos ¿pero hemos hecho alguna selección de variables??
