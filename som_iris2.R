# install.packages("kohonen") # 
# 0.1 load de library 
library("kohonen")
# 0.2load dataset http://archive.ics.uci.edu/ml/datasets/Iris
# the data is stored in the variable iris
str(iris)
# 0.3 seed for random inizialization
set.seed(7)
# 1. split the data into test and training (random)
# 1.1 get the training set observations
train.obs <- sample(nrow(iris), 50) 
# 1.2 division and scalation (parameters seems to be unnecessary)
train.set <- scale(iris[train.obs,][,-5]) # removing the class attribute (5)
test.set <- scale(iris[-train.obs,][,-5], 
                  center = attr(train.set, "scaled:center"),
                  scale  = attr(train.set, "scaled:scale"))
# 2. training the som. somgrid (!!)
som.iris <- som(train.set, grid = somgrid(5,5, "hexagonal"))
# 3. Plotting
# 3.1 plot the variable influence in each neuron
plot(som.iris, main = "Iris Data")
# 3.2 plot the quantity of samples
plot(som.iris, type="count")
# 3.3 plot the distance matrix
plot(som.iris, type="quality")
# 4. Prediction
som.prediction <- predict(som.iris, newdata = test.set, 
                          trainX = train.set,
                          trainY=classvec2classmat(iris[,5][train.obs]))


table(iris[,5][-train.obs], som.prediction$prediction)

# code from http://www.di.fc.ul.pt/~jpn/r/clustering/clustering.html#self-organizing-map-som

