# install.packages("kohonen") # 
# load de library 
library("kohonen")
# load dataset (http://archive.ics.uci.edu/ml/datasets/Wine)
data("wines") # the matrix wines (with columns names) store the data
#data normalization
wines.sc = scale(wines)
# seed for random inizialization
set.seed(7)
# training the som. somgrid (!!)
wine.som <- som(data = wines.sc, grid = somgrid(5,4,"hexagonal"))
# plot the variable influence in each neuron
plot(wine.som, main = "Wine Data")
#~plot the quantity of samples
plot(wine.som, type="count")
# plot the distance matrix
plot(wine.som, type="quality")


# code from http://stackoverflow.com/questions/22196346/som-example-with-r-explanation
