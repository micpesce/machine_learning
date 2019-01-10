library(tidyverse)
library(purrr)
library(dslabs)
library(broom)
data(mnist_27)
if(!exists("mnist")) mnist <- read_mnist()

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]
x[3,]
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)
image(1:28, 1:28, grid[,28:1])
sums <- rowSums(x)
as.vector(x)
qplot(as.vector(x), bins = 30, color = I("black"))
bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1
bin_X <- (x > 255/2)*1
bin_X

qplot(rnorm(100*10))
x <- matrix(rnorm(1000), 100, 10)
my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
seq(nrow(mat))
mat_1 <- mat + seq(nrow(mat))
M = matrix( 1:12, ncol=3)
dx = colMeans(M)
mat <- as.matrix(mnist$train$images)
dim(mat)
grey <- mat[mat > 50 & mat <205] 
length(grey)
length(grey)/(ncol(mat)*nrow(mat))
boxplot(grey)