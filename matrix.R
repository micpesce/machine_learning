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