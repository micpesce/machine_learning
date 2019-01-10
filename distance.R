library(tidyverse)
library(purrr)
library(dslabs)
library(broom)
library(dplyr)
data(mnist_27)


set.seed(0)
if(!exists("mnist")) mnist <- read_mnist()

ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]
d_492 <- as.matrix(d)[492,]

###esercizi
library(dslabs)
data("tissue_gene_expression")

x <- tissue_gene_expression$x
table(tissue_gene_expression$y)
d <- dist(x)
class(d)
#distanza fra elementi della stessa classe
as.matrix(d)[1:2,2:1]
as.matrix(d)[39:40,40:39]
as.matrix(d)[73:74,74:73]
#distanza fra elementi diversi
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
image(as.matrix(d))