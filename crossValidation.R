library(dplyr)
library(caret)
library(tidyverse)
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

#Q1
fit <- train(x_subset, y, method = "glm")
fit$results
#DOMANDA Q3
library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)
#RISPOSTA Q3
ind <- which(tt$p.value<0.01)
ind

x_subset_R <- x[ ,ind]