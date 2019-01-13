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
#risposta q4
x_subset<- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results
#risposta q5
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


#risposta q6
library(dslabs)
data("tissue_gene_expression")
x<-tissue_gene_expression$x
y<-tissue_gene_expression$y 

train_index <- createDataPartition(y, times = 1, list = FALSE)
train_set_x <- x[train_index, ]
test_set_x <- x[-train_index, ]
train_set_y <- y[train_index]
test_set_y <- y[-train_index]
set.seed(1)
ks <- c(1, 3, 5, 7, 9, 11)
accuracy <- map_df(ks, function(k){
  
  fit <- knn3(train_set_x,train_set_y,  k = k)
  y_hat <- predict(fit, test_set_x, type = "class") 
  test_error<- confusionMatrix(data=y_hat,reference = test_set_y,mode="everything")$overall["Accuracy"]
  list(k=k,test=test_error)
})
as.data.frame(accuracy)

fit <- train(train_set_x, train_set_y, method = "knn", tuneGrid = data.frame(k = seq(1, 45, 2)))
ggplot(fit)

