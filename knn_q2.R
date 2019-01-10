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

