names(iris)
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
#> [5] "Species"
iris$Species
x <- iris[,1:4] %>% as.matrix()
d <- dist(x)
image(as.matrix(d), col = rev(RColorBrewer::brewer.pal(9, "RdBu")))

if(!exists("mnist")) mnist <- read_mnist()

col_means <- colMeans(mnist$test$images)
pca <- prcomp(mnist$train$images)
plot(pca$sdev)
summary(pca)$importance[,1:5] %>% knitr::kable()
library(dplyr)
tmp <- lapply( c(1:4,781:784), function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(id=i, label=paste0("PC",i), 
           value = pca$rotation[,i])
})
tmp <- Reduce(rbind, tmp)
library(ggplot2)
library(RColorBrewer)
tmp %>% filter(id<5) %>%
  ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(colors = brewer.pal(9, "RdBu")) +
  facet_wrap(~label, nrow = 1)

tmp %>% filter(id>5) %>%
  ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(colors = brewer.pal(9, "RdBu")) +
  facet_wrap(~label, nrow = 1)


library(caret)
K <- 36
x_train <- pca$x[,1:K]
y <- factor(mnist$train$labels)
fit <- knn3(x_train, y)

x_test <- sweep(mnist$test$images, 2, col_means) %*% pca$rotation
x_test <- x_test[,1:K]
y_hat <- predict(fit, x_test, type = "class")
confusionMatrix(y_hat, factor(mnist$test$labels))$overall






#Q1
#We want to explore the tissue_gene_expression predictors
#by plotting them.

#We want to get an idea of which observations are close
#to each other, but, as you can see from the dimensions,
#the predictors are 500-dimensional, making plotting difficult.
#Plot the first two principal components with color representing
#tissue type.
library(dslabs)
library(ggplot2)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
col_means <- colMeans(tissue_gene_expression$x)
pca <- prcomp(tissue_gene_expression$x)
plot(pca$sdev)
summary(pca)$importance[,1:6] %>% knitr::kable()

data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2],
           label=factor(tissue_gene_expression$y)) %>%
  ggplot(aes(PC1, PC2, fill=label))+
  geom_point(cex=3, pch=21)

#q2
#The predictors for each observation are measured using the same device
#and experimental procedure. This introduces biases that can affect
#all the predictors from one observation.
#For each observation, compute the average across all predictors,
#and then plot this against the first PC with color representing tissue.
#Report the correlation.

#What is the correlation?
row_means <- rowMeans(tissue_gene_expression$x)
df <- data.frame(x_means=row_means,pc=pca$x[,1],label=factor(tissue_gene_expression$y))
ggplot(df,aes(x_means, pc, fill=label))+
  geom_point(cex=3, pch=21)
cor(row_means, pca$x[,1])


#q3
#We see an association with the first PC and the observation averages.
#Redo the PCA but only after removing the center.
#Part of the code is provided for you.

#BLANK
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x))) 
pca <- prcomp(x)
data.frame(pc_1 = pca$x[,1], pc_2 = pca$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#q4


#For the first 10 PCs, make a boxplot showing the values for
#each tissue.

#For the 7th PC, which two tissues have the greatest median difference?
#Select the TWO tissues that have the greatest median difference.

data.frame(pc7=pca$x[,7], tissue = tissue_gene_expression$y) %>% 
  ggplot(aes(y=pc7, x=tissue)) + geom_boxplot()
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}
 summary(pca)
 