library(caret)
library(purrr)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
y
set.seed(2)
# line of code
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
#petal.lenght as a variable for cut-off
###TRAIN TEST
cutoff <- seq(4.2, 6.9, by=0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
####

###SELEZIONE DEL BEST CUT-OFF
plot(cutoff,accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
####

###APPLICAZIONE BEST CUT-OFF AL TRAIN GROUP E VALUTAZIONE ACCURACY

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% factor(levels = levels(test$Species))

mean(y_hat == test$Species)
####





###########################