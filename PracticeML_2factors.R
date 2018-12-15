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

#petal.lenght as a  1st. variable for cut-off
###TRAIN TEST
cutoff <- seq(4.2, 6.9, by=0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
####

###SELEZIONE DEL 1st BEST CUT-OFF
plot(cutoff,accuracy)
max(accuracy)
best_cutoff_1 <- cutoff[which.max(accuracy)]
####

#petal.width as a  2nd. variable for cut-off
###TRAIN TEST
cutoff <- seq(4.2, 6.9, by=0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
####

###SELEZIONE DEL 2st BEST CUT-OFF
plot(cutoff,accuracy)
max(accuracy)
best_cutoff_2 <- cutoff[which.max(accuracy)]
####

###APPLICAZIONE BEST CUT-OFF AL TRAIN GROUP E VALUTAZIONE ACCURACY

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% factor(levels = levels(test$Species))

mean(y_hat == test$Species)
####





###########################