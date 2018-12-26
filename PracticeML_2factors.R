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
cutoff_1 <- seq(4.2, 6.9, by=0.1)
accuracy_1 <- map_dbl(cutoff_1, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
####

###SELEZIONE DEL 1st BEST CUT-OFF
plot(cutoff_1,accuracy_1)
max(accuracy_1)
best_cutoff_1 <- cutoff_1[which.max(accuracy_1)]
####

#petal.width as a  2nd. variable for cut-off
###TRAIN TEST
cutoff_2 <- seq(1.4, 2.5, by=0.1)
accuracy_2 <- map_dbl(cutoff_2, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
####

###SELEZIONE DEL 2st BEST CUT-OFF
plot(cutoff_2,accuracy_2)
max(accuracy_2)
best_cutoff_2 <- cutoff_2[which.max(accuracy_2)]
####

###APPLICAZIONE BEST CUT-OFF AL TRAIN GROUP E VALUTAZIONE ACCURACY

y_hat <- ifelse(test$Petal.Length > best_cutoff_1 & test$Petal.Width > best_cutoff_2, "virginica", "versicolor") %>% factor(levels = levels(test$Species))

mean(y_hat == test$Species)
####





###########################