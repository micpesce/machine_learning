#Q1

#Use the training set to build a model with several of the models available from the caret package. 
#We will test out all of the following models in this exercise:

models <- c("gamLoess","glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost","knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma","qda")
library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  caret::train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#q2
#Now that you have all the trained models in a list,
#use sapply or map to create a matrix of predictions for the test set.
#You should end up with a matrix with length(mnist_27$test$y) rows and length(models).

#What are the dimensions of the matrix of predictions?

length(mnist_27$test$y)
length(models)
fits_predicts <- sapply(fits, function(fits){ predict(fits,mnist_27$test) })
dim(fits_predicts)
# La risposta è 200;23 


#q3
#Now compute accuracy for each model on the test set. Report the mean accuracy across all models
predicts <- as.data.frame(fits_predicts)


accuracy <- sapply(models, function(model){
  confusionMatrix(data=predicts[, model],reference = mnist_27$test$y,mode="everything")$overall["Accuracy"] })

mean(accuracy)

#q4

#Next, build an ensemble prediction by majority vote
#and compute the accuracy of the ensemble.

#What is the accuracy of the ensemble?
#n<-seq(1,200,1)
#majority <- sapply(n, function(i){
#  ifelse(length(which(fits_predicts[i,]==7)> 12) ,7,2) })
#confusionMatrix(as.factor(majority),reference = mnist_27$test$y)$overall["Accuracy"] 

library(purrr)
df <- data.frame(matrix(unlist(fits_predicts), nrow=23, byrow=T))
colnames(df) <- seq(1:200)
rownames(df) <- models
col_index <- seq(1,ncol(df), 1)
predict_vote <- map_df(col_index, function(j){
  vote <- ifelse(test = sum(df[,j] == 7) > 12, yes = 7, no = 2)
  return(data.frame(vote = vote))
})   # returns a df

predict_vote <- as.factor(predict_vote$vote) #  as factor

confusionMatrix(predict_vote,  mnist_27$test$y)$overall["Accuracy"]

#q5
#sort(accuracy)
ind <- accuracy > mean(predict_vote == mnist_27$test$y)
sum(ind)
models[ind]

#q6

#It is tempting to remove the methods that do not perform well
#and re-do the ensemble. The problem with this approach
#is that we are using the test data to make a decision.
#However, we could use the accuracy estimates obtained from
#cross validation with the training data.
#Obtain these estimates and save them in an object.
#Report the mean accuracy of the new estimates.

#What is the mean accuracy of the new estimates?

######MIA RISPOSTA ( ACCETTATA ma PPROCEDURA ERRATA)
accuracy <- map_df(models, function(model){
  
  train_cv<- caret:: train(y ~ .,method = model,data = mnist_27$train)
  y_hat <- predict(train_cv, mnist_27$test)
  confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
})

mean(accuracy)
#####


##RISPOSTA STAFF
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy) )
mean(acc_hat)


#q7
#Now let's only consider the methods with an estimated accuracy
#of greater than or equal to 0.8 when constructing the ensemble.

#What is the accuracy of the ensemble now?

library(tidyr)
ah <- acc_hat[acc_hat >= 0.8] #filtro da matrice
ah <- as.data.frame(ah) #ttraformazione in df
df1 <- cbind(models = rownames(ah), ah) #trasformazione id in colonna
rownames(df1) <- 1:nrow(ah) #ridenominazione id
models08 <- sapply(df1$models, function(model) toString(model)) #applicazione toString ad ogni elemento del vettore


library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models08, function(model){ 
  print(model)
  caret::train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models08

fits_predicts <- sapply(fits, function(fits){ predict(fits,mnist_27$test) })

predicts <- as.data.frame(fits_predicts)


library(purrr)
df <- data.frame(matrix(unlist(fits_predicts), nrow=14, byrow=T))
colnames(df) <- seq(1:200)
rownames(df) <- models08
col_index <- seq(1,ncol(df), 1)
predict_vote <- map_df(col_index, function(j){
  vote <- ifelse(test = sum(df[,j] == 7) > 7, yes = 7, no = 2)
  return(data.frame(vote = vote))
})   # returns a df

predict_vote <- as.factor(predict_vote$vote) #  as factor

confusionMatrix(predict_vote,  mnist_27$test$y)$overall["Accuracy"]


accuracy > mean(predict_vote == mnist_27$test$y)

