#q1
#In the exercise in Q6 from Comprehension Check: Trees and Random Forests,
#we saw that changing nodesize to 50 and setting maxnodes to 25 yielded smoother results.
#Let's use the train function to help us pick what the values of nodesize and maxnodes should be.
#From the caret description of methods, we see that we can't tune the maxnodes parameter or the nodesize
#argument with randomForests. So we will use the __Rborist__ package and tune the minNode argument.
#Use the train function to try values minNode <- seq(25, 100, 25). Set the seed to 1.

#Which value minimizes the estimated RMSE?
library(Rborist)
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
#risposta
fit <- train(y ~ x , method = "Rborist", tuneGrid = data.frame(predFixed = 1, minNode = seq(25, 100, 25)), data = dat)


#q2
#Part of the code to make a scatterplot along with the prediction
#from the best fitted model is provided below.

#Which code correctly can be used to replace #BLANK in the code above?
library(caret)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2) #BLANK risposta


#Q3

#Use the rpart function to fit a classification tree to the
#tissue_gene_expression dataset. Use the train function to estimate
#the accuracy. Try out cp values of seq(0, 0.1, 0.01).
#Plot the accuracies to report the results of the best model. Set the seed to 1991.

#Which value of cp gives the highest accuracy?
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

#risposta
set.seed(1991)
train_rpart <- train(x, y, 
                     method = "rpart",
                     tuneGrid = data.frame(cp=seq(0, 0.1, 0.01))                     )
ggplot(train_rpart)

#q4
#Study the confusion matrix for the best fitting classification tree from the exercise in Q3.
#What do you observe happening for the placenta samples?
confusionMatrix(predict(train_rpart,x), y)
#risposta:
#Placenta samples are being classified somewhat evenly across tissues


#q5

#Note that there are only 6 placentas in the dataset. By default,
#rpart requires 20 observations before splitting a node.
#That means that it is difficult to have a node in which placentas are the majority. 
#Rerun the analysis you did in the exercise in Q3, but this time, 
#allow rpart to split any node by using the argument control = rpart.control(minsplit = 0). 
#Look at the confusion matrix again to determine whether the accuracy increases. 
#Again, set the seed to 1991.

#What is the accuracy now?

set.seed(1991)
data("tissue_gene_expression")

fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)

#q6
#Plot the tree from the best fitting model of the analysis you ran in Q5.
#Which gene is at the first split?
fit_rpart$finalModel



#q7
#We can see that with just seven genes, we are able to predict the tissue type.
#Now let's see if we can predict the tissue type with even fewer genes using a Random Forest.
#Use the train function and the rf method to train a Random Forest.
#Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other values on your own).
#What mtry value maximizes accuracy? To permit small nodesize to grow as we did with the classification trees, use the following argument: nodesize = 1.
#Note: This exercise will take some time to run. If you want to test out your code first,
#try using smaller values with ntree. Set the seed to 1991 again.

#What value of mtry maximizes accuracy?
set.seed(1991)
fit_rtree <- with(tissue_gene_expression, 
                  train(x, y, method = "rf",
                        tuneGrid = data.frame(mtry = seq(50,200,25)),
                         nodesize = 1 ))
print(fit_rtree)
#q8
imp <- varImp(fit_rtree)
imp

#q9

imp

#output VarImp con CFHR4=25.03 AL 4° POSTO (RANK)