#Create a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor,
#using this code:
  
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)
#q1
#Which code correctly uses rpart to fit a regression tree and saves the result to fit?
plot(fit, margin = 0.1)
text(fit, cex = 0.75)