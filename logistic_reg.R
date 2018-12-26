library(dslabs)
library(dplyr)
library(caret)
library(e1071)
set.seed(2)
make_data <- function(mu_1 = delta){
  n <- 1000
  p <- 0.5
  mu_0 <- 0
  sigma_0 <- 1
  sigma_1 <- 1
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index)
  train
  test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index)
  test
  fit <- glm(y ~ x, data=train, family="binomial")
  p_y <- predict(fit, newdata = test, type = "response")
  y_hat <- ifelse(p_y>=0.5, 1,0)
  cm <-confusionMatrix(data = y_hat, reference = test$y)
  accuracy <- cm$overall['Accuracy']
  
}


delta <- seq(0, 3, len=25)

res <- sapply(delta, make_data)
plot(delta,res)
