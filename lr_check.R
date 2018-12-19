library(dslabs)
library(dplyr)
library(caret)
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
predictor <- dat$y
set.seed(1)
rmse <- replicate(n, {
test_index <- createDataPartition(predictor, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y ~ x, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2)) })
mean(rmse)
sd(rmse)