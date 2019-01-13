library(tidyverse)
library(purrr)
library(dslabs)
library(broom)
library(dplyr)
data(mnist_27)
library(caret)
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
tre<- which(indexes==3) 
tre 

set.seed(1)
y <- rnorm(100, 0, 1)
qnorm(0.75)

##q3
B <- 10^5
Ms <- replicate(B, {
   y <- rnorm(100, 0, 1)
  M <- quantile(y, 0.75)
})

avg <- mean(Ms)
sd <- sd(Ms)

#q4 e q5
set.seed(1)
y <- rnorm(100, 0, 1)

set.seed(1)
indexes <- createResample(y, 10000)

Q75s10k <- map_dbl(indexes, function(ind)
{
  y_hat <- y[ind]
  q75 <- quantile(y_hat, 0.75)
})

mean(Q75s10k)
sd(Q75s10k)

#q6 ( sbagliata)
var.test(Q75s10, Q75s10k)
