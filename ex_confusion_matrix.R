library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)
y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#PROPORTION
k <- dat %>% filter(type=="inclass") %>% summarise(mean(sex=="Female"))
k
j<- dat %>% filter(type=="online") %>% summarise(mean(sex=="Female"))
j
#######

#In a course video, height cutoffs were used to predict sex. Instead of using height to predict sex, 
#use the type variable and assume students are male if x==online.
#You do not need to split the data into training and test sets.


#####ACCURACY
y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(levels = levels(y))

y_hat <- factor(y_hat)

mean(y_hat == dat$sex)

#########


##CONFUSION MATRIX

table(predicted = y_hat, actual = y)
dat %>% group_by(sex) %>% filter(type=="inclass" & sex=="Male") 
y_hat
y

sensitivity(y_hat, y)
specificity(y_hat, y)
prev <- mean(y == "Female")
prev