library(dslabs)
data("movielens")
head(movielens)
#numero utenti e numero film ( tra loro indipendenti)
movielens %>% summarize(n_users = n_distinct(userId),n_movies = n_distinct(movieId)) 

#distribuzione delle recensioni su base id film: si noti la differenza fra blockbuster e film indipendenti
movielens %>% count(movieId) %>%   ggplot(aes(n)) + 
geom_histogram(bins = 30, color = "black") + 
scale_x_log10() + 
ggtitle("Movies")

#distribuzione delle recensioni su base quantità utente: sino ti chi vota più e chi vota meno
movielens %>%   count(userId) %>%   ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")


#partizione del df in test e train set per algoritmo ML

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

#SEMI-JOIN per essere certi che test-set e train-set hanno le stesse coppie movieID-userID
test_set <- test_set %>%   semi_join(train_set, by = "movieId") %>%  semi_join(train_set, by = "userId")

#PREVISIONE BASATA SULLA SEMPLICE MEDIA(INDIPENDENTE DAL TIPO DI FILM E DI UTENTE)

mu_hat <- mean(train_set$rating)
mu_hat

#Formula RMSE 

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#CALCOLO RMSE ( viene un mumero maggiore di 1 -troppo alto )

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse


predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)


rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)


#calcolo del b_i(indice di gradiento) ottenuto dalla formula Y-mu 
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>%   group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- test_set %>%  left_join(movie_avgs, by='movieId') %>%  .$b_i +mu


model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()


#differenziazione per userID

train_set %>% group_by(userId) %>%  summarize(b_u = mean(rating)) %>% filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

#PREDICTION CONSIDERANDO BU E BI

user_avgs <- test_set %>%   left_join(movie_avgs, by='movieId') %>%  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred


model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


#Q1
#Compute the number of ratings for each movie and then plot it against
#the year the movie came out. Use the square root transformation on the counts.

#What year has the highest median number of ratings?
#mia risposta ok
movielens %>% group_by(movieId,year) %>% count(rating)  %>% ggplot(aes(year,n)) + 
    scale_y_sqrt() + geom_point()
a$year[max(median(a$n))]

#risposta grader
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#q2
#We see that, on average, movies that came out after 1993 get more
#ratings. We also see that with newer movies,
#starting in 1993, the number of ratings decreases with year:
#the more recent a movie is, the less time users have had to rate it.

#Among movies that came out in 1993 or later,
#what are the 25 movies with the most ratings per year,
#and what is the average rating of each of the top 25 movies?
  
#What is the average rating for the movie The Shawshank Redemption?



library(dplyr)

a <- movielens %>% group_by(movieId)  %>% mutate(avg=mean(rating)) %>% filter(year >1992)
b<-a[order(-a$rating),]
as.data.frame(b) %>% slice(1:25)
# fin qui prima risposta ok


# seconda risposta: la mia sarebbe giusta, ma il db arriva fino a 2016
#per cui le righe sono 21 risultato: 16,..
#se considerassi altre due righe con rating zero (2017 e 2018) dividerei
#il totale per 23 ed otterei 14.82 risultato giusto
library(dplyr)
library(tibble)
library(lubridate)
library(dslabs)
data("movielens")
movielens <- as.tibble(movielens)
movielens <- movielens %>% mutate(datetime = as_datetime(timestamp)) %>% mutate(rating_year = year(datetime), rating_week=week(datetime),rating_day=day(datetime)) %>%
  filter(movieId==356) %>%
  group_by(rating_year) %>% tally()  

avg <- sum(movielens$n)/23






#soluzione proposta dal gruppo di discussione ( giust)
FG <- movielens %>% filter(title == "Forrest Gump" )     # Grab all the FG data
counts <- FG %>% count(rating)    # Simple table of ratings and counts
sum(counts$n) / (2018-1994)  # Total counts / Time span
  
#soluzione del grader
library(dplyr)
library(tibble)
library(lubridate)
library(dslabs)
data("movielens")
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 

#q6
#Compute the average rating for each week
#and plot this average against day.
#Hint: use the round_date function before you group_by.

#What type of trend do you observe?
#mia risposta
library(dplyr)
library(tibble)
library(lubridate)
library(dslabs)
library(ggplot2)
data("movielens")
movielens <- movielens %>% mutate(datetime = round_date(as_datetime(timestamp))) %>% mutate(rating_week=week(datetime))%>%
group_by(rating_week) %>%  summarize( avg= mean(rating)) 
movielens %>% ggplot(aes(movielens$rating_week,movielens$avg)) +geom_point() +
  geom_smooth()

#grader
movielens %>% mutate(datetime = round_date(as_datetime(timestamp))) %>% mutate(rating_week=week(datetime))%>%
  group_by(rating_week) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(rating_week, rating)) +
  geom_point() +
  geom_smooth()


#q8
#The movielens data also has a genres column. 
#This column includes every genre that applies to the movie.
#Some movies fall under several genres.
#Define a category as whatever combination appears in this column.
#Keep only categories with more than 1,000 ratings.
#Then compute the average and standard error for each category.
#Plot these as error bar plots.
#Which genre has the lowest average rating?

#mia risposta corretta (riordinato solo tabella) ma incompleta
movielens %>% group_by(genres)%>% summarize(n=n(), avg = mean(rating), se=sd(rating)/sqrt(n) )%>%  filter(n >1000) %>%
ggplot(aes(x=genres,y=avg)) +geom_bar()

#risposta grader
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
