test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()


# I DIECI PIU' QUOTATI
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

# I DIECI MENO QUOTATI
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

#NUMERO DI RECENSIONI PER I PIU' QUOTATI

train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

#NUMERO DI RECENSIONI PER I MENO QUOTATI

train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

#REGOLARIZZAZIONE
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

#GRAFICO DEL CONFRONTO FRA IL BI REGOLARIZZATO E ORIGINALE: AL CRESCERE DEL
#NUMERO DELLE RECENSIONI bi acquista peso e le due valutazaioni tendono
#a coincidere
data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# i dieci migliori con il bi regolarizzato
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()


# i dieci peggiori con il bi regolarizzato

train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

#verifica RMSE ( vediamo il miglioramento)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((predicted_ratings-true_ratings)^2))
}
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(test_set$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()


#q1
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))
hist(n)

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#What are the top schools based on the average score?
#Show just the ID, size, and the average score.
#Report the ID of the top school and average score of the 10th school.
#What is the ID of the top school?
#risposta:
schools %>%  select(id,size,score) %>% arrange(desc(score)) %>% top_n(10, score) 

#q2
med_overall <- median(schools$size)

top_10_score <- schools %>%  select(id,size,score) %>% arrange(desc(score)) 


med_top_10 <- median(top_10_score$size[1:10])

#q3

worst_10_score <-schools %>%  select(id,size,score) %>%  arrange(score)  
med_worst_10 <- median(worst_10_score$size[1:10])

#q4
schools %>% ggplot(aes(size,score)) +geom_line() +scale_x_log10()sh
schools <-schools %>% mutate (se = sd(score)/sqrt(size))
#risposta:
#The standard error of the score has larger variability when the school is smaller,
#which is why both the best and the worst schools are more likely to be small

#q5
overall <- mean(sapply(scores, mean))
alfa <- 25 
schools <-schools %>% mutate (score_reg=overall + ((score - overall)*size/(size+alfa)))

schools %>% select(id,size,score,score_reg) %>% arrange(desc(score_reg)) %>% top_n(10, score_reg) 
#schools %>% group_by(id) %>% summarize(qualities= mean(quality), n=n())


#q6
alfas <- seq(10, 250)

rmses <- sapply(alfas, function(alfa){
  


schools %>% mutate (score_reg=overall + ((score - overall)*size/(size+alfa)))
  
  return(mean(schools$score_reg-schools$quality)^2)
})

qplot(alfas, rmses)  
alfa <- alfas[which.min(rmses)]
