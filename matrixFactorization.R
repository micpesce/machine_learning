library(tidyr)
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% 
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()
#We add row names and column names:

rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

#and convert them to residuals by removing the column and row effects:


y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))

#q1

set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#q3
s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))
#q4
ss_yv <-s$u %*% diag(s$d)

ss_y <-colSums(y^2)
ss_yv <-colSums(ss_yv^2)

ss_y <-round(ss_y)
ss_yv <-round(ss_yv)

a <-sum(ss_y)
b<- sum(ss_yv)
identical(a,b)
plot(ss_y)
plot(ss_yv)

#q5

plot(sqrt(ss_yv), s$d)
abline(0,1)

#q6
YV<-s$u %*% diag(s$d)
pcyv<-prcomp(YV)
summary(pcyv)
#Q7
u <- sweep(s$u, 2, s$d, FUN = "*")
pu<-prcomp(u)
summary(pu)

#Q8

avgSt <- rowMeans(y)
#MIA RISPOSTA SBAGLIATA
plot(YV[,1],avgSt)
#GRADER
plot(-s$u[,1]*s$d[1], rowMeans(y))
identical(-s$u[,1]*s$d[1],-YV[,1])

#Q9


my_image(s$v)

#risposta:
#The first column is very close to being a constant,
#which implies that the first column of YV is the sum of
#the rows of Y multiplied by some constant,
#and is thus proportional to an average.