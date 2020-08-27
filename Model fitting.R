rm(list = ls())
libs = c("MASS", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate",
         "tidyverse")
lapply(libs, library, character.only = TRUE)
options(digits = 5)
seed = function(x) set.seed(x, sample.kind = "Rounding")


# Importing Data 
library(data.table)
dl = tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings = fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                col.names = c("userId", "movieId", "rating", "timestamp"))
movies = str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) = c("movieId", "title", "genres")
movies = as.data.frame(movies) %>%
   mutate(movieId = as.numeric(movieId))

movielens = left_join(ratings, movies, by = "movieId")

# Validation set 
seed(1)
test_index = createDataPartition(y = movielens$rating, p = 0.1, list = F)
edx = movielens[-test_index, ]; temp = movielens[test_index, ]
validation = temp %>%
   semi_join(edx, by = "movieId") %>%
   semi_join(edx, by = "userId")

removed = anti_join(temp, validation)
edx = rbind(edx, removed)
rm(ratings, movies, test_index, temp, movielens, removed)

save(edx, file = "edx.RData")
save(validation, file = "validation.RData")

# NA values ?
edx %>% as.matrix() %>%
   apply(2, function(x) sum(is.na(x)))
validation %>% as.matrix() %>%
   apply(2, function(x) sum(is.na(x)))

# Data subset for training
seed(10)
ind = createDataPartition(edx$rating, p = 0.1, list = F)
edx_small = edx[ind,]   
save(edx_small, file = "data/edx_small.RData")

# Locally load data 
load("data/edx.RData")
load("data/validation.RData")
load("data/edx_small.RData")
rm(validation); rm(edx)

# Partitionning data in train/test sets
seed(20)
index = createDataPartition(edx_small$rating, p = 0.2, list = F)
train_set = edx_small[-index,]; test_set = edx_small[index,]
rm(index); rm(edx_small)

# RMSE function
RMSE = function(true, estimate) {
   sqrt(mean((true - estimate)^2))
}

# Make sure testing and training sets match in movies/users
test_set = test_set %>%
   semi_join(train_set, by = "movieId") %>%
   semi_join(train_set, by = "userId")

# Average rating of the training set
mu = mean(train_set$rating)

# Compute training set estimates of biases
movie_bias = train_set %>%
   group_by(movieId) %>%
   summarise(bi = mean(rating - mu))

user_bias = train_set %>%
   left_join(movie_bias, by = 'movieId') %>% 
   group_by(userId) %>%
   summarise(bu = mean(rating - mu - bi), .groups = "keep")

genre_bias = train_set %>%
   left_join(movie_bias, by = "movieId") %>%
   left_join(user_bias, by = "userId") %>%
   group_by(genres) %>%
   summarise(bg = mean(rating - mu - bi - bu))

# Train models
r_hat_m = mu + test_set %>%
   left_join(movie_bias, by = "movieId") %>%
   .$bi
Results = RMSE(test_set$rating, r_hat_m) %>%
   data.frame(Model = c("Model 1 : LM with movie bias"),
              RMSE = .)

r_hat_mb = mu + test_set %>%
   left_join(movie_bias, by = "movieId") %>%
   left_join(user_bias, by = "userId") %>%
   mutate(bias = bu + bi) %>%
   .$bias
Results = rbind(Results, 
                c("Model 2 : Model 1 + user bias", 
                  RMSE(test_set$rating, r_hat_mb)))

r_hat_mbg = mu + test_set %>%
   left_join(movie_bias, by = "movieId") %>%
   left_join(user_bias, by = "userId") %>%
   left_join(genre_bias, by = "genres") %>%
   mutate(bias = bu + bi + bg) %>%
   .$bias
Results = rbind(Results, 
                c("Model 3 : Model 2 + genre bias",
                  RMSE(test_set$rating, r_hat_mbg))) %>%
   knitr::kable(align = "c")

# Tune the penalty parameter for the movie effect regularized model
lambdas_bi = seq(2, 3, 0.1)

rmses_bi = sapply(lambdas_bi, function(l){
   movie_bias = train_set %>%
      group_by(movieId) %>%
      summarise(bi = sum(rating - mu) / (n() + l))
   r_hat = mu + test_set %>%
      left_join(movie_bias, by = "movieId") %>%
      .$bi
   RMSE(test_set$rating, r_hat)
})

plot(lambdas_bi, rmses_bi)
lambda_movie = lambdas_bi[which.min(rmses_bi)]; lambda_movie

# Best movie reg Model
movie_reg = train_set %>%
   group_by(movieId) %>%
   summarise(bi = sum(rating - mu) / (n() + 2.2))

r_reg_m = mu + test_set %>%
   left_join(movie_reg, by = "movieId") %>%
   .$bi
Results = rbind(Results, 
                c("Model 4 : Regularized Model 1",
                  RMSE(test_set$rating, r_reg_m)))

# Tune the penalty parameter for the movie+user reg model
lambdas_bi_bu = expand.grid(lambdas_bi = lambda_movie,
                            lambdas_bu = seq(4.5, 6, 0.1))

rmses_bi_bu = apply(lambdas_bi_bu, 1, function(l){
   movie_bias = train_set %>%
      group_by(movieId) %>%
      summarise(bi = sum(rating - mu) / (n() + l[1]))
   user_bias = train_set %>%
      left_join(movie_bias, by = "movieId") %>%
      group_by(userId) %>%
      summarise(bu = sum(rating - bi - mu) / (n() + l[2]))
   r_hat = mu + test_set %>%
      left_join(movie_bias, by = "movieId") %>%
      left_join(user_bias, by = "userId") %>%
      mutate(bias = bi + bu) %>%
      .$bias
   RMSE(test_set$rating, r_hat)
})

lambda_movie_user = lambdas_bi_bu[which.min(rmses_bi_bu),]; lambda_movie_user
plot(lambdas_bi_bu[,2], rmses_bi_bu)
cbind(rmses_bi_bu, lambdas_bi_bu)

# Best movie + user reg model
user_reg = train_set %>%
   left_join(movie_reg, by = "movieId") %>%
   group_by(userId) %>%
   summarise(bu = sum(rating - mu - bi) / (n() + 5.3))

r_reg_m_u = (mu + test_set %>%
                left_join(movie_reg, by = "movieId") %>%
                left_join(user_reg, by = "userId") %>%
                mutate(bias = bi + bu) %>%
                .$bias)[,1]
Results = rbind(Results, 
                c("Model 5 : Regularized Model 2",
                  RMSE(test_set$rating, r_reg_m_u)))

# Tune the penalty parameter for the movie+user+genre reg model - final model
lambdas_all = expand.grid(lambdas_bi = lambda_movie, lambdas_bu = lambda_movie_user,
                          lambdas_bg = seq(1.6, 1.9, 0.1))

rmses_all = apply(lambdas_all, 1, function(l){
   movie_bias = train_set %>%
      group_by(movieId) %>%
      summarise(bi = sum(rating - mu) / (n() + l[1]))
   user_bias = train_set %>%
      left_join(movie_bias, by = "movieId") %>%
      group_by(userId) %>%
      summarise(bu = sum(rating - bi - mu) / (n() + l[2]))
   genre_bias = train_set %>%
      left_join(movie_bias, by = "movieId") %>%
      left_join(user_bias, by = "userId") %>%
      group_by(genres) %>%
      summarise(bg = sum(rating - mu - bi - bu) / (n() + l[3]))
   r_hat = mu + test_set %>%
      left_join(movie_bias, by = "movieId") %>%
      left_join(user_bias, by = "userId") %>%
      left_join(genre_bias, by = "genres") %>%
      mutate(bias = bi + bu + bg) %>%
      .$bias
   RMSE(test_set$rating, r_hat)
})

lambda_all = lambdas_all[which.min(rmses_all),]; lambda_all
plot(lambdas_all[,3], rmses_all)
cbind(rmses_all, lambdas_all)

# Best final model
genre_reg = train_set %>%
   left_join(movie_reg, by = "movieId") %>%
   left_join(user_reg, by = "userId") %>%
   group_by(genres) %>%
   summarise(bg = sum(rating - mu - bi - bu) / (n() + 1.7))

r_reg_all = (mu + test_set %>%
                left_join(movie_reg, by = "movieId") %>%
                left_join(user_reg, by = "userId") %>%
                left_join(genre_reg, by = "genres") %>%
                mutate(bias = bi + bu + bg) %>%
                .$bias)[,1]
Results = rbind(Results, 
                c("Model 6 : Regularized Model 3",
                  RMSE(test_set$rating, r_reg_all))) %>%
   knitr::kable(align = "c")

# Final test on the Validation set
load("data/edx.RData")
load("data/validation.RData")
validation = validation %>%
   semi_join(train_set, by = "movieId") %>%
   semi_join(train_set, by = "userId")
mu = mean(edx$rating)

bi = edx %>%
   group_by(movieId) %>%
   summarise(bi = sum(rating - mu) / (n() + 2.2))

bu = edx %>%
   left_join(bi, by = "movieId") %>%
   group_by(userId) %>%
   summarise(bu = sum(rating - bi - mu) / (n() + 5.3))

bg = edx %>%
   left_join(bi, by = "movieId") %>%
   left_join(bu, by = "userId") %>%
   group_by(genres) %>%
   summarise(bg = sum(rating - mu - bi - bu) / (n() + 1.7))

r_hat = mu + validation %>%
   left_join(bi, by = "movieId") %>%
   left_join(bu, by = "userId") %>%
   left_join(bg, by = "genres") %>%
   mutate(bias = bi + bu + bg) %>%
   .$bias

performance = RMSE(validation$rating, r_hat)
performance





