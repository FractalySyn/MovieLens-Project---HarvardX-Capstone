rm(list = ls())
libs = c("MASS", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate",
         "tidyverse")
lapply(libs, library, character.only = TRUE)
options(digits = 5)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")



# Intro -------------------------------------------------------------------

# For this project, you will be creating a movie recommendation system using the MovieLens 
# dataset. The version of movielens included in the dslabs package (which was used for some 
# of the exercises in PH125.8x: Data Science: Machine Learning) is just a small subset of a
# much larger dataset with millions of ratings. You can find the entire latest MovieLens 
# dataset here "https://grouplens.org/datasets/movielens/latest/" You will be creating your 
# own recommendation system using all the tools we have shown you throughout the courses in
# this series. We will use the 10M version of the MovieLens dataset to make the computation a 
# little easier.
# 
# You will download the MovieLens data and run code we will provide to generate your datasets.
# 
# First, there will be a short quiz on the MovieLens data. You can view this quiz as an opportunity 
# to familiarize yourself with the data in order to prepare for your project submission.
# 
# Second, you will train a machine learning algorithm using the inputs in one subset to predict 
# movie ratings in the validation set. Your project itself will be assessed by peer grading.


"You will use the following code to generate your datasets. Develop your algorithm using the edx 
set. For a final test of your final algorithm, predict movie ratings in the validation set (the 
final hold-out test set) as if they were unknown. RMSE will be used to evaluate how close your 
predictions are to the true values in the validation set (the final hold-out test set).

Important: The validation data (the final hold-out test set) should NOT be used for training,
developing, or selecting your algorithm and it should ONLY be used for evaluating the RMSE of
your final algorithm. You should split the edx data into separate training and test sets to 
design and test your algorithm."




# Create train and validation sets ----------------------------------------

library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl = tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings = fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                col.names = c("userId", "movieId", "rating", "timestamp"))

movies = str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) = c("movieId", "title", "genres")

movies = as.data.frame(movies) %>%
   mutate(movieId = as.numeric(movieId))
          # title = as.character(title),
          # genres = as.character(genres))

movielens = left_join(ratings, movies, by = "movieId")

# Validation set 10%
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




# Quiz MovieLens Dataset --------------------------------------------------

head(edx)

dim(edx)

sum(edx$rating == 0)
sum(edx$rating == 3)

length(unique(edx$movieId))
length(unique(edx$userId))

sum(edx$genres %>% str_detect("Drama"))
sum(edx$genres %>% str_detect("Comedy"))
sum(edx$genres %>% str_detect("Thriller"))
sum(edx$genres %>% str_detect("Romance"))

"Which movie has the greatest number of ratings?"
edx %>%
   group_by(movieId) %>%
   summarise(title = title, n = n()) %>%
   arrange(desc(n)) %>%
   unique() %>%
   .[1:10,]

"What are the five most given ratings in order from most to least?"
edx %>%
   group_by(rating) %>%
   summarise(n = n()) %>%
   arrange(desc(n)) %>%
   unique() %>%
   .[1:5,]

"In general, half star ratings are less common than whole star ratings ?"
whole_number = function(x) 
{
   round(x, 0) == x
}
mean(whole_number(edx$rating))






# MovieLens Project Instructions ------------------------------------------

"The submission for the MovieLens project will be three files: a report in the form of 
an Rmd file, a report in the form of a PDF document knit from your Rmd file, and an R 
script that generates your predicted movie ratings and calculates RMSE. Your grade for 
the project will be based on two factors:

1. Your report and script (75%)
2. The RMSE returned by testing your algorithm on the validation set (the final hold-out 
test set) (25%)"

# Note that to receive full marks on this project, you may not simply copy code from other 
# courses in the course series and be done with your analysis. Your work on this project 
# needs to build on code that is already provided

"Your report and script will be graded by your peers, based on a rubric defined by the
course staff. Each submission will be graded by three peers and the median grade will 
be awarded. To receive your grade, you must review and grade the submissions of five 
of your fellow learners after submitting your own. This will give you the chance to 
learn from your peers."

# Your movie rating predictions will be compared to the true ratings in the validation 
# set (the final hold-out test set) using RMSE. Be sure that your report includes the RMSE 
# and that your R script outputs the RMSE.







# MovieLens Grading Rubric ------------------------------------------------

# 10 points - Files
"The appropriate files are submitted in the correct formats: a report in both PDF and 
Rmd format and an R script in R format."

# 40 points - Report
"The report documents the analysis and presents the findings, along with supporting
statistics and figures. The report must be written in English and uploaded. The report
must include the RMSE generated. The report must include at least the following sections:

1. an introduction/overview/executive summary section that describes
the dataset and summarizes the goal of the project and key steps that
were performed

2. a methods/analysis section that explains the process and techniques
used, including data cleaning, data exploration and visualization, 
insights gained, and your modeling approach

3. a results section that presents the modeling results and discusses
the model performance

4. a conclusion section that gives a brief summary of the report, its
limitations and future work"

# 25 points - Code
"The code in the R script should should be well-commented and easy to follow. You are
not required to run the code provided (although you may if you wish), but you should 
visually inspect it."

# 25 points - RMSE
"Provide the appropriate score given the reported RMSE. Please be sure not to use the 
validation set (the final hold-out test set) for training or regularization - you should 
create an additional partition of training and test sets from the provided edx dataset 
to experiment with multiple parameters or use cross-validation.

25 points: RMSE < 0.86490"





# Ressources --------------------------------------------------------------

"http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary/"
"https://www.netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf"






# Exploration -------------------------------------------------------------

# NA values ?
edx %>% as.matrix() %>%
   apply(2, function(x) sum(is.na(x)))
validation %>% as.matrix() %>%
   apply(2, function(x) sum(is.na(x)))

# Work on a small subset of data -----------------------------------------------------------------

seed(10)
ind = createDataPartition(edx$rating, p = 0.1, list = F)
edx_small = edx[ind,]   
save(edx_small, file = "data/edx_small.RData")

# Load data (locally) --------------------------------------------------------------------

load("data/edx.RData")
load("data/validation.RData")
rm(validation)
rm(edx)
load("data/edx_small.RData")



# Partitioning -----------------------------------------------------------

seed(20)
index = createDataPartition(edx_small$rating, p = 0.2, list = F)
train_set = edx_small[-index,]; test_set = edx_small[index,]
rm(index); rm(edx_small)


# RMSE --------------------------------------------------------------------

RMSE = function(true, estimate) {
   sqrt(mean((true - estimate)^2))
}

# Fitting -----------------------------------------------------------------

test_set = test_set %>%
   semi_join(train_set, by = "movieId") %>%
   semi_join(train_set, by = "userId")

mu = mean(train_set$rating)

movie_bias = train_set %>%
   group_by(movieId) %>%
   summarise(bi = mean(rating - mu))
user_bias = train_set %>%
   left_join(movie_bias, by = 'movieId') %>% 
   group_by(userId) %>%
   summarise(bu = mean(rating - mu - bi))
genre_bias = train_set %>%
   left_join(movie_bias, by = "movieId") %>%
   left_join(user_bias, by = "userId") %>%
   group_by(genres) %>%
   summarise(bg = mean(rating - mu - bi - bu))

r_hat_m = mu + test_set %>%
   left_join(movie_bias, by = "movieId") %>%
   .$bi
RMSE(test_set$rating, r_hat_m)

r_hat_mb = mu + test_set %>%
   left_join(movie_bias, by = "movieId") %>%
   left_join(user_bias, by = "userId") %>%
   mutate(bias = bu + bi) %>%
   .$bias
RMSE(test_set$rating, r_hat_mb)

r_hat_mbg = mu + test_set %>%
   left_join(movie_bias, by = "movieId") %>%
   left_join(user_bias, by = "userId") %>%
   left_join(genre_bias, by = "genres") %>%
   mutate(bias = bu + bi + bg) %>%
   .$bias
RMSE(test_set$rating, r_hat_mbg)

# Regularized movie effect
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
lambda_movie = lambdas_bi[which.min(rmses_bi)]
movie_reg = train_set %>%
   group_by(movieId) %>%
   summarise(bi = sum(rating - mu) / (n() + 2.2))
r_reg_m = mu + test_set %>%
   left_join(movie_reg, by = "movieId") %>%
   .$bi
RMSE(test_set$rating, r_reg_m)

# Regularized movie + user effects
lambdas_bi_bu = expand.grid(lambdas_bi = 2.2,
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
user_reg = train_set %>%
   left_join(movie_reg, by = "movieId") %>%
   group_by(userId) %>%
   summarise(bu = sum(rating - mu - bi) / (n() + 5.3))
r_reg_m_u = (mu + test_set %>%
   left_join(movie_reg, by = "movieId") %>%
   left_join(user_reg, by = "userId") %>%
   mutate(bias = bi + bu) %>%
   .$bias)[,1]
RMSE(test_set$rating, r_reg_m_u)

# Regularized movie + user + genre effects
lambdas_all = expand.grid(lambdas_bi = 2.2, lambdas_bu = 5.3,
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
RMSE(test_set$rating, r_reg_all)

#╣ final model -> 2.2, 5.3, 1.7
load("data/edx.RData")
seed(50)
index = createDataPartition(edx$rating, p = 0.2, list = F)
train_set = edx[-index,]; edx = edx_small[index,]
rm(index); rm(edx)

test_set = test_set %>%
   semi_join(train_set, by = "movieId") %>%
   semi_join(train_set, by = "userId")

mu = mean(train_set$rating)

bi = train_set %>%
   group_by(movieId) %>%
   summarise(bi = sum(rating - mu) / (n() + 2.2))
bu = train_set %>%
   left_join(bi, by = "movieId") %>%
   group_by(userId) %>%
   summarise(bu = sum(rating - bi - mu) / (n() + 5.3))
bg = train_set %>%
   left_join(bi, by = "movieId") %>%
   left_join(bu, by = "userId") %>%
   group_by(genres) %>%
   summarise(bg = sum(rating - mu - bi - bu) / (n() + 1.7))
r_hat = mu + test_set %>%
   left_join(bi, by = "movieId") %>%
   left_join(bu, by = "userId") %>%
   left_join(bg, by = "genres") %>%
   mutate(bias = bi + bu + bg) %>%
   .$bias
RMSE(test_set$rating, r_hat)


# Validation --------------------------------------------------------------

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
RMSE(validation$rating, r_hat)




# Final code --------------------------------------------------------------

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
   summarise(bu = mean(rating - mu - bi))

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
   data.frame(Model = c("Model1 : LM with movie bias"),
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





