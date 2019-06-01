#run a summary on the dataset to look for NAs or anomalies
summary(edx)

#import tidyverse and create a tibble 
library(tidyverse)
edx %>% as_tibble()

#check count of users and movies which were reviewed
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

#histogram to see bias in number of reviews for different movies
hist(edx$movieId)

#import caret and set seed
library(caret)
set.seed(1)

# Splitting the train-test 80-20
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Removing the movies and users not present in trainset
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Defining the RMSE Function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Get the mean for the training set ratings and calculate a first off base rmse
mu <- mean(train_set$rating)
base_rmse <- RMSE(train_set$rating, mu)

# Add the base RMSE to a resuts dataframe
rmse_results <- data_frame(method = "Base RMSE", RMSE = base_rmse)

# Decompose the movie ratings by the average of the movie rating
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# Get predictions for each movie based on movie (popularity and reach) and set as b_i (item effect)
predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

# MODEL 1: predictions based on just the movie effects model
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effects Model",
                                     RMSE = model_1_rmse))

# Decompose the rating further by including the user averages for rating the movie
user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Get predictions for each movie based on user (number of users) and set as b_u (user effects) 
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# MODEL 2: Movie raings based on both user and movie effects model
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",
                                     RMSE = model_2_rmse))



# Regularization. Searching for the correct value of lambda
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
lambdas[which.min(rmses)] # min lambda value comes to be 4.75
qplot(lambdas, rmses) 


# Running regularization on the user + item effects model with lambda as 4.75
l <- 4.75
mu <- mean(train_set$rating)

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effects Model",
                                     RMSE = model_3_rmse))

# Intoducting a time based concept
# To model time between the first and corresponding reviews
# Writing a function to bin the time deltas
get_bin <- function(max_timestamp, min_timestamp,curr_timestamp){
  max_timestamp
  min_timestamp
  ts_delta <- max_timestamp - min_timestamp
  ts_delta
  bin_num <- cut(ts_delta,breaks = 5,labels = c('1','2','3','4','5'))
  return(as.numeric(bin_num))
}

# Trying to find the relationship factor (I call it alpha) between the ratings and the time passed by between the
# first rating and all the subsequent ratings
l = 4.75
alpha_Seq = seq(0,10,0.25)
rmses <- sapply(alpha_Seq, function(alpha_val){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_t <- train_set %>%
    left_join(b_i,by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    group_by(movieId) %>% 
    mutate(bin = as.numeric(cut(timestamp, breaks = 5, labels = c(1:5)))) %>%
    group_by(movieId) %>%
    summarize(b_t = sum(rating - b_i - b_u - mu + (bin * alpha_val)/(n() * l)))

  predicted_ratings <-  test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = 'movieId') %>%
    mutate(pred = mu + b_i + b_u - b_t) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
alpha_Seq[which.min(rmses)] 
which.min(rmses)
qplot(alpha_Seq, rmses) 
# Since the above approach does not help in reducing the RMSE. We will leave it find out which model
# has the lowest RMSE

View(rmse_results)

# Lowest RMSE is 0.865 for the "Regularized Movie + User Effects Model".
# So lets train this model on the whole edx dataset and find the RMSE on validations dataset

l <- 4.75
mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
final_model_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Final RMSE Validation",
                                     RMSE = final_model_rmse))

rmse_results
#final_model_rmse = 0.8648201