################
# Data Wrangling
################

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


###############
# Data Analysis
###############
# Themes for visualisation
if(!require(ggthemes)) install.packages("ggthemes")

# Check data
head(edx)
edx %>% as_tibble()

# Timestamp to date-time
if(!require(lubridate)) install.packages("lubridate")
edx <- mutate(edx, date = as_datetime(timestamp))
edx %>% as_tibble()

# Add week column to edx
edx <- edx %>% mutate(week = round_date(as_datetime(timestamp), unit = "week"))

# Add year column
edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
edx %>% as_tibble()

# Check for missing values
any(is.na(edx))

# Number of unique users and movies
edx %>% summarize(n_users = n_distinct(userId), 
                  n_movies = n_distinct(movieId))

# Sparse matrix for 100 random, unique userId and moveiId
users <- sample(unique(edx$userId), 100)
rafalib::mypar()
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

# Distribution of ratings
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, col = "black") + 
  scale_x_log10() + 
  ggtitle("Distribution of Ratings")

# Top movies by ratings count
edx %>% group_by(movieId, title) %>%
  summarize(rating_count = n(),
            avg_rating = mean(rating)) %>%
  arrange(desc(rating_count))

# Top movies by average rating (with at least 1000 ratings)
edx %>%
  group_by(movieId, title) %>%
  summarise(rating_count = n(),
            avg_rating = mean(rating)) %>%
  filter(rating_count >= 1000) %>%
  arrange(desc(avg_rating))

# Most to least popular ratings 
edx %>% group_by(rating) %>% 
  summarize(rating_count = n()) %>%
  arrange(desc(rating_count))

# Seperate genres
edx_genre <- edx %>% separate_rows(genres, sep = "\\|") # seperating genres may take some time
edx_genre %>% as_tibble()

# No. of ratings per genre 
edx_genre %>%
  group_by(genres) %>%
  summarize(rating_count = n()) %>%
  arrange(desc(rating_count)) # Drama & Comedy are broad, single genres

# Genre popularity per year
genre_popularity <- edx_genre %>%
  select(movieId, year, genres, rating) %>%
  mutate(genres = as.factor(genres)) %>% 
  group_by(year, genres) %>% 
  summarise(rating_count = n(), 
            avg_rating = mean(rating), 
            se = sd(rating)/sqrt(n()))
genre_popularity %>% as_tibble()

# Chosen genres = drama, comedy, action, horror, thriller & sci-fi
# Genres over time (no. of ratings)
genre_popularity %>%
  filter(genres %in% c("Drama", "Comedy", "Action", "Horror", "Thriller", "Sci-Fi")) %>%
  ggplot(aes(year, rating_count)) +
  geom_line(aes(col=genres)) +
  ggtitle("Genre Popularity Over Time (no. of ratings)")

# Genres over time (average rating)
genre_popularity %>%
  filter(genres %in% c("Drama", "Comedy", "Action", "Horror", "Thriller", "Sci-Fi")) %>%
  ggplot(aes(year, avg_rating)) +
  geom_point(aes(color=genres)) +
  geom_smooth(aes(color=genres), alpha = 0) +
  ggtitle("Genre Popularity Over Time (average rating)")

# Rating vs genres
edx_genre %>% group_by(genres) %>%
  summarize(rating_count = n(), 
            avg_rating = mean(rating), 
            se = sd(rating)/sqrt(n())) %>%
  filter(rating_count >= 1000) %>% 
  mutate(genres = reorder(genres, avg_rating)) %>%
  ggplot(aes(x = genres, 
             y = avg_rating, 
             ymin = avg_rating - 20*se, 
             ymax = avg_rating + 20*se, 
             col = rating_count)) + 
  geom_point(size = 2) +
  geom_errorbar(size = 1.25) + 
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Average Rating Per Genre")

# Rating vs year 
edx %>% group_by(year) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(year, avg_rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Rating Per Year")

# Distribution of users
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, col = "black") + 
  scale_x_log10() +
  ggtitle("Distribution of Users")

# Top Users (with at least 100 ratings)
edx %>%
  group_by(userId) %>%
  summarise(rating_count = n(),
            avg_rating = mean(rating)) %>%
  filter(rating_count >= 100) %>%
  arrange(desc(rating_count))


###########
# Modelling
###########
# Create a train & test set from edx
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-edx_test_index,]
edx_test <- edx[edx_test_index,]

###############
# RMSE function
###############
# Create a rmse function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,na.rm = T))
}

# Calculate mu
mu <- mean(edx_train$rating)

# Predict rmse on validation set
RMSE_pred <- RMSE(edx_test$rating, mu)
RMSE_pred

###############
# Effects model
###############
# Movie effect
b_i <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu)) # b_i = movie/item effect
qplot(b_i, geom = "histogram", col = I("black"), data = b_i)

predicted_ratings_movie <- edx_test %>%
  left_join(b_i, by="movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
rmse_movie <- RMSE(predicted_ratings_movie, edx_test$rating) 
rmse_movie # predicted rmse for movie effect

# User effect
b_u <- edx_train %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - b_i - mu)) # b_u = user effect
qplot(b_u, geom = "histogram", col = I("black"), data = b_u)

predicted_ratings_user <- edx_test %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(pred = mu + b_u) %>%
  pull(pred)
rmse_user <- RMSE(predicted_ratings_user, edx_test$rating)
rmse_user # predicted rmse for user effect

# Genre effect
b_g <- edx_train %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - b_i - b_u - mu)) # b_g = genre effect
qplot(b_g, geom = "histogram", col = I("black"), data = b_g)

predicted_ratings_genre <- edx_test %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  mutate(pred = mu_hat + b_g) %>%
  pull(pred)
rmse_genre <- RMSE(predicted_ratings_genre, edx_test$rating)
rmse_genre # predicted rmse for genre effect

# Time effect
b_t <- edx_train %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(week) %>%
  summarize(b_t = mean(rating - b_i - b_u - b_g - mu)) # b_t = time effect
qplot(b_t, geom = "histogram", col = I("black"), data = b_t)

predicted_ratings_time <- edx_test %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_t, by = "week") %>%
  mutate(pred = mu_hat + b_t) %>%
  pull(pred)
rmse_time <- RMSE(predicted_ratings_time, edx_test$rating)
rmse_time # predicted rmse for time effect

# Optimise tuning parameters
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l)) 
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l)) 
  b_g <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - b_i - b_u - mu)/(n() + l))
  b_t <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(week) %>%
    summarize(b_t = sum(rating - b_i - b_u - b_g - mu)/(n() + l))
  predicted_ratings <- edx_test %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_t, by = "week") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_t) %>% 
    pull(pred)
  return(RMSE(predicted_ratings, edx_test$rating))
})
rmses

# Optimum lambda = smallest rmse
qplot(lambdas, rmses)
min(rmses)
best_lambda <- lambdas[which.min(rmses)]
best_lambda

# Calculate final models with best_lambda
b_i <- edx_train %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + best_lambda))

b_u <- edx_train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() + best_lambda))

b_g <- edx_train %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>% 
  summarise(b_g = sum(rating - b_i - b_u - mu)/(n() + best_lambda))

b_t <- edx_train %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(week) %>%
  summarize(b_t = sum(rating - b_i - b_u - b_g - mu)/(n() + best_lambda))

#########
# RESULTS
#########
# Update validation set
validation <- mutate(validation, date = as_datetime(timestamp)) # add date column to validation
validation <- validation %>% mutate(week = round_date(as_datetime(timestamp), unit = "week")) # add week column to validation
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2))) # add year column to validation
validation %>% as_tibble()

validation_effects <- validation %>% # add effects variables to validation
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_t, by = "week")

# Predict ratings for entire effects model
validation_effects_predict <- validation_effects %>%  
  mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
  pull(pred)

# Final rmse
validation_effects_rmse <- RMSE(validation_effects_predict, validation$rating)
validation_effects_rmse

# Final Results
sprintf("The RMSE for the effects model is: %s", round(validation_effects_rmse, digits = 5))

################################
# BIBLIOGRAPHY
################################
https://bits.blogs.nytimes.com/2009/09/21/netflix-awards-1-million-prize-and-starts-a-new-contest/
http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary/
https://www.netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf

################################################################################
# Convert timestamp to date
edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")

#table thing
rmse_result_model_2 <- tibble(Model_Id = "Model 2",
                              Model_Method = "Content-based approach (Movie Effects)",
                              Predicted_RMSE = rmse_movie) # We generate a table to record our approaches and the rmse for the model

rmse_result_model_2 # Display the rmse results