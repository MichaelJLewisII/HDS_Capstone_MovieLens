#########################################################
# Capstone Project I:  Predicting Movie Ratings
# Course:  HarvardX - PH125.9x
# Student:  Mike Lewis
# Date:  2019-2020
#########################################################



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



################################
# End of Provide Code
################################


################################


################################
# Save files & load libraries
################################

save(edx, file = "edx.Rdata")
save(validation, file = "validation.Rdata")

if(!require(knitr)) install.packages("knitr")
if(!require(GGally)) install.packages("GGally")
if(!require(ggExtra)) install.packages("ggExtra")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(stringr)) install.packages("stringr")

  # Note: Caret and tidyverse are both required as well but are loaded in the provided code.


################################
# EDA - Prep
################################

str(edx)

# Checking for missing values
sapply(edx, function(x) sum(is.na(x))) %>% 
  kable(col.names = c("Missing Values")) 

sapply(edx, function(x) sum(is.null(x))) %>% 
  kable(col.names = c("Missing Values")) 

# Unique movies and users
edxUniques <- summarize(edx, users = n_distinct(edx$userId), 
                        movies = n_distinct(edx$movieId)) %>%
                          kable(caption = "Table 1. Counts of Unique Users and Movies", col.names = c("Unique Users", "Unique Movies"))
edxUniques

# Transform timestamp to date (from UTC 1/1/1970)
edx$timestamp <- edx$timestamp %>%
  as.POSIXct(origin = "1970-01-01")

# Create a year of release (assumes mid-year release) and lag-to-rate columns
releaseYear <- as.Date(paste("07/01/",
                             str_sub(edx$title, str_length(edx$title) - 4, 
                                     str_length(edx$title) -1), sep =""), "%m/%d/%Y")

ratingLag <- as.numeric(as.Date(edx$timestamp) - releaseYear)

  # Append new columns
  edx <- cbind(edx, ratingLag, releaseYear)

  head(edx)
  

################################
# EDA - Visuals
################################
  
# Create small set with which to generate charts quickly
set.seed(1, sample.kind="Rounding")  # If using R <v3.6, use set.seed(1)
edaTbl <- sample_n(edx, 10000)


# Figure 1 - Distribution of Movie Ratings
ggplot(edaTbl, aes(rating)) +
  geom_bar(position = "dodge", fill = "#06b5ed") +
  labs(title = "Distribution of Movie Ratings", x = "Rating", y = "Count of Ratings")


# Figure 2 - Is there a distinction between movies getting worse and reviewers looking back fondly on them?
ggplot(edaTbl, aes(releaseYear, ratingLag, group = releaseYear)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#FFCC66", aes(group = 1)) + 
  labs(title = "Movie Age vs. Rating Lag", x = "Year of Release", y = "Days Between Release and Rating") +
  geom_text(x = 7250, y = 27000, label = round(cor(as.numeric(releaseYear), ratingLag), 3))   # Guess not - ratings began recently.
  
  # Thus, let's drop the 'ratings lag' column
  edx <- subset(edx, select = -ratingLag)
  

# Figure 3 - Time Series Analysis
annualTbl <- edaTbl %>% 
                group_by(releaseYear) %>% 
                summarize(meas = mean(rating), n = n()) %>%
                mutate(YOY.RelativeRating = (meas - mean(edaTbl$rating))/mean(edaTbl$rating)*100)

ggplot(annualTbl, aes(x = releaseYear, y = YOY.RelativeRating)) + 
  geom_line() +
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Figure 3. Time Series Analysis of Ratings", 
       subtitle = "YOY Relative Ratings Performance",
       x = "Year",
       y = "% Performance")


# Figure 4 - A closer look at 'genre'
edaTbl %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(numRatings = n()) %>% 
  arrange(desc(numRatings))

  # Ratings by genre
  edaTbl %>% 
    separate_rows(genres, sep = "\\|") %>% 
    select(genres, rating) %>% 
    group_by(genres) %>%
    ggplot(aes(x = factor(genres), y = rating, fill = genres)) +
    geom_boxplot() +
    coord_flip() +
    labs(title = "Figure 4. Movie Ratings by Genre", 
         x = "Genre",
         y = "Rating")  # Appears some genres have higher rating distributions, but given some movies fall into multiple genres.  
                        # Thus, we need to be aware of potential multicollinearity if further analysis pursued.


  
################################
# Update Validation Data Set
################################
  
releaseYear <- as.Date(paste("07/01/",
                         str_sub(validation$title, str_length(validation$title) - 4, str_length(validation$title) -1), sep =""), "%m/%d/%Y")

validation <- cbind(validation, releaseYear)

  # Check for missing values
  sapply(validation, function(x) sum(is.na(x))) %>% 
    kable(col.names = c("Missing Values"))
  
  sapply(validation, function(x) sum(is.null(x))) %>% 
    kable(col.names = c("Missing Values")) 

  # Remove remaining extraneous objects
  rm(ratingLag, releaseYear, noirDummy)

  

################################
# ANALYSIS (on full data sets)
################################
  
# Now let's perform some analytics
  # Subset dataset into training and testing sets (50/50)
  set.seed(1, sample.kind="Rounding") # If using R <v3.6, use set.seed(1)
  test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
  training <- edx[-test_index,]
  temp <- edx[test_index,]

  # Check to ensure all 'users' and 'movies' from test are in training set
  testing <- temp %>% 
    semi_join(training, by = "movieId") %>%
    semi_join(training, by = "userId")
  
    # Add back missing rows to training set and remove extraneous data
    withheld <- anti_join(temp, testing)
    training <- rbind(training, withheld)
    
    rm(temp, withheld, edx)
 
# Function for calc root mean sq. error
RMSE <- function(trueRatings, predictedRatings){
          sqrt(mean((trueRatings - predictedRatings)^2, na.rm = TRUE))
        } 
    
# MODEL 0 - A Naive Model, predicitng average movie rating for all obs.
mu_0 <- mean(training$rating)  
  
  # Evaluate MODEL 0 on TEST data & add to table for model comparison
  naiveRMSE <- RMSE(testing$rating, mu_0)
  rmseResults <- tibble(method = "Average Rating Only", RMSE = naiveRMSE)
  

# MODEL 1 - Linear regression with 'movie' effects
m1 <- training %>% 
  group_by(movieId) %>%
  summarize(b_mov = mean(rating - mu_0))

predictedRatings <- mu_0 + testing %>%
  left_join(m1, by = 'movieId') %>%
  .$b_mov

  # Evaluation
  m1RMSE <- RMSE(predictedRatings, testing$rating)
  rmseResults <- bind_rows(rmseResults, tibble(method = "Movie Effects", RMSE = m1RMSE))
  rmseResults %>% knitr::kable()


# MODEL 2 - Model 1 + 'user' (rater) effects
m2 <- training %>%
  left_join(m1, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_user = mean(rating - mu_0 - b_mov))

predictedRatings <- testing %>% 
  left_join(m1, by = 'movieId') %>% 
  left_join(m2, by = 'userId') %>% 
  mutate(pred = mu_0 + b_mov + b_user) %>%
  .$pred

  # Evaluation
  m2RMSE <- RMSE(predictedRatings, testing$rating)
  rmseResults <- bind_rows(rmseResults, tibble(method = "Movie + User Effects", RMSE = m2RMSE))
  rmseResults %>% knitr::kable()
  
  
# MODEL 3 - Model 2 + 'year' effects
m3 <- training %>%
  left_join(m1, by = 'movieId') %>%
  left_join(m2, by = 'userId') %>%
  group_by(releaseYear) %>%
  summarize(b_yr = mean(rating - mu_0 - b_mov - b_user))

predictedRatings <- testing %>% 
  left_join(m1, by = 'movieId') %>% 
  left_join(m2, by = 'userId') %>%
  left_join(m3, by = 'releaseYear') %>%
  mutate(pred = mu_0 + b_mov + b_user + b_yr) %>%
  .$pred

  # Evaluation
  m3RMSE <- RMSE(predictedRatings, testing$rating)
  rmseResults <- bind_rows(rmseResults, tibble(method = "Movie + User + Year Effects", RMSE = m3RMSE))
  rmseResults %>% knitr::kable()
  

### Predicting on the 'Validation' data ###
predictedRatings <- validation %>% 
  left_join(m1, by = 'movieId') %>% 
  left_join(m2, by = 'userId') %>%
  left_join(m3, by = 'releaseYear') %>%
  mutate(pred = mu_0 + b_mov + b_user + b_yr) %>%
  .$pred
  
  # Evaluation
  validRMSE <- RMSE(predictedRatings, validation$rating)
  rmseResults <- bind_rows(rmseResults, tibble(method = "Validation Performance",
                                               RMSE = validRMSE))
  rmseResults %>% knitr::kable()
  # RMSE for predictions on validation data is 0.8655043.
