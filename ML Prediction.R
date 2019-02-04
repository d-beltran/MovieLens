# The MovieLens data set is downloaded
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
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

#-----------------------------------------------------------------------------------

# The repeated movie is corrected
edx[edx$movieId==64997,] <- edx[edx$movieId==64997,] %>% 
  mutate(movieId=34048, genres="Action|Adventure|Sci-Fi|Thriller")
edx <- edx %>%
  anti_join(edx[edx$title == "War of the Worlds (2005)",][duplicated(edx[edx$title == "War of the Worlds (2005)",]$userId),])

# Validation set is also corrected
validation[validation$movieId==64997,] <- validation[validation$movieId==64997,] %>%
  mutate(movieId=34048, genres="Action|Adventure|Sci-Fi|Thriller")

# The movie without genres is filled
edx <- edx %>% mutate(genres=ifelse(title=="Pull My Daisy (1958)", "Comedy", genres))

# Validation set is also corrected
validation <- validation %>% mutate(genres=ifelse(title=="Pull My Daisy (1958)", "Comedy", genres))

# The mean rate of each movie is calculated and joined to each row of the whole edx data set
movieInfo <- edx %>% group_by(movieId) %>% summarise(meanrate = mean(rating))
edx <- edx %>% left_join(movieInfo, by="movieId")

# The mean difference in each user's ratings is calculated
userInfo <- edx %>% group_by(userId) %>% summarise(meandiffuser = mean(rating-meanrate))

# Mean rate of each movie and mean difference in each user's ratings is joined to the validation set
validation <- validation %>% left_join(movieInfo, by="movieId") %>% left_join(userInfo, by="userId")

# Rates in the validation set are calculated
y_hat <- validation$meanrate + validation$meandiffuser
y_hat[y_hat < 0.5] <- 0.5
y_hat[y_hat > 5] <- 5

# Calculate the RMSE
sqrt(mean((y_hat-validation$rating)^2))