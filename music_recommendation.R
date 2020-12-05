#https://rstudio-pubs-static.s3.amazonaws.com/342419_52da968026d04b749cad4d00f688f189.html
#https://www.kaggle.com/adiamaan/eda-and-feature-engineering/comments?select=song_extra_info.csv.7z

library(knitr)
load('FM/FM.RData')















library(tidyverse)
library(data.table)
library(Hmisc)
library(class)
library(recommenderlab)
library(dplyr)
options(tibble.print_max = 5, tibble.print_min = 5)
setwd("D:/7th sem/dsr/dsr project test")
rm(list= ls())
#setting data
train <- as.tibble(fread('music_rec_data/train.csv'))
songs <- as.tibble(fread('music_rec_data/songs.csv'))
song_extra_info <- as.tibble(fread('music_rec_data/song_extra_info.csv'))

#filtering data
ratings <- as.data.table(train[c('msno', 'song_id', 'target')])
filter_song_data <- as.data.frame(table(ratings$song_id))
filter_user_data <- as.data.frame(table(ratings$msno))

names(filter_song_data) <- c('song_id', 'count')
names(filter_user_data) <- c('msno', 'count')
ratings$song_count <- filter_song_data$count[match(ratings$song_id, filter_song_data$song_id)]
ratings$user_count <- filter_user_data$count[match(ratings$msno, filter_user_data$msno)]

#printing length of datasets
sc = 1000
uc = 500
filter_data <- subset(ratings, ratings$song_count>= sc & ratings$user_count >=uc)
nrow(filter_data)/nrow(ratings)
paste('Users:', length(unique(filter_data$msno)))
paste('Songs:', length(unique(filter_data$song_id)))

#train 
user_key <- as.data.frame(cbind(unique(filter_data$msno),c(1:length(unique(filter_data$msno)))))
names(user_key) <- c('user_code', 'msno')
song_key <- as.data.frame(cbind(unique(filter_data$song_id), c(1:length(unique(filter_data$song_id)))))
names(song_key) <- c('song_code', 'song_id')
filter_data$msno <- user_key$msno[match(filter_data$msno,user_key$user_code)]
filter_data$song_id <- song_key$song_id[match(filter_data$song_id, song_key$song_code)]


set.seed(666)
train_prop = 0.75
train_test_split <- function(ratings, train_proportion = 0.8){
  sample_size <- floor(train_proportion*nrow(ratings))
  train_ind <- sample(seq_len(nrow(ratings)), size = sample_size)
  train_data <- ratings[train_ind,]
  test_data <- ratings[-train_ind,]
  split_data <- list('train_data' = train_data, 'test_data' = test_data)
  return(split_data)
}
split_data <- train_test_split(filter_data,0.75)
training_data <- split_data$train_data
testing_data <- split_data$test_data
songs$song_id <- song_key$song_id[match(songs$song_id, song_key$song_code)]
songs <- subset(songs, songs$song_id %in% filter_data$song_id)
song_extra_info$song_id <- song_key$song_id[match(song_extra_info$song_id, song_key$song_code)]
song_extra_info <- subset(song_extra_info, song_extra_info$song_id %in% filter_data$song_id)

training_data <- as.tibble(training_data)


## ggplot setting for readable labels
readable_labs <- theme(axis.text=element_text(size=12),
                       axis.title=element_text(size=14),
                       plot.title = element_text(hjust = 0.5))
# Function to display count of each category of the column and plot how it affects target
target_vs_column <-function(df, col_name, x , y, title){
  temp_df <- df %>% 
    group_by_(col_name) %>% 
    summarise(count = n(), mean_target = mean(target)) %>% 
    arrange(desc(mean_target)) 
  
  df_plot <- temp_df %>%  
    ggplot(aes_string(col_name, "mean_target")) + 
    geom_col(aes(fill=count)) +
    scale_fill_gradient(low='turquoise', high = 'violet')+
    coord_flip() +
    labs(x = x,
         y = y,
         title= title) +
    readable_labs
  print(df_plot)
  return (temp_df)
}

# Function to group songs and user by count and check it against mean_target

# Function to group songs and user by count and check it against mean_target
target_vs_colcount <- function(df, col_name, x, y, title){
  df %>% 
    group_by_(col_name) %>% 
    summarise(count = n(), mean_target = mean(target)) %>% 
    group_by(count) %>% 
    summarise(new_count = n(), avg_target = mean(mean_target)) %>% 
    rename(no_of_items = new_count, occurence = count) %>% 
    arrange(desc(avg_target)) %>% 
    ggplot(aes(occurence, avg_target)) +
    geom_line(color='turquoise') +
    geom_smooth(color='turquoise') +
    labs(x = x,
         y = y,
         title= title) +
    readable_labs
}

#calling functions for plots
target_vs_colcount(training_data, "song_id", "Song Occurence", "Target", "Song Occurence vs Target")
target_vs_colcount(training_data, "msno", "User Occurence", "Target", "User Occurence vs Target")
#numbers count
training_data %>% 
  group_by(target) %>% 
  count
#members
members_colgroup <- function(df,col_name, x, y, title, xmin, xmax, ymin, ymax){
  
  temp_df <- df %>% 
    group_by_(col_name) %>% 
    count() %>% 
    arrange(desc(n))
  
  df_plot <- temp_df %>% 
    ggplot(aes_string(col_name, "n")) + 
    geom_col(fill='goldenrod2') + 
    labs(x = x,
         y = y,
         title = title) +
    xlim(xmin, xmax) +
    ylim(ymin, ymax) +
    readable_labs
  
  print(df_plot)
  return(temp_df)
  
}
training_data
members<- as.tibble(fread('music_rec_data/members.csv'))
members
members_colgroup(members, "bd", "Age", "Frequency", "Age Distribution", 1, 100, 0, 200)
members_colgroup(members, "city", "City", "Frequency", "City Distribution", 0, 25, 0, 1500)
members %>% 
  group_by(gender) %>% 
  count

#songs
songs
top_100 <- function(df, col_name)
{
  temp_df <- df %>% 
    group_by_(col_name) %>% 
    count %>% 
    arrange(desc(n)) %>% 
    print
  
  return(temp_df)
}
#TOP 100 
artist_count <- top_100(songs, "artist_name")
lyricist_count <- top_100(songs, "lyricist")
composer_count <- top_100(songs, "composer")
anguage_count <- top_100(songs, "language")

genre_count <- songs %>% 
  separate(genre_ids, c("one", "two", "three", "four", "five", "six", 
                        "seven", "eight"), extra="merge") %>% 
  select(one:eight)%>% 
  gather(one:eight, key="nth_id", value="genre_ids", na.rm=TRUE) %>% 
  group_by(genre_ids) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  print()

#Distribution of song length
songs %>% 
  mutate(song_length = song_length/6e4) %>% 
  ggplot(aes(song_length)) +
  geom_histogram(binwidth = 0.25, fill='darkorchid3') +
  labs(x='Song Length', y = 'Frequency', title = 'Distribution of song length') +
  xlim(0, 10)


#Collaborative Filter
#Item based CF
user_item_rating <- dcast(training_data, formula = msno~song_id, value.var = 'target')
training_sparse <- as(as.matrix(user_item_rating[-1]), 'realRatingMatrix')

ibcf <- Recommender(data = training_sparse, method = "IBCF", parameter = list(method = "Jaccard"))
predicted_ratings <- predict(ibcf, training_sparse, type = 'ratings')
predicted_ratings <- as(predicted_ratings, 'matrix')
predicted_ratings <- as.data.frame(predicted_ratings)
predicted_ratings$msno <- user_item_rating$msno
predicted_ratings_melt <- melt(predicted_ratings, id.vars = 'msno', variable.name = 'song_id')
results_ibcf <- merge(testing_data, predicted_ratings_melt, by = c('msno', 'song_id'))
results_ibcf$predicted_class <- 1
results_ibcf$predicted_class[results_ibcf$value < 0.5] <- 0

#user based Cf
ubcf <- Recommender(data = training_sparse, method = "UBCF", parameter = list(method = "Jaccard"))
predicted_ratings <- predict(ubcf, training_sparse, type = 'ratings')
predicted_ratings <- as(predicted_ratings, 'matrix')
predicted_ratings <- as.data.frame(predicted_ratings)
predicted_ratings$msno <- user_item_rating$msno
predicted_ratings_melt <- melt(predicted_ratings, id.vars = 'msno', variable.name = 'song_id')
results_ubcf <- merge(testing_data, predicted_ratings_melt, by = c('msno', 'song_id'))
results_ubcf$predicted_class <- 1
results_ubcf$predicted_class[results_ubcf$value < 0.5] <- 0
results_ubcf$random_class <- 1


#baseline accuracy measures
conf_matrix_cf_ibcf <- table(results_ibcf$target, results_ibcf$predicted_class)
ibcf_accuracy <- (conf_matrix_cf_ibcf[1,1]+conf_matrix_cf_ibcf[2,2])/sum(conf_matrix_cf_ibcf)
conf_matrix_cf_ubcf <- table(results_ubcf$target, results_ubcf$predicted_class)
ubcf_accuracy <- (conf_matrix_cf_ubcf[1,1]+conf_matrix_cf_ubcf[2,2])/sum(conf_matrix_cf_ubcf)
conf_matrix_rand <- table(results_ubcf$target, results_ubcf$random_class)
rand_accuracy <- (conf_matrix_rand[2,1])/sum(conf_matrix_rand)

cf_accuracy <- as.data.frame(cbind(c('item_based', 'user_based', 'random'), c(ibcf_accuracy, ubcf_accuracy, rand_accuracy)))
names(cf_accuracy) <- c('model', 'accuracy')
cf_accuracy$accuracy_r <- round(as.numeric(as.character(cf_accuracy$accuracy)), 4)

#checking values 
paste('Random Prediction Accuracy is: ', round(rand_accuracy,4))

paste('Item-Based Collaborative Filter Accuracy is: ', round(ibcf_accuracy,4))

print('IBCF Confusion Matrix')

conf_matrix_cf_ibcf

paste('User-Based Collaborative Filter Accuracy is: ', round(ubcf_accuracy,4))

print('UBCF Confusion Matrix')

conf_matrix_cf_ubcf

#plot for CF
p <- ggplot(data=cf_accuracy, aes(x=model, y=accuracy_r)) +
  geom_bar(stat="identity", fill="steelblue")+ ylim(0,0.8) +
  ggtitle('Collaborative Filtering Models') +
  theme_minimal()
p




#Factorization Machines
all <- data.table(rbind(training_data, testing_data))

# clean song genre
songs %<>% 
  separate(genre_ids, c("one", "two", "three", "four", "five", "six", "seven", "eight"), 
           extra="merge") %>%
  replace_na(list(one = 0, two = 0, three = 0, four = 0,
                  five = 0, six = 0, seven = 0, eight = 0)) %>% 
  mutate(no_of_genre = (if_else(one == 0, 0, 1) + if_else(two == 0, 0, 1) +
                          if_else(three == 0, 0, 1) + if_else(four == 0, 0, 1) +
                          if_else(five == 0, 0, 1) + if_else(six == 0, 0, 1) +
                          if_else(seven == 0, 0, 1) + if_else(eight == 0, 0, 1))) %>% 
  select(song_id, song_length, language, artist_name, no_of_genre, one)

#add metadata
merged_all <- all %>% 
  left_join(songs, by = "song_id") %>%
  left_join(members, by = "msno") %>%
  left_join(train, by = c("msno", "song_id", "target"))

#drop variables from the merged data
merged_all %<>%
  select(-song_count, -user_count, -registered_via, -registration_init_time, -expiration_date)

#clean data: categorical variables must be coded as factors to train the model
merged_all <- within(merged_all, {
  bd[bd < 14 | bd > 100] <- median(bd) #set age outliers to median
  logbd <- log(bd)
  log_song_length <- log10(song_length)
  one <- factor(one)
  msno <- factor(msno)
  song_id <- factor(song_id)
  artist_name <- factor(artist_name)
  language <- factor(language)
  city <- factor(city)
  gender <- factor(gender)
  source_system_tab <- factor(source_system_tab)
  source_screen_name <- factor(source_screen_name)
  source_type <- factor(source_type)
}) 

merged_training <- merged_all[1:nrow(training_data),]
merged_testing <- merged_all[-c(1:nrow(training_data)),]


k.vec <- seq(0, 30, by = 5)
cv_k <- cv_libFM(merged_training, target ~ song_id + language + artist_name +
                   one + no_of_genre + msno + city + gender + logbd + log_song_length +  
                   source_system_tab + source_screen_name + source_type,
                 task = "c", dims = k.vec, iter = 100, folds = 3,
                 cv_verbosity = 1, verbosity = 1, method = "mcmc")



plot_param_cv <- function(k, m, err.bar.width = .1){
  # Plot Parameter Tuning CV Results
  ## input: 
  # k: vector of values to test for the hyperparamter
  # m: an n x k matrix of accuracy rates (n = # of folds)
  ## returns list of:
  # data frame of parameter values, mean error rate, and standard error
  # ggplot of the error rate vs. value of the hyperparameter
  
  err <- 1 - m
  mean <- apply(err, 2, mean)
  se <- apply(err, 2, function(x) sd(x)/sqrt(length(x)))
  df <- data.frame(cbind(k, mean, se))
  
  p <- ggplot(df, aes(x=k, y=mean)) + 
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                  width = err.bar.width, color='blue' ) +
    geom_line(color='blue') +
    ylab("Error Rate") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(list(df, p))
}
#use the fm_k_cv data frame in the FM.RData file which contains the console output of cv_libFM
cv_k_results <- plot_param_cv(k.vec, fm_k_cv, err.bar.width = .3)
as.tibble(cv_k_results[[1]])