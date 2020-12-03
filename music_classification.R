remove.packages("devtools")
library(dplyr)
library(caTools)
library(caret)
library(class)
library(mlbench)
library(glmnet)
library(nnet)
library(DescTools)
library(rpart)
# Read in the file, which is tab-delimited
data.full <- read.delim(file.path("D:","7th sem","dsr","dsr project test", "songDb.tsv"), header=TRUE, sep="\t")
data
# Remove columns that don't serve as features (the Spotify URI, The track reference, the full URL, etc.)
data <- subset(data.full, select= - c(Uri, Ref_Track, URL_features, Type, ID, Name))
data
# Identify each song by its name (by changing the row names to song names)
rownames(data) <- make.names(data.full$Name, unique = TRUE)
rownames(data)
# Ensure the time signature is numeric, rather than a factor
# Not sure if the best decision, but prevents time_signature var from having too many factor levels
data$time_signature <- as.numeric(data$time_signature)
data$time_signature
# Tempo should also be numeric
data$Tempo <- as.numeric(data$Tempo)
data$Tempo






# Get much smaller subset of genres
genres <- list("canadianpop", "electronica", "rock", "modernblues", "r&b", "polishblackmetal", "videogamemusic", "irishfolk", "koreanpop", "hiphop")
genres
#genres <- droplevels(sample(unique(data$Genre), 10))
data.sub <- data[data$Genre %in% genres,]
data.sub
data.sub$Genre <- droplevels(data.sub$Genre)
data.sub$Genre




get_train_test <- function(split_ratio, data) {
  results <- list()
  
  split.index <- sample.split(seq_len(nrow(data)), split_ratio)
  
  results$data.train <- data[split.index, ]
  results$data.test <- data[!split.index, ]
  
  results$X.train <- results$data.train %>% select(-Genre) %>% as.matrix()
  results$Y.train <- results$data.train$Genre
  
  results$X.test <- results$data.test %>% select(-Genre) %>% as.matrix()
  results$Y.test <- results$data.test$Genre
  return(results)
}
# Knn 
set.seed(101)
# Create an empty data frame to store the predictions and the actual labels
classifications <- data.frame(pred=factor(), actual=factor())
# Use K-fold cross validation
K=5
for(k in 1:K) {
  # shuffle the data
  res <- get_train_test(0.8, data.sub)
  fit.knn <- knn(train=res$X.train, test=res$X.test, cl=res$Y.train)
  classifications <- rbind(classifications, data.frame(pred=fit.knn,   actual=res$Y.test))
}
confusionMatrix(classifications$pred, factor(classifications$actual))




set.seed(102)
res <- get_train_test(0.8, data.sub)
# Use cross validation
train.control <- trainControl(method="cv", number=5)
# Naive Bayes
fit.nb <- train(x=res$X.train, y=res$Y.train, trControl=train.control, method="nb")
Y.pred.nb <- predict(fit.nb, newdata = res$X.test, type="raw")
confusionMatrix(Y.pred.nb, factor(res$Y.test))



# Decision Tree
set.seed(103)
fit.dtree <- train(Genre~., data=res$data.train, method = "rpart", parms = list(split = "information"),trControl=train.control)
fit.dtree
res$X.test
dfr=data.frame(res$X.test)
dfr

Y.pred.dtree <- predict(fit.dtree, newdata=dfr, type="raw")
Y.pred.dtree
tou=confusionMatrix(Y.pred.dtree, res$Y.test)
tou

