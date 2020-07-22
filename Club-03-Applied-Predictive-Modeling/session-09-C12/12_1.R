### Problem 12.1

library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
library(DMwR)
library(ROSE)

data(hepatic)

################################################################################
### (a) - Given the classification imbalance, describe how you would create a 
###       training and test set
################################################################################

# This is multinomial classification, that is imbalanced
levels(injury)
table(injury)

table_df <- function(df_list){
  map_dfr(df_list, function(x){table(x$injury)}) %>% mutate(injury = levels(df_list[[1]]$injury))
}

# Create the data frames that are needed
df <- list(
  'bio' = bind_cols(data.frame(injury = factor(injury)), as.data.frame(bio)),
  'chem' = bind_cols(data.frame(injury = factor(injury)), as.data.frame(chem)),
  'bio_chem' = bind_cols(data.frame(injury = factor(injury)), as.data.frame(bio), as.data.frame(chem))
)
table_df(df)

# List of training indices for a balanced split
map(df, function(x){
  createDataPartition(x$injury, p = 0.8, list = FALSE, times = 1)
}) -> train_index

# Split the data
map2(df, train_index, function(x, index){ x[index,] }) -> train_raw
table_df(train_raw)
map2(df, train_index, function(x, index){ x[-index,] }) -> test
table_df(test)

# SMOTE sample the training data
map(train_raw, function(x){ SMOTE(injury ~ ., data = x)}) -> train
table_df(train)

#upsample_train <- upSample(x = bio, y = injury)
#table(upsample_train$Class)

#rose_train <- ROSE(injury~., data = df_bio)
#rose_train <- ROSE(Class ~ ., data = imbal_train)$data
#table(rose_train$Class)

################################################################################
### (b) - Which classification statistic would you choose and why
################################################################################

# While the training data is some what balanced by SMOTE the test data set is not.
# The cost of an incorrect prediction between the different classes is not the same.
# For example, if the true case was Severe and the model predicted Mild, that is 
# an issue but less of an issue for a None prediction. The multiclass accuracy 
# metric does not account for this difference in cost. However, the multiclass
# weighted accuracy metric allows for a weight to be assigned for each class 
# such that the sum of the weights across all classes equals 1. This helps but
# does not address the key issue in that the cost between different classes can
# be different, as previously discussed.\
# https://www.datascienceblog.net/post/machine-learning/performance-measures-multi-class-problems/

################################################################################
### (d) - preprocess data, split, and build models
################################################################################

df <- list(
  'bio' = bind_cols(data.frame(injury = factor(injury)), as.data.frame(bio)),
  'chem' = bind_cols(data.frame(injury = factor(injury)), as.data.frame(chem)),
  'bio_chem' = bind_cols(data.frame(injury = factor(injury)), as.data.frame(bio), as.data.frame(chem))
)
table_df(df)

# Preprocess the data
map(df, function(x) x[complete.cases(x), ])%>% 
  map(function(x) { x[,-nearZeroVar(x)] }) -> df_p

# List of training indices for a balanced split
map(df_p, function(x){
  createDataPartition(x$injury, p = 0.8, list = FALSE, times = 1)
}) -> train_index

# Split the data
map2(df_p, train_index, function(x, index){ x[index,] }) -> train_raw
table_df(train_raw)
map2(df_p, train_index, function(x, index){ x[-index,] }) -> test
table_df(test)

# SMOTE sample the training data
map(train_raw, function(x){ SMOTE(injury ~ ., data = x)}) -> train
table_df(train)


# LDA
map(train, function(x) {
  train(select(x, -injury),
        y = x$injury,
        method = 'lda',
        metric = 'Accuracy',
        preProc = c('center', 'scale'),
        trControl = trainControl(
          method = 'LGOCV',
          classProbs = TRUE,
          savePredictions = TRUE
          )
      )  
}) -> model_lda


# Partial Least Squares Discriminant Analysis
map(train, function(x) {
  train(select(x, -injury),
        y = x$injury,
        method = 'pls',
        metric = 'Accuracy',
        preProc = c('center', 'scale'),
        tuneGrid = expand.grid(.ncomp = 1:15),
        trControl = trainControl(
          method = 'LGOCV',
          classProbs = TRUE,
          savePredictions = TRUE
        )
  )  
}) -> model_pls

# Penalized 
map(train, function(x) {
  train(select(x, -injury),
        y = x$injury,
        method = 'glmnet',
        metric = 'Accuracy',
        preProc = c('center', 'scale'),
        tuneGrid = expand.grid(.alpha = c(0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 1),
                               .lambda = seq(0.01, 0.2, length = 40)),
        trControl = trainControl(
          method = 'LGOCV',
          classProbs = TRUE,
          savePredictions = TRUE
        )
  )  
}) -> model_glmnet

# Nearest Shrunken Centraids 
map(train, function(x) {
  train(select(x, -injury),
        y = x$injury,
        method = 'pam',
        metric = 'Accuracy',
        preProc = c('center', 'scale'),
        tuneGrid = expand.grid(.threshold = 0:25),
        trControl = trainControl(
          method = 'LGOCV',
          classProbs = TRUE,
          savePredictions = TRUE
        )
  )  
}) -> model_pam
