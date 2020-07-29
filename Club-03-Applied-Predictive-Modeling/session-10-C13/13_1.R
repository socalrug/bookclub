### Problem 13.1

library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
library(DMwR)
library(ROSE)
library(mda)
library(earth)
library(klaR)
library(MLmetrics)

data(hepatic)

################################################################################
### (a) - Create the models used in this chapter
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

# MDA
# I am not able to get this to run.
map(train, function(x) {
  train(select(x, -injury),
        y = x$injury,
        method = 'mda',
        metric = 'Accuracy',
        preProc = c('center', 'scale'),
        tuneGrid = expand.grid(.subclasses = 1:3),
        trControl = trainControl(
          method = 'LGOCV',
          classProbs = TRUE,
          savePredictions = TRUE
        )
  )
}) -> model_mda


# nnet
map(train, function(x) {
  nnetGrid <- expand.grid(.size = 1:10, .decay = c(0, 0.1, 1, 2))
  train(select(x, -injury),
        y = x$injury,
        method = 'nnet',
        metric = 'Accuracy',
        preProc = c('center', 'scale'),
        tuneGrid = nnetGrid,
        trace = FALSE,
        maxit = 2000,
        maxNWts = max(nnetGrid$.size) * (length(x) + 1) + 2,
        trControl = trainControl(
          method = 'LGOCV',
          classProbs = TRUE,
          savePredictions = TRUE
        )
  )
}) -> model_nnet

# FDA
map(train, function(x) {
  fda(injury ~ ., data = x, method = earth)
}) -> model_fda

map(model_fda, function(x) {
  summary(x$fit)
})


# Support vector
map(train, function(x) {
  train(select(x, -injury),
        y = x$injury,
        method = 'svmLinear',
        metric = 'Accuracy',
        preProc = c('center', 'scale'),
#        tuneGrid = expand.grid(.sigma = sigest(as.matrix(x)),
#                               .C = 2^(seq(-4, 4))),
        fit = FALSE,
        trControl = trainControl(
          method = 'LGOCV',
          classProbs = TRUE,
          savePredictions = TRUE
          )
      )
}) -> model_svm


# KNN
map(train, function(x) {
  train(select(x, -injury),
        y = x$injury,
        method = 'knn',
        metric = 'Accuracy',
        preProc = c('center', 'scale'),
        tuneGrid = data.frame(.k = c( 4* (0:5) + 1,
                                     20 * (1:5) + 1,
                                     50 * (2:9) + 1)),
        trControl = trainControl(
          method = 'LGOCV',
          classProbs = TRUE,
          savePredictions = TRUE
        )
  )
}) -> model_knn

# Naive Bayes
map(train, function(x) {
  NaiveBayes(injury ~ .,
             data = x,
             usekernel = TRUE,
             fL = 2)
}) -> model_nb

map2(model_nb, test, function(model, data) {
  predict(model, newdata = data)
}) -> predict_nb

map2(predict_nb, test, function(prediction, truth) {
  MLmetrics::Accuracy(prediction$class, truth$injury)
}) -> accura
