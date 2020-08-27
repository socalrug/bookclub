library(tidyverse)
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)


################################################################################
#  (a) Create an initial filter of the predictors that removes predictors to
#      minimize the amount of multicollinearity in the data prior to modelling
################################################################################
cut_off <- 0.7

# Get a tibble of unique pairs of features and their correlation
# Only keep those pairs that have a correlation about cut_off
predictors %>% 
  dplyr::select(-Genotype) %>% 
  cor() %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(feature = names(.)) %>% 
  tidyr::pivot_longer(cols = !feature) %>% 
  dplyr::filter(name != feature,
                abs(value) > cut_off) %>% 
  dplyr::mutate(feature_1 = dplyr::if_else(name < feature, name, feature),
                feature_2 = dplyr::if_else(name > feature, name, feature)) %>% 
  dplyr::select(-c(name, feature)) %>% 
  dplyr::distinct() -> correlation


# Get a list all the features that have a high correlation (i.e. about cut_off)
# and compute the Spearman-Rank correlation with the predictor, diagnosis
correlation %>% 
  select(starts_with("feature")) %>% 
  tidyr::pivot_longer(cols = everything()) %>% 
  dplyr::select(value) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(value) %>% 
  dplyr::select(predictors, .) %>% 
  cor(as.integer(diagnosis), method = 'spearman') %>% 
  tibble::tibble(feature = row.names(.),
                 spearman = .) -> spearman

# Create a list of features to remove as they have the lowest Spearman-Rank
# correlation in the pair of correlations
correlation %>% 
  dplyr::left_join(spearman, by = c('feature_1' = 'feature')) %>% 
  dplyr::rename('spearman_1' = 'spearman') %>% 
  dplyr::left_join(spearman, by = c('feature_2' = 'feature')) %>% 
  dplyr::rename('spearman_2' = 'spearman') %>% 
  dplyr::mutate(worst = dplyr::if_else(abs(spearman_1) < abs(spearman_2), feature_1, feature_2)) %>% 
  dplyr::pull(worst) %>% 
  unique -> remove_feature

# Remove the highly correlated features from the predictor tibble
predictors %>%   
  dplyr::select(-remove_feature) -> predictors_low_corr

# The findCorrelation function kind of does the same thing but it
# does not consider the relationship to the predictor. Let us see
# what the difference in nanmes are
setdiff(findCorrelation(select(predictors, -Genotype), names = TRUE, cutoff = cut_off), remove_feature)

# findCorrelation removed a lot
length(findCorrelation(select(predictors, -Genotype), names = TRUE, cutoff = cut_off))

# remove_feature is much more conservative
length(remove_feature)

# Which ones did they have in common?
intersect(findCorrelation(select(predictors, -Genotype), names = TRUE, cutoff = cut_off), remove_feature)

# Almost everything in remove_feature is in findCorrelations
length(intersect(findCorrelation(select(predictors, -Genotype), names = TRUE, cutoff = cut_off), remove_feature))


################################################################################
# (b) Refit the recursive feature selection models
################################################################################

# Random Forest RFE

# Orginal Model
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
split <- createDataPartition(diagnosis, p = 0.8, list = FALSE)
adData <- predictors
adData$Class <- diagnosis
training <- adData[split, ]
testing <- adData[-split, ]
predVars <- names(adData)[!names(adData) %in% c("Class", "Genotype")]
index <- createMultiFolds(training$Class, times = 5)
varSeq <- seq(1, length(predVars) - 1, by = 2)
newRF <- rfFuncs
newRF$summary <- fiveStats

ctrl_rf <- rfeControl(method = 'repeatedcv',
                   repeats = 5,
                   verbose = TRUE,
                   functions = newRF,
                   index = index)
rfRFE <- rfe(x = training[, predVars],
             y = training$Class,
             sizes = varSeq,
             metric = "ROC",
             rfeControl = ctrl_rf,
             ntree = 1000)
rfRFE

---------------------------
# High Correlation features removed

adData_low_corr <- predictors_low_corr
adData_low_corr$Class <- diagnosis
training_low_corr <- adData_low_corr[split, ]
testing_low_corr <- adData_low_corr[-split, ]
predVars_low_corr <- names(adData_low_corr)[!names(adData_low_corr) %in% c("Class", "Genotype")]

rfRFE_low_corr <- rfe(x = training_low_corr[, predVars_low_corr],
             y = training_low_corr$Class,
             sizes = varSeq,
             metric = "ROC",
             rfeControl = ctrl_rf,
             ntree = 1000)

rfRFE_low_corr

###########################################################
# SVM RFE

# Original Model
svmFuncs <- caretFuncs
svmFuncs$summary <- fiveStats

ctrl_svm <- rfeControl(method = 'repeatedcv',
                   repeats = 5,
                   verbose = TRUE,
                   functions = svmFuncs,
                   index = index)

svmRFE <- rfe(x = training[, predVars],
              y = training$Class,
              sizes = varSeq,
              metric = "ROC",
              rfeControl = ctrl_svm,
              method = "svmRadial",
              tuneLength = 12,
              preProc = c("center", "scale"),
              trControl = trainControl(method = "cv",
                                       verboseIter = FALSE,
                                       classProbs = TRUE))
svmRFE

-------------
# High Correlation features removed
  
adData_low_corr <- predictors_low_corr
adData_low_corr$Class <- diagnosis
training_low_corr <- adData_low_corr[split, ]
testing_low_corr <- adData_low_corr[-split, ]
predVars_low_corr <- names(adData_low_corr)[!names(adData_low_corr) %in% c("Class", "Genotype")]

svmRFE_low_corr <- rfe(x = training[, predVars],
              y = training$Class,
              sizes = varSeq,
              metric = "ROC",
              rfeControl = ctrl_svm,
              method = "svmRadial",
              tuneLength = 12,
              preProc = c("center", "scale"),
              trControl = trainControl(method = "cv",
                         verboseIter = FALSE,
                         classProbs = TRUE))
svmRFE_low_corr

################################################################################
# (c) Did the RFE profiles shown in Fig 19.4 change considerably? Which models
#     would have been mostly likely affected by multicollinearity?
################################################################################

