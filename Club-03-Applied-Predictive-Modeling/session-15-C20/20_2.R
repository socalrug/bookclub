library(caret)
library(AppliedPredictiveModeling)
library(tidyverse)
data(solubility)


# Reload models
#for (file in list.files(pattern = "*.Rds")) {
#  var_name = str_replace(file, ".Rds", "")
#  assign(var_name, readRDS(file))
#


################################################################################
# Split the solubility data into three groups. The middle range was defined as
# the mean +/- SD
################################################################################
trainData <- solTrainXtrans
lowcut <- mean(solTrainY) - sd(solTrainY)
highcut <- mean(solTrainY) + sd(solTrainY)
breakpoints <- c(min(solTrainY), lowcut, highcut, max(solTrainY))
groupNames <- c("Insoluble", "MidRange", "Soluble")
solTrainY3bin <- cut(solTrainY,
                     breaks = breakpoints,
                     include.lowest = TRUE,
                     labels = groupNames)
solTestY3bin <- cut(solTestY,
                    breaks = breakpoints,
                    include.lowest = TRUE,
                    labels = groupNames)


################################################################################
# (a) Fit a linear model, a nonlinear model, and a tree-based model to the 
#     three-bin data.
################################################################################
set.seed(100)
indx3bin <- createFolds(solTrainY3bin, returnTrain = TRUE)

# Recursive Partition
ctrl3bin <- trainControl(method = 'cv',
                         index = indx3bin,
                         classProbs = TRUE,
                         savePredictions = TRUE)
Rpart3bin <- train(x = trainData, 
                   y = solTrainY3bin,
                   method = "rpart",
                   metric = "Kappa",
                   tuneLength = 30,
                   trControl = ctrl3bin)
Rpart3bin
saveRDS(Rpart3bin, "Rpart3bin.Rds")

# Random Forest
set.seed(100)
mtryVals = floor(seq(10, ncol(trainData), length = 10))
mtryGrid <- data.frame(.mtry = mtryVals)
ctrlrf <- trainControl(method = 'cv', index = indx3bin)
rf3bin <- train(x = trainData, 
                y = solTrainY3bin,
                method = "rf",
                metric = "Kappa",
                ntree = 1000,
                importance = TRUE,
                trControl = ctrlrf)
rf3bin
saveRDS(rf3bin, "rf3bin.Rds")

# LDA 
set.seed(100)
ctrllda <- trainControl(method = 'cv',
                        index = indx3bin,
                        classProbs = TRUE,
                        savePredictions = TRUE)
lda3bin <- train(x = trainData, 
                 y = solTrainY3bin,
                 method = "lda",
                 preProc = c('center', 'scale'),
                 metric = "Kappa",
                 trControl = ctrllda)
lda3bin
saveRDS(lda3bin, "lda3bin.Rds")

################################################################################
# (b) Predict test set performance using the performance measure of your 
#     choice for each of the models in (a). Which model performs best for the
#     three-bin data?
################################################################################

accuracy <- function(model, x, y) {
  sum(y == predict(model, newdata = x)) / length(y)
}

model3bin <- list(Rpart3bin, rf3bin, lda3bin)
prediction3bin <- map(model3bin, predict, newdata = solTestX)
accuracy3bin <- map(model3bin, accuracy, x = solTestX, y = solTestY3bin)
accuracy3bin

# LDA performs the best
################################################################################
# (c) Now exclude the MidRange data from the training set, rebuild each model
#     and predict the test set.
################################################################################


trainData2bin <- trainData[solTrainY3bin != "MidRange", ]
solTrainY2bin <- droplevels(solTrainY3bin[solTrainY3bin != "MidRange"])
solTestY2bin <- droplevels(solTestY3bin[solTestY3bin != "MidRange"])
testData2bin <- solTestX[solTestY3bin != "MidRange", ]

# Recursive Partition
set.seed(100)
indx2bin <- createFolds(solTrainY2bin, returnTrain = TRUE)
ctrl2bin <- trainControl(method = 'cv',
                         index = indx2bin,
                         classProbs = TRUE,
                         savePredictions = TRUE)
Rpart2bin <- train(x = trainData2bin, y = solTrainY2bin,
                   method = 'rpart',
                   metric = "Kappa",
                   tuneLength = 30,
                   trControl = ctrl2bin)

Rpart2bin
saveRDS(Rpart2bin, "Rpart2bin.Rds")

# Random Forest
set.seed(100)
mtryVals = floor(seq(10, ncol(trainData2bin), length = 10))
mtryGrid <- data.frame(.mtry = mtryVals)
ctrlrf <- trainControl(method = 'cv', index = indx2bin)
rf2bin <- train(x = trainData2bin, 
                y = solTrainY2bin,
                method = "rf",
                metric = "Kappa",
                ntree = 1000,
                importance = TRUE,
                trControl = ctrlrf)
rf2bin
saveRDS(rf2bin, "rf2bin.Rds")

# LDA 
set.seed(100)
ctrllda <- trainControl(method = 'cv',
                        index = indx2bin,
                        classProbs = TRUE,
                        savePredictions = TRUE)
lda2bin <- train(x = trainData2bin, 
                 y = solTrainY2bin,
                 method = "lda",
                 preProc = c('center', 'scale'),
                 metric = "Kappa",
                 trControl = ctrllda)
lda2bin
saveRDS(lda2bin, "lda2bin.Rds")

model2bin <- list(Rpart2bin, rf2bin, lda2bin)
prediction2bin <- map(model2bin, predict, newdata = testData2bin)
cm2bin <- map(prediction2bin, confusionMatrix, solTestY2bin)
cm2bin

################################################################################
# (d) How do sensitivity and specificity compare for the insoluable and soluable
#     classes for the binding approaches in (b) and (c) within each model ond
#     between models
################################################################################
sensitivity2bin <- map(prediction2bin, sensitivity, solTestY2bin)
specificity2bin <- map(prediction2bin, specificity, solTestY2bin)

sensitivity2bin
specificity2bin

# specificity and sensitivity do not apply to multiclass problems
#map(prediction3bin, sensitivity, solTestY3bin)
#map(prediction3bin, specificity, solTestY3bin)

# LDA did really well in the 3 class problem but in the two class problem
# it had 0 sensitivity and 1/2 specificity. The other two models had perfect
# sensitivity and the rpart had 0 specificity.
