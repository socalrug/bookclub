library(mlbench)
library(caret)
set.seed(200)
trainingData <- mlbench::mlbench.friedman1(200, sd = 1)
trainingData$x <- data.frame(trainingData$x)
caret::featurePlot(trainingData$x, trainingData$y)
testData <- mlbench::mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x)

knnModel <- train(
  x = trainingData$x,
  y = trainingData$y,
  method = 'knn',
  preProc = c('center', 'scale'),
  tuneLength = 10
)
knnModel

knnPred <- predict(knnModel, newdata = testData$x)
postResample(pred = knnPred, obs = testData$y)

# Mars
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
marsTuned <- train(trainingData$x, trainingData$y,
                   method = 'earth',
                   tuneGrid = marsGrid,
                   trControl = trainControl(method = 'cv'))
marsTuned
