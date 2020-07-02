library(mlbench)
library(caret)
library(kernlab)
##EXERCISE 7.1
# set.seed(100)
# x <- runif(100, min=2, max=10)
# y <- sin(x) + rnorm(length(x)) * .25
# sinData <- data.frame(x = x, y = y)
# ## Create a grid of x values to use for prediction
# dataGrid <- data.frame(x = seq(2, 10, length= 100))
# rbfSVM <- kvsm(x = x, y = y, data = sinData,
#                kernel = "rbfdot", kpar = "automatic",
#                C = 1, epsilon = 0.1)

##EXERCISE 7.2
set.seed(200)
trainingData <- mlbench.friedman1(200, sd = 1)
## We convert the 'x' data from a matrix to a data frame
## One reason is that this will give the columns names.
trainingData$x <- data.frame(trainingData$x)
## Look at the data using
featurePlot(trainingData$x, trainingData$y)
## or other methods.

## This creates a list with a vector 'y' and a matrix of predictors 'x'.
## Also simulate a large test set to estimate the true error rate with good precision.
testData <- mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x)

#Table to store model results.
results_table <- matrix(nrow = 4, ncol = 3, byrow = TRUE)
rownames(results_table) <- c("knn", "nn", "MARS", "SVM")
colnames(results_table) <- c("RMSE", "Rsquared", "MAE")

#K nearest neighbor
knnModel <- train(x = trainingData$x, 
                  y = trainingData$y,
                  method = "knn",
                  preProc = c("center", "scale"),
                  tuneLength = 10,
                  metric = "MAE")
knnModel
knnPred <- predict(knnModel, newdata = testData$x)
## The function 'postResample' can be used to get the test set
## performance values
results_mod <- postResample(pred = knnPred, obs = testData$y)
results_table[1,] <- results_mod

#Neural network
set.seed(0)
#Create a specific candidate set of models to evaluate.
nnGrid = expand.grid( decay=c(0,0.01,0.1), size=1:10 )
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
nnetModel = train(x=trainingData$x, 
                  y=trainingData$y, 
                  method="nnet", 
                  preProc=c("center", "scale"),
                  linout=TRUE,trace=FALSE,
                  MaxNWts=10 * (ncol(trainingData$x)+1) + 10 + 1, maxit=500,
                  tuneGrid = nnGrid,
                  trainControl = ctrl,
                  metric = "MAE")
nnetModel
nnetPred <- predict(nnetModel, newdata=testData$x)
results_mod <- postResample(pred=nnetPred, obs=testData$y)
results_table[2,] <- results_mod

#SVM
set.seed(0)
svmRModel <- train(x=trainingData$x, 
                  y=trainingData$y, 
                  method="svmRadial", 
                  preProc=c("center", "scale"), tuneLength=20)
svmRModel
svmRPred <- predict(svmRModel, newdata=testData$x)
postResample(pred = svmRPred, obs = testData$y)
results_table[4,] <- results_mod

#MARS
set.seed(0)
#Define the candidate models to test
marsGrid = expand.grid(.degree=1:2, .nprune=2:38)
marsModel <- train(x=trainingData$x, 
                   y=trainingData$y, 
                   method="earth", 
                   preProc=c("center", "scale"), tuneGrid=marsGrid)
marsModel
marsPred <- predict(marsModel, newdata=testData$x)
results_mod <- postResample(pred = marsPred, obs = testData$y)
results_table[3,] <- results_mod


#Present table with model results.
results_table #MARS gives the best performance.

#Does MARS select the informative predictors (those named X1-X5)?
#Variable importance scores.
varImp(marsModel)

