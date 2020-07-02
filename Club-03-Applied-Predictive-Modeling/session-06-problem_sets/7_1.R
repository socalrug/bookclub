library(kernlab)
library(purrr)

set.seed(100)
x <- runif(100, min = 2, max = 10)
y <- sin(x) + rnorm(length(x)) * 0.25
sinData <- data.frame(x = x, y = y)
plot(x, y)
dataGrid <- data.frame(x = seq(2, 10, length = 100))

# (a)

purrr::walk(seq(-4, 0), function(index, x, y) {
  epsilon <- 2 ** index
  rbfSVM <- ksvm(x = x, y = y, data = sinData,
                 kernel = 'rbfdot', kpar = 'automatic',
                 C = 1, epsilon = epsilon)
  modelPrediction <- predict(rbfSVM, newdata = dataGrid)
  colour = c('red', 'green', 'blue', 'orange', 'black')
  points(x = dataGrid$x, y = modelPrediction[,1], type = 'l',
         col =  palette()[index + 5])

}, x=x, y=y)


# (b)

purrr::walk(seq(-4, 2), function(index, x, y) {
  sigma <- 2 ** index
  rbfSVM <- ksvm(x = x, y = y, data = sinData,
                 kernel = 'rbfdot', kpar = list(sigma = sigma),
                 C = 1, epsilon = 0.1)
  modelPrediction <- predict(rbfSVM, newdata = dataGrid)
  colour = c('red', 'green', 'blue', 'orange', 'black')
  points(x = dataGrid$x, y = modelPrediction[,1], type = 'l',
         col =  palette()[index + 5])

}, x=x, y=y)

print(palette())