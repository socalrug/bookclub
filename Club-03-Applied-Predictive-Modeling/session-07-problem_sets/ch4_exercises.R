##EXERCISE 4.1: MUSIC DATA##
setwd("/Users/judithborghouts/Documents/UCI/Courses/OC-BookClub/music-dataset")
train <- read.csv( 'train.csv', header=TRUE)

#(a) What data splitting method(s) would you use for the music data?
#The response categories are not balanced. Many predictors correlated. 191 predictors, 12,495 samples.
#Samples are not independent of each other.
table(train$GENRE)
genre.freq <- table(train$GENRE)
barplot(genre.freq)
print('Relatively large sample size; 10-fold cv because computational costs not too high, stratified because
      imbalanced categories.')

#(b) Using tools described in this chapter, provide code for implementing your approach(es).
set.seed(1)
cvSplits <- createFolds(train$GENRE, k = 10, returnTrain = TRUE) #10-fold crossvalidation.

##EXERCISE 4.2: PERMEABILITY DATA##
data(permeability)

#(a) What data splitting method(s) would you use for the permeability data?
#165 unique compounds; 1,107 molecular fingerprints were determined for each.
hist(permeability)
print('Small sample size, more predictors than samples, skewed sample; stratified resampling/bootstrapping.')

#(b) Using tools described in this chapter, provide code for implementing your approach(es).
createResample(permeability, 10) %>%  map(.f = function(i) permeability[i]) -> bs

repeatedCV <- createResample(permeability, 10) #resampling, 10 partitions.

##EXERCISE 4.3: CHEMFACT DATA##
data("ChemicalManufacturingProcess")
complete.cases(ChemicalManufacturingProcess)
cmp <- ChemicalManufacturingProcess[complete.cases(ChemicalManufacturingProcess), ]
str(cmp)

train_index <- createDataPartition(pull(cmp, Yield), p = 0.8, times = 1, list = FALSE)
plsTune <- train(select(cmp, -Yield),
                 pull(cmp, Yield),
                 method = 'pls',
                 tuneLength = 59,
                 trControl = trainControl(method = "repeatedcv", repeats = 5,
                                          index = list(TrainSet = train_index)),
                 preProcess = c('center', 'scale'),
                 metric = 'Rsquared')
predict(plsTune, cmp[-train_index,])

#(a) using the one-standard error method, what number of PLS components provides the most parsimonious model?
print('4.')

#(a) Compute the tolerance values for this example. If a 10% loss in R2 is acceptable, what is the optimal nr of PLS
#components?

#(c)If goal is to select model that optimizes R2, which model(s) would you choose and why?

#(d) Given each model's prediction time, model complexity, and R2 estimates, which model(s) would you choose and why?

##EXERCISE 4.4##
data(oil)
str(oilType)
oil.freq <- table(oilType)
#(a) Create a completely random sample of 60 oils. How closely do the frequencies of the random sample match the 
#original samples? Repeat the procedure several times to understand the variation in the sampling process.
randSamp1 <- sample(oilType, 60)
rand.freq <- table(randSamp1)

#(b) Create a stratified random sample. How does this compare to the completely random samples?
set.seed(1)
trainingRows <- createDataPartition(oilType, p=.80, times = 10)
stratSamp1 <- lapply(trainingRows, function(x,y) table (y[x]), y = oilType )
head(stratSamp1, 1)
strat.freq <- head(stratSamp1, 1)

#(c) With such as a small sample size, what are the options for determining performance? Should a test set
#be used?
print('A test set would probably not be suitable for a small sample size; bootstrapping or resampling may be better.')

#(d) Try different sample sizes and accuracy rates to understand the trade-off between uncertainty in results,
#the model performance, and the test set size.
binom.test(16,20) #16 out of 20 correct
binom.test(14,20)
