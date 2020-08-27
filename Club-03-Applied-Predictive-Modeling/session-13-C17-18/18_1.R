library(mlbench)
library(MASS)
library(caret)
library(kernlab)
library(AppliedPredictiveModeling)
library(pROC)
library(corrgram)
library(corrr)
library(C50)
library(CORElearn)
library(GoodmanKruskal)

#############################
##EXERCISE 18.1: CHURN DATA##
#############################
library(modeldata)
data(mlc_churn) #Dataset with 19 predictors, 5000 samples.

#Split dataset into a training and test set.
trainIndex <- createDataPartition(mlc_churn$churn, p = .8,
                                  list = FALSE,
                                  times = 1)
churnTrain <- mlc_churn[ trainIndex,]
churnTest <- mlc_churn[-trainIndex,]

predictors <- churnTrain[,-20]
outcome <- churnTrain[,20]

#(a) Calculate the correlations between predictors. Are there strong relationships
#between these variables? How does this compare to what one would
#expect with these attributes?

# Find out which predictors are numeric and categorical
#lapply(predictors, class)
class_pred <- sapply(predictors,class)

#Get the indices of categorical predictors
index_factor_pred = c()
for( i in 1:19 ){
  if( class_pred[i]=="factor" ){
    index_factor_pred = c( index_factor_pred, i )
  }
}

#Categorical predictors
print( colnames( predictors )[index_factor_pred] )

#Numeric predictors
print( colnames( predictors )[-index_factor_pred] )

cor(predictors[index_factor_pred], method = "kendall")
#res2 <- cor.test(predictors$state, predictors$area_code,  method="kendall")

#tbl = matrix(data=predictors[index_factor_pred], nrow=4, ncol=4, byrow=T)
#dimnames(tbl) = list(pred1=colnames( predictors )[index_factor_pred], pred2=colnames( predictors )[index_factor_pred])
#chi2 = chisq.test(tbl, correct=F)
#c(chi2$statistic, chi2$p.value)

#Correlation categorical predictors
#Chi square to check for association; not sure if appropriate for >2 predictors.
chisq.test(predictors$state, predictors$area_code, correct = FALSE)

#Goodman’s Kruskal Tau test to check for effect size (strength of association).
fact_names <- colnames( predictors )[index_factor_pred]
corr_subset <- subset(predictors, select = fact_names)
GKmatrix1<- GKtauDataframe(corr_subset)
plot(GKmatrix1, corrColors = "blue")

#Correlation numeric predictors
cor(predictors[-index_factor_pred])
corrgram(predictors[-index_factor_pred])
#cor(predictors[sapply(predictors, is.numeric)])

#(b) Assess the importance of the categorical predictors (i.e., area code, voice
#mail plan, etc) individually using the training set.
rocValues = filterVarImp( x=churnTrain[,index_factor_pred], y=churnTrain$churn )
rocValues

#(c) Also estimate the importance scores of the continuous predictors individually.
rocValues <- filterVarImp( x=churnTrain[,-index_factor_pred], y=churnTrain$churn )
#head(rocValues)
rocValues

# Sort by variable importance
var_order = order( rocValues$yes, decreasing=TRUE )
VI = as.matrix( rocValues[var_order,] )
print( VI[,"yes"] )

#(d) Now use ReliefF to jointly estimate the importance of the predictors. Is
#there a difference in rankings? Why or why not?
reliefValues <- attrEval(churn ~ ., data = churnTrain,
                             estimator = "ReliefFequalK",
                             ReliefIterations = 50)
head(reliefValues)
reliefValues
print( sort( reliefValues, decreasing=TRUE ) )

