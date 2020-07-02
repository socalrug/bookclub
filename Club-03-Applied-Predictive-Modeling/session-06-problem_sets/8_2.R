library(randomForest)
library(caret)
library(party)

set.seed(10)
v1 = sample(0:10000/10000, 200, replace = T)
print(length(unique(v1)))
v2 = sample(0:1000/1000, 200, replace = T)
print(length(unique(v2)))
v3 = sample(0:100/100, 200, replace = T)
print(length(unique(v3)))
v4 = sample(0:10/10, 200, replace = T)
print(length(unique(v4)))

#y = v1+ 2*v4 + rnorm(200)
y = v1 + v4 + 0.5*rnorm(200)

df = data.frame(v1,v2,v3,v4,y)

model1 <- randomForest(y ~., data = df, importance = TRUE, ntree = 500)
rfImp1 <- varImp(model1, scale = FALSE)
print(rfImp1)

library(rpart)
model2 <- rpart(y ~. , data = df)
rfImp2 <- varImp(model2)



