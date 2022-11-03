#######################################
#
# Lab: Random Forests with Boston Data
#
#######################################


## Calling packages

library(randomForest) #install.packages("randomForest")

help(randomForest)


## Data reading 

library(MASS) 
i = which(Boston$medv == 50) 
boston = Boston[-i,] # delete cases with medv=50
boston$chas = factor(boston$chas)
boston$rad = factor(boston$rad)


## Foresting

set.seed(1234)
fit = randomForest(medv~., data=boston, ntree=100, mtry=5, importance=T, , na.action=na.omit)

plot(fit, type="l")
importance(fit)
varImpPlot(fit, type=1)


## Predicting

yhat = predict(fit, newdata=boston, type="response") # predictions
head(yhat)

plot(boston$medv, yhat, xlim=c(0,50), ylim=c(0,50), xlab="Observed Values", ylab="Fitted Values")
abline(a=0, b=1)


## Evaluating

mean((boston$medv - yhat)^2)  # MSE
mean(abs(boston$medv - yhat)) # MAE



###########################################
# Computing the test error by paritioning


## Data Partitioning

set.seed(1234)
train.index = sample(1:nrow(boston), round(0.7*nrow(boston)))
boston.train = boston[ train.index,] #train data                              
boston.test  = boston[-train.index,] #test data

## Foresting

set.seed(1234)
fit = randomForest(medv~., data=boston.train, ntree=100, mtry=5, importance=T, , na.action=na.omit)

plot(fit, type="l")
importance(fit)
varImpPlot(fit, type=1)

## Predicting and Evaluating

yhat = predict(fit, newdata=boston.train, type="response")
mean((boston.train$medv - yhat)^2)  # train MSE
mean(abs(boston.train$medv - yhat)) # train MAE

yhat = predict(fit, newdata=boston.test, type="response")
mean((boston.test$medv - yhat)^2)  # test MSE
mean(abs(boston.test$medv - yhat)) # test MAE




##########################
# Computing the CV error


V = 10 #V-fold CV
mse.test = 0
mae.test = 0

set.seed(1234)
id = sample(1:V, nrow(boston), replace = T)

for(i in 1:V) {
  
  print(i)

  ## Data Partitioning

  test.index = which(id==i)
  boston.train = boston[-test.index,] #train data                              
  boston.test  = boston[ test.index,] #test data

  ## Foresting

  fit = randomForest(medv~., data=boston.train, ntree=100, mtry=5, importance=T, , na.action=na.omit)

  ## Predicting and Evaluating

  yhat = predict(fit, newdata=boston.test, type="response")
  mse.test = mse.test + mean((boston.test$medv - yhat)^2)  # MSE
  mae.test = mae.test + mean(abs(boston.test$medv - yhat)) # MAE
}

cv.mse.test  = mse.test/V;  cv.mse.test  # test CV MSE
cv.mae.test  = mae.test/V;  cv.mae.test  # test CV MAE 

#END
