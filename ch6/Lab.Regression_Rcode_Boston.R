################################################
#
# Lab: Linear regression Model with Boston Data
#
################################################


## Data reading

library(MASS) 
i = which(Boston$medv == 50) 
boston = Boston[-i,] # delete cases with medv=50
boston$chas = factor(boston$chas)
boston$rad = factor(boston$rad)


## Model fitting

fit.all = lm(medv ~ ., data = boston) # fit a linear model with all variables
fit.step = step(fit.all, direction="both") # stepwise variable selection
fit.step$anova
summary(fit.step) # print the fitted model


## Predicting

yhat = predict(fit.step, newdata=boston, type="response") # predictions
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


## Model Fitting

fit.reg = lm(medv ~ ., data = boston.train)
fit.step.reg = step(fit.reg, direction="both", trace=FALSE) #Stepwise variable selection


## Predicting and Evaluating

yhat.reg = predict(fit.step.reg, newdata=boston.train, type="response")
mean((boston.train$medv - yhat.reg)^2)  # train MSE
mean(abs(boston.train$medv - yhat.reg)) # train MAE

yhat.reg = predict(fit.step.reg, newdata=boston.test, type="response")
mean((boston.test$medv - yhat.reg)^2)  # test MSE
mean(abs(boston.test$medv - yhat.reg)) # test MAE




##########################
# Computing the CV error


V = 10 #V-fold CV
mse.train = 0; mse.test = 0
mae.train = 0; mae.test = 0

set.seed(1234)
id = sample(1:V, nrow(boston), replace = T)

for(i in 1:V) {
  
  print(i)

  ## Data partitioning

  test.index = which(id==i)
  boston.train = boston[-test.index,] #train data                              
  boston.test  = boston[ test.index,] #test data

  ## Fitting

  fit.reg = lm(medv ~ ., data = boston.train)
  fit.step.reg = step(fit.reg, direction="both", trace=FALSE) #Stepwise variable selection

  ## Predicting and Evaluating

  yhat.reg = predict(fit.step.reg, newdata=boston.test, type="response")
  mse.test = mse.test + mean((boston.test$medv - yhat.reg)^2)  # MSE
  mae.test = mae.test + mean(abs(boston.test$medv - yhat.reg)) # MAE

}

cv.mse.test  = mse.test/V;  cv.mse.test  # test CV MSE
cv.mae.test  = mae.test/V;  cv.mae.test  # test CV MAE 


#END
