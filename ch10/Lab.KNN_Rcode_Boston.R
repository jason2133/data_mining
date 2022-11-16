#######################################
#
# Lab: KNN regression with Boston Data
#
#######################################

## Calling packages for knnreg

library(caret) #install.packages("caret")


## Data reading

library(MASS) 
i = which(Boston$medv == 50) 
boston = Boston[-i,] # delete cases with medv=50
boston$chas = factor(boston$chas)
boston$rad = factor(boston$rad)


## Selecting and standardizing numerical variables 

X = scale(boston[,-c(4,9,14)]) #4,9 =categorical, 14=response
y = boston[,14]


## Model fitting with K=5

fit = knnreg(X,y, k=5)
str(fit)


## Predicting

yhat = predict(fit, newdata=X) # predictions
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

y.train = boston.train[,14] # 14=response
x.train = scale(boston.train[,-c(4,9,14)]) #4,9 =categorical, 14=response
x.test = scale(boston.test[,-c(4,9,14)]) #4,9 =categorical, 14=response
fit = knnreg(x=x.train, y=y.train, k=5)
str(fit)


## Predicting and Evaluating

yhat = predict(fit, newdata=x.train)
mean((boston.train$medv - yhat)^2)  # train MSE
mean(abs(boston.train$medv - yhat)) # train MAE

yhat = predict(fit, newdata=x.test)
mean((boston.test$medv - yhat)^2)  # test MSE
mean(abs(boston.test$medv - yhat)) # test MAE




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

  y.train = boston.train[,14] # 14=response
  x.train = scale(boston.train[,-c(4,9,14)]) #4,9 =categorical, 14=response
  x.test = scale(boston.test[,-c(4,9,14)]) #4,9 =categorical, 14=response
  fit = knnreg(x=x.train, y=y.train, k=5)
  #str(fit)

  ## Predicting and Evaluating

  yhat = predict(fit, newdata=x.test)
  mse.test = mse.test + mean((boston.test$medv - yhat)^2)  # MSE
  mae.test = mae.test + mean(abs(boston.test$medv - yhat)) # MAE
}


cv.mse.test  = mse.test/V;  cv.mse.test  # test CV MSE
cv.mae.test  = mae.test/V;  cv.mae.test  # test CV MAE 


#END
