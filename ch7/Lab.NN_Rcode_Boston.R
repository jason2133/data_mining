#####################################
#
# Regression Analysis of Boston Data
#
#####################################

 
## Calling the libraries

library(neuralnet) #install.packages("neuralnet")
library(dummy) #install.packages("dummy")


## Data reading

library(MASS)
i = which(Boston$medv == 50) 
boston = Boston[-i,] # delete cases with medv=50
boston$chas = factor(boston$chas)
boston$rad = factor(boston$rad)


## Data handling

dvar = c(4,9) #find nominal variables
boston2 = dummy(x=boston[,dvar]) # transform nominal variables into dummy variables
boston2 = boston2[,-c(2,11)] # delete redundant dummy variables
boston2 = cbind(boston[,-dvar], boston2) # combine them
for(i in 1: ncol(boston2)) if(!is.numeric(boston2[,i])) boston2[,i] = as.numeric(boston2[,i])


## Scaling

max1 = apply(boston2, 2, max) 
min1 = apply(boston2, 2, min)
sdat = scale(boston2, center = min1, scale = max1 - min1)
sdat = as.data.frame(sdat)


## Model fitting

vname = names(sdat.train)
f = as.formula(paste("medv ~", paste(vname[!vname %in% "medv"], collapse = " + ")))
fit.nn = neuralnet(f, data=sdat, hidden=c(3,1), linear.output=T) # hidden (3,1)
plot(fit.nn)


## Predicting

pred.nn = predict(fit.nn, sdat) 
pred.nn = pred.nn*(max1[12]-min1[12])+min1[12] #12=position of target variable medv
 
plot(boston2$medv, pred.nn, xlim=c(0,50), ylim=c(0,50), xlab="Observed Values", ylab="Fitted Values")
abline(a=0, b=1)


## Evaluating

mean((boston2$medv - pred.nn)^2)  # MSE 
mean(abs(boston2$medv - pred.nn)) # MAE 



###########################################
# Computing the test error by paritioning


## Data partitioning 

set.seed(1234)
train.index = sample(1:nrow(boston2), round(0.7*nrow(boston2)))
boston2.train = boston2[ train.index,] #train data                              
boston2.test  = boston2[-train.index,] #test data


## Scaling

max1 = apply(boston2.train, 2, max) 
min1 = apply(boston2.train, 2, min)
sdat.train = scale(boston2.train, center = min1, scale = max1 - min1)
sdat.train = as.data.frame(sdat.train)
sdat.test = scale(boston2.test, center = min1, scale = max1 - min1)
sdat.test = as.data.frame(sdat.test)


## Model fitting

vname = names(sdat.train)
f = as.formula(paste("medv ~", paste(vname[!vname %in% "medv"], collapse = " + ")))
fit.nn = neuralnet(f, data=sdat.train, hidden=c(3,1), linear.output=T) #hidden (3,1) 
plot(fit.nn)


## Predicting

pred.nn = predict(fit.nn, sdat.test) 
pred.nn = pred.nn*(max1[12]-min1[12])+min1[12] #12=position of target variable medv
 

## Evaluating

mean((boston2.test$medv - pred.nn)^2)  # MSE 
mean(abs(boston2.test$medv - pred.nn)) # MAE 



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
  boston2.train = boston2[-test.index,] #train data                              
  boston2.test  = boston2[ test.index,] #test data

  ## Scaling

  max1 = apply(boston2.train, 2, max) 
  min1 = apply(boston2.train, 2, min)
  sdat.train = scale(boston2.train, center = min1, scale = max1 - min1)
  sdat.train = as.data.frame(sdat.train)
  sdat.test = scale(boston2.test, center = min1, scale = max1 - min1)
  sdat.test = as.data.frame(sdat.test)

  ## Model fitting

  vname = names(sdat.train)
  f = as.formula(paste("medv ~", paste(vname[!vname %in% "medv"], collapse = " + ")))
  fit.nn = neuralnet(f, data=sdat.train, hidden=c(3,1), linear.output=T) #hidden (3,1) 
  #plot(fit.nn)

  ## Predicting and Evaluating

  pred.nn = predict(fit.nn, sdat.test) 
  pred.nn = pred.nn*(max1[12]-min1[12])+min1[12] #12=position of target variable medv
  mse.test = mse.test + mean((boston2.test$medv - pred.nn)^2)  # MSE 
  mae.test = mae.test + mean(abs(boston2.test$medv - pred.nn)) # MAE 

}


cv.mse.test  = mse.test/V; cv.mse.test   # CV test MSE
cv.mae.test  = mae.test/V; cv.mae.test   # CV test MAE


#END

