###############################################
#
# Lab: Random Forests with Iris Data 
#
###############################################


## Calling packages

library(randomForest) #install.packages("randomForest")

help(randomForest)


## Foresting

set.seed(1234)
fit = randomForest(Species~., data=iris, ntree=100, mtry=1, importance=T)

plot(fit, type="l")
importance(fit)


## Predicting

yhat = predict(fit, newdata=iris, type="class")
ctable = table(iris$Species, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy



###########################################
# Computing the test error by paritioning


## Data Partitioning

set.seed(123)
V = 2
n =  NROW(iris)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
iris.train = iris[ii,]
iris.test  = iris[-ii,]


## Foresting

set.seed(1234)
fit = randomForest(Species~., data=iris.train, ntree=100, mtry=1, importance=T)

plot(fit, type="l")
importance(fit)


## Predicting and Evaluating

yhat = predict(fit, newdata=iris.test, type="class")
ctable = table(iris.test$Species, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy



##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(12345)
id = sample(1:V, nrow(iris), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning

  iris.train = iris[id != i,] 
  iris.test  = iris[id == i,] 
  
  ## Foresting

  fit = randomForest(Species~., data=iris.train, ntree=100, mtry=1, importance=T)

  ## Predicting and Evaluating

  yhat = predict(fit, newdata=iris.test, type="class")
  miss.err.test = miss.err.test + mean(iris.test$Species != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
