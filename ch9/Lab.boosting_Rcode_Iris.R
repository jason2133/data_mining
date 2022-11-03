###############################################
#
# Lab: Boosting with Iris Data 
#
###############################################


## Calling packages

library(rpart) #install.packages("rpart")
library(adabag) #install.packages("adabag")

help(boosting)


## Data reading

head(iris)
sum(is.na(iris))


## Boosting

set.seed(1234)
my.control = rpart.control(xval=0, cp=0, maxdepth=4)
fit = boosting(Species~., data=iris, boos=T, mfinal=50, control=my.control)
fit$trees


## Predicting

pred = predict.boosting(fit, newdata=iris)
yhat = pred$class
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


## Boosting

set.seed(1234)
my.control = rpart.control(xval=0, cp=0, maxdepth=4)
fit = boosting(Species~., data=iris.train, boos=T, mfinal=50, control=my.control)
fit$trees


## Predicting and Evaluating

pred = predict.boosting(fit, newdata=iris.test)
yhat = pred$class
ctable = table(iris.test$Species, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy



##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.test = 0

set.seed(12345)
id = sample(1:V, nrow(iris), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning

  iris.train = iris[id != i,] 
  iris.test = iris[id == i,] 
  
  ## Boosting

  my.control = rpart.control(xval=0, cp=0, maxdepth=1)
  fit = boosting(Species~., data=iris.train, boos=T, mfinal=50, control=my.control)

  ## Predicting and Evaluating
  
  pred = predict.boosting(fit, newdata=iris.test)
  yhat = pred$class
  miss.err.test = miss.err.test + mean(iris.test$Species != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
