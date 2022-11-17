##################################################
#
# Lab: SVM with Iris Data            
#
##################################################

## Calling packages

library(e1071) #install.packages("e1071")


## Data reading

head(iris)
sum(is.na(iris))

#iris = na.omit(iris) #can remove the cases with missing values if any


## Model fitting

fit = svm(Species ~., data = iris, kernel="linear")
summary(fit)


## Predicting

fit.yhat = predict(fit, newdata = iris) 
ctable = table(iris$Species, fit.yhat,  dnn = c("Actual", "Predicted"))  
ctable
 

## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  


###########################################
# Computing the test error by paritioning


## Data partitioning

set.seed(123)
V = 2
n =  NROW(iris)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
iris.train = iris[ ii,]
iris.test  = iris[-ii,]


## Model fitting

fit = svm(Species ~., data = iris.train, kernel="linear")
summary(fit)


## Predicting

fit.yhat = predict(fit, newdata = iris.test) 
ctable = table(iris.test$Species, fit.yhat,  dnn = c("Actual", "Predicted"))  
ctable


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  


##########################
# Computing the CV error

V = 10 #V-fold CV
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(iris), replace = T)

for(i in 1:V) {
  
  print(i)

  ## Data partitioning

  iris.train = iris[id != i,] 
  iris.test  = iris[id == i,] 
  
  ## Fitting

  fit = svm(Species ~., data = iris.train, kernel="linear")

  ## Predicting and Evaluating

  yhat.test = predict(fit, newdata = iris.test) 
  miss.err.test = miss.err.test + mean(iris.test$Species != yhat.test) 
  
}

cv.err.test = miss.err.test/V; cv.err.test # CV test error


#END

