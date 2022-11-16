##############################
#
# Lab: LDA/QDA with Iris Data        
#
##############################


## Calling packages for lda and qda

library(MASS) #install.packages("MASS")


## LDA/QDA

fit = lda(Species ~., data=iris) #LDA
plot(fit)
#fit = qda(Species ~., data=iris) #QDA



## Predicting

pred = predict(fit, newdata=iris)$posterior #probability
yhat = predict(fit, newdata=iris)$class     #class
ctable = table(iris$Species, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy



###########################################
# Computing the test error by paritioning


## Data partitioning

set.seed(123)
V = 2
n =  NROW(iris)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
iris.train = iris[ii,]
iris.test  = iris[-ii,]


## LDA/QDA

fit = lda(Species ~., data=iris.train) #LDA
#fit = qda(Species ~., data=iris.train) #QDA
plot(fit)


## Predicting

pred = predict(fit, newdata=iris.test)$posterior
yhat = predict(fit, newdata=iris.test)$class
ctable = table(iris.test$Species, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy



##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(123)
id = sample(1:V, nrow(iris), replace = T)

for(i in 1:V) {
  
  print(i)

  ## Data partitioning
  iris.train = iris[id != i,] 
  iris.test  = iris[id == i,] 
  
  ## LDA/QDA
  fit = lda(Species ~., data=iris.train)
  #fit = qda(Species ~., data=iris.train)
 
  ## Predicting and Evaluating
  pred = predict(fit, newdata=iris.test)$posterior
  yhat = predict(fit, newdata=iris.test)$class
  miss.err.test = miss.err.test + mean(iris.test$Species != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
