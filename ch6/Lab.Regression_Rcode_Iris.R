##################################################
#
# Lab: Logistic regression with Iris Data            
#
##################################################

## Calling packages

library("VGAM") #install.packages("VGAM")


## Data reading

head(iris)
sum(is.na(iris))

#iris = na.omit(iris) #can remove the cases with missing values if any


## Model fitting

fit = vglm(Species ~., data = iris, family = multinomial)
summary(fit)


## Predicting

fit.pred = predict(fit, newdata = iris, type = "response") 
i.choice = apply(fit.pred, 1, which.max)
fit.yhat = colnames(fit.pred)[i.choice]

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

fit = vglm(Species ~., data = iris.train, family = multinomial)
summary(fit)


## Predicting

fit.pred = predict(fit, newdata = iris.test, type = "response") 
i.choice = apply(fit.pred, 1, which.max)
fit.yhat = colnames(fit.pred)[i.choice]

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
miss.err.train = 0
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(iris), replace = T)

for(i in 1:V) {
  
  print(i)

  ## Data partitioning

  iris.train = iris[id != i,] 
  iris.test  = iris[id == i,] 
  
  ## Fitting

  fit = vglm(Species ~., data = iris.train, family = multinomial)
  
  ## Predicting and Evaluating

  pred.test = predict(fit, newdata = iris.test, type = "response") 
  i.choice = apply(pred.test, 1, which.max)
  yhat.test = colnames(pred.test)[i.choice]
  miss.err.test = miss.err.test + mean(iris.test$Species != yhat.test) 
  
}

cv.err.test = miss.err.test/V; cv.err.test # CV test error


#END

