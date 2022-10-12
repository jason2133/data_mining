################################################
# Lab: Neural Networks with Iris Data #
################################################


## Calling the libraries

library(neuralnet) #install.packages("neuralnet")
library(dummy) #install.packages("dummy")


## Data reading and handling

library(MASS)
iris2 = iris[,c(1:4)]  #when taking only numerical variables
y = iris[,5]           #5=target variable
y.class = c("setosa","versicolor","virginica") 


## Scaling

max1 = apply(iris2, 2, max) 
min1 = apply(iris2, 2, min)

gdat = scale(iris2, center = min1, scale = max1 - min1) #Standaization
gdat = data.frame(gdat, y)
head(gdat)


## Model fitting

iris2 = gdat
gn = names(iris2)
f = as.formula(paste("y ~", paste(gn[!gn %in% "y"], collapse = " + "))) #y: target variable
fit.nn = neuralnet(f, data = iris2, hidden=c(3,1), linear.output=F) 
#fit neural networks with 1 hidden layer consisting of 3 hidden neurons.

plot(fit.nn)


## Predicting

p.nn = predict(fit.nn, iris2)
yhat.nn = apply(p.nn, 1, which.max)
yhat.nn = y.class[yhat.nn]

ctable = table(iris2$y, yhat.nn, dnn=c("Actual","Predicted"))
print(ctable) # classification table  


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
n =  NROW(gdat)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
iris2.train = gdat[ii,]
iris2.test  = gdat[-ii,]


## Model fitting

gn = names(iris2.train)
f = as.formula(paste("y ~", paste(gn[!gn %in% "y"], collapse = " + ")))
fit.nn = neuralnet(f, data = iris2.train, hidden=c(3,3), linear.output=F) #3 hidden neurons

plot(fit.nn)


## Predicting

p.test.nn = predict(fit.nn, iris2.test)
yhat.test.nn = apply(p.test.nn, 1, which.max)
yhat.test.nn = y.class[yhat.test.nn]

ctable = table(iris2.test$y, yhat.test.nn, dnn=c("Actual","Predicted"))
print(ctable) # classification table  


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  



##########################
# Computing the CV errors

V = 5 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(iris), replace = T)

for(i in 1:V) {
  
  print(i)

  ## Data partitioning 

  iris2.train = gdat[id != i,] 
  iris2.test  = gdat[id == i,] 
 
  ## Model fitting
 
  gn = names(iris2.train)
  f = as.formula(paste("y ~", paste(gn[!gn %in% "y"], collapse = " + ")))
  fit.nn = neuralnet(f, data = iris2.train, hidden=1, linear.output=F) #1 hidden neuron

  ## Predicting and Evaluating

  p.test.nn = predict(fit.nn, iris2.test)
  yhat.test.nn = apply(p.test.nn, 1, which.max)
  yhat.test.nn = y.class[yhat.test.nn]
  miss.err.test = miss.err.test + mean(iris2.test$y != yhat.test.nn)
  
}

cv.err.test = miss.err.test/ V; cv.err.test # CV test error



### END
