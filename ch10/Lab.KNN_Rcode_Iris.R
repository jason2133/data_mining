##################################################
#
# Lab: KNN classification with Iris Data            
#
##################################################

## Calling packages
library(class) #install.packages("class")


## Data reading

head(iris)
sum(is.na(iris))

#iris = na.omit(iris) #can remove the cases with missing values if any


## Data explorating

summary(iris)
str(iris)
dim(iris)
head(iris)
barplot(table(iris$Species), col = "blue", xlab = "Species", ylab = "Frequency")


## Standardizing

X = scale(iris[,-5]) #5=response
y = iris[,5]


## KNN fitting with K=5

fit = knn(train=X, test=X, cl=y, k=5) 


## Predicting and Evaluating

yhat=fit
ctable = table(y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

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
X.train = X[ii,]; X.test  = X[-ii,]
y.train = y[ii];  y.test  = y[-ii]


## KNN fitting with K=5

fit = knn(train=X.train, test=X.test, cl=y.train, k=5) 


## Predicting and Evaluating

yhat=fit
ctable = table(y.test, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy




##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0
cutoff = 0.5

set.seed(12345)
id = sample(1:V, nrow(iris), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning 

  X.train = X[id != i,]; X.test  = X[id == i,] 
  y.train = y[id != i];  y.test  = y[id == i]  

  ## Fitting

  fit = knn(train=X.train, test=X.test, cl=y.train, k=5) 

  ## Predicting and Evaluating
  yhat=fit
  miss.err.test = miss.err.test + mean(y.test != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
