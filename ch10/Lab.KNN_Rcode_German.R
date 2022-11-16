##################################################
#
# Lab: KNN classification with German Credit Data            
#
##################################################

## Calling packages for classification knn

library(class) #install.packages("class")


## Data Reading

german = read.table("germandata.txt",header=T) 
german$y = ifelse(german$y=="good", 1, 0)
german$y = factor(german$y)

sum(is.na(german))

#german = na.omit(german) #can remove the cases with missing values if any


## Data explorating

summary(german)
str(german)
dim(german)
head(german)
barplot(table(german$y), col = "blue", xlab = "Credit", ylab = "Frequency")



## Selecting and standardizing numerical variables 

y = german[,21]
X = scale(german[,c(2,5,8,11,13,16,18)])
head(germanX)


## KNN with K=5

fit = knn(train=X, test=X, cl=y, k=5) 


## Predicting

yhat=fit
ctable = table(y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity



###########################################
# Computing the test error by paritioning


## Data partitioning

set.seed(123)
V = 2
n =  NROW(german)
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
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity




##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0
cutoff = 0.5

set.seed(12345)
id = sample(1:V, nrow(german), replace = T)

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
