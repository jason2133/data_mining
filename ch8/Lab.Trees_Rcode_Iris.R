###############################################
#                        
# Lab: Decision Trees with German Credit Data 
#
###############################################

## Calling packages

library(rpart) #install.packages("rpart")


## Data reading

head(iris)
sum(is.na(iris))


## Growing a tree

set.seed(1)
fit = rpart(Species ~., data=iris, method="class") #cp=0.01 (default), 
#NOTE: parms=list(split='gini') is the default, parms=list(split='information') is optional

fit
summary(fit)
plot(fit);  text(fit)


fit2 = rpart(Species ~., data=iris, method="class", 
             control = rpart.control(xval=10, cp=0)) #cp=0 (maximal tree)
fit2
summary(fit2)
plot(fit2);  text(fit2)


## Pruning the tree and selecting the tree with min error

tmp = printcp(fit2)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit2, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE)


## Alternatively, Pruning the tree and selecting the smallest tree within min error + 1 se)

tmp = printcp(fit2)
k = which.min(tmp[,"xerror"])
err = tmp[k,"xerror"]; se = tmp[k,"xstd"]; k = which(tmp[,"xerror"] <= err+se)[1]
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit2, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE)


## Predicting

pred = predict(fit.pruned, newdata=iris, type="prob") #prediction
i.choice = apply(pred, 1, which.max)
yhat = colnames(pred)[i.choice]
ctable = table(iris$Species, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy





###########################################
# Computing the test error by paritioning


## Data Partitioning 

set.seed(12)
V = 2
n =  NROW(iris)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
iris.train = iris[ii,]
iris.test  = iris[-ii,]


## Growing a tree

fit = rpart(y ~., data=iris.train, method="class", control = rpart.control(xval=10, cp=0))
fit
summary(fit)
plot(fit);  text(fit)


## Pruning

tmp = printcp(fit)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE, all = T)


## Predicting and Evaluating

pred = predict(fit.pruned, newdata=iris.test, type="prob") #prediction
i.choice = apply(pred, 1, which.max)
yhat = colnames(pred)[i.choice]
ctable = table(iris.test$Species, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

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
  
  ## Data Partitioning 

  iris.train = iris[id != i,] 
  iris.test  = iris[id == i,] 
  
  ## Growing

  fit = rpart(Species ~., data=iris.train, method="class", 
              control = rpart.control(xval=10, cp=0)) #maximal tree
  
  ## Pruning

  tmp = printcp(fit)
  k = which.min(tmp[,"xerror"])
  cp.tmp = tmp[k,"CP"]
  fit.pruned = prune(fit, cp=cp.tmp) #tree with min CV error
  
  ## Predicting and Evaluating
  
  pred = predict(fit.pruned, newdata=iris.test, type="prob") #prediction for test data
  i.choice = apply(pred, 1, which.max)
  yhat = colnames(pred)[i.choice]
  miss.err.test = miss.err.test + mean(iris.test$Species != yhat) 
  
}

cv.err.test = miss.err.test/V; cv.err.test # CV test error



### END
