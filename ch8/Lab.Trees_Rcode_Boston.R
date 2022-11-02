#####################################
#
# Regression Analysis of Boston Data
#
#####################################

## Calling packages

library(rpart) #install.packages("rpart")


## Data reading

library(MASS) 
i = which(Boston$medv == 50) 
boston = Boston[-i,] # delete cases with medv=50
boston$chas = factor(boston$chas)
boston$rad = factor(boston$rad)


## Growing a tree
set.seed(1)
fit = rpart(medv ~., data=boston, method="anova") #cp=0.01 (default), 

fit
summary(fit)
plot(fit);  text(fit)


fit2 = rpart(medv ~., data=boston, method="anova", 
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

yhat = predict(fit.pruned, newdata=boston, type="vector") # predictions
head(yhat)

plot(boston$medv, yhat, xlim=c(0,50), ylim=c(0,50), xlab="Observed Values", ylab="Fitted Values")
abline(a=0, b=1)


## Evaluating

mean((boston$medv - yhat)^2)  # MSE
mean(abs(boston$medv - yhat)) # MAE


###########################################
# Computing the test error by paritioning


## Data Partitioning 

set.seed(1234)
train.index = sample(1:nrow(boston), round(0.7*nrow(boston)))
boston.train = boston[ train.index,] #train data                              
boston.test  = boston[-train.index,] #test data


## Growing a tree

fit2 = rpart(medv ~., data=boston.train, method="anova", 
             control = rpart.control(xval=10, cp=0)) #cp=0 (maximal tree)

## Pruning

tmp = printcp(fit2)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit2, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE)


## Predicting and Evaluating

yhat.tree = predict(fit.pruned, newdata=boston.train, type="vector")
mean((boston.train$medv - yhat.tree)^2)  # train MSE
mean(abs(boston.train$medv - yhat.tree)) # train MAE

yhat.tree = predict(fit.pruned, newdata=boston.test, type="vector")
mean((boston.test$medv - yhat.tree)^2)  # test MSE
mean(abs(boston.test$medv - yhat.tree)) # test MAE




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
  boston.train = boston[-test.index,] #train data                              
  boston.test  = boston[ test.index,] #test data

  ## Growing

  fit2 = rpart(medv ~., data=boston.train, method="anova", 
               control = rpart.control(xval=10, cp=0)) #cp=0 (maximal tree)

  ## Pruning

  tmp = printcp(fit2)
  k = which.min(tmp[,"xerror"])
  cp.tmp = tmp[k,"CP"]
  fit.pruned = prune(fit2, cp=cp.tmp)
  #plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE)

  ## Predicting and Evaluating

  yhat.tree = predict(fit.pruned, newdata=boston.test, type="vector")
  mse.test = mse.test + mean((boston.test$medv - yhat.tree)^2)  # MSE
  mae.test = mae.test + mean(abs(boston.test$medv - yhat.tree)) # MAE
}


cv.mse.test  = mse.test/V;  cv.mse.test  # test CV MSE
cv.mae.test  = mae.test/V;  cv.mae.test  # test CV MAE 


#END
